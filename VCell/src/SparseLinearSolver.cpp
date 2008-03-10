/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
// Dec 16 2001
// Handles mixed boundary cases in 2D and 3D - Diana Resasco
// Additional changes April 24, 2003 - Diana Resasco

// Jan 15 2002 9:45PM
// Solves for subregions - Diana Resasco
 

// Dec 2002
// Handles symmetric or general (non-symmetric) storage - Diana Resasco

#include <VCELL/SimTypes.h>
#include <VCELL/SparseMatrixPCG.h>
#include <VCELL/SparseMatrixEqnBuilder.h>
#include <VCELL/Variable.h>
#include <VCELL/EqnBuilder.h>
#include <VCELL/Simulation.h>
#include <VCELL/SimTool.h>
#include <VCELL/Mesh.h>
#include <VCELL/Solver.h>
#include <VCELL/VarContext.h>
#include <VCELL/SparseLinearSolver.h>
#include <VCELL/FVUtils.h>
#include <iomanip>
using namespace std;

//#define SHOW_MATRIX
//#define SHOW_IPARM

//------------------------------------------------------------------------
// class SparseLinearSolver
//------------------------------------------------------------------------
SparseLinearSolver::SparseLinearSolver(Variable *Var,  SparseMatrixEqnBuilder * arg_eqnbuilder,  bool  AbTimeDependent)
: PDESolver(Var, AbTimeDependent)
{
	enableRetry = true;
	eqnBuilder = arg_eqnbuilder;
	pcg_workspace = NULL;

	smEqnBuilder = arg_eqnbuilder;
	long size = smEqnBuilder->getSize();
	Mesh* mesh = smEqnBuilder->getMesh();
	switch (mesh->getDimension()) {
		case 1:
		case 2:	
			if (smEqnBuilder->getSymmetricFlag() == MATRIX_SYMMETRIC) {
				nWork = size * 9;
			}else {
				nWork = size * 30;
			}	
			break;
		case 3:	
			if (smEqnBuilder->getSymmetricFlag() == MATRIX_SYMMETRIC) {
				nWork = (long)(size * 11.8 + 2208);
			}else {
				nWork = size * 40;
			}	
			break;
	}		
	initPCGWorkspace(0);
}

void SparseLinearSolver::initPCGWorkspace(long additional) {
	nWork += additional;
	delete[] pcg_workspace;
	try {
		pcg_workspace = new double[nWork];
	} catch (...) {
		throw "Out of Memory";
	}
	memset(pcg_workspace, 0, nWork * sizeof(double));
}

SparseLinearSolver::~SparseLinearSolver()
{
	delete[] pcg_workspace;	
}

bool SparseLinearSolver::solveEqn(double dT_sec, 
				 int volumeIndexStart, int volumeIndexSize, 
				 int membraneIndexStart, int membraneIndexSize, bool bFirstTime)
{
	int* IParm = PCGSolve(bFirstTime);
	if (IParm[50] != 0) {
		switch (IParm[50]) {
			case 2:
			case 3:
			case 4:
			case 9:
			case 10:
			case 15:
				if (enableRetry) {
					enableRetry = false;						
					cout << endl << "!!Note: Insufficient PCG workspace (need additional " << IParm[53] << "), for variable " << var->getName() << ", retry again" << endl;
					initPCGWorkspace(IParm[53]);
					delete[] IParm;
					return solveEqn(dT_sec, volumeIndexStart, volumeIndexSize, membraneIndexStart, membraneIndexSize, true);
				} else {
					handlePCGExceptions(IParm[50], IParm[53]);
					delete[] IParm;
					return false;
				}
				break;
			default:				
				printf("SparseLinearSolver::PCGSolve, error solving system\n");
				handlePCGExceptions(IParm[50], IParm[53]);
				delete[] IParm;
				return false;
		}
	}
	delete[] IParm;
	return true;
}

// --------------------------------------------------
int* SparseLinearSolver::PCGSolve(bool bRecomputeIncompleteFactorization)
// --------------------------------------------------
{
	//  prepare data to call pcgpak
	//	
	SparseMatrixPCG *A = smEqnBuilder->getA();
	double* sa = A->getsa();
	double *pRHS = smEqnBuilder->getB();
	INT32 *ija = A->getFortranIJA(); //before was long
	long Nrsp = nWork;
	double *pNew = smEqnBuilder->getX();
	int symmetricflg = A->getSymmetricFlag();  // general or symmetric storage format

	// set number of unknowns (size of linear system to solve)
	long size = smEqnBuilder->getSize();

	string timername = var->getName() + " PCG";
	TimerHandle tHndPCG = SimTool::getInstance()->getTimerHandle(timername);

	int* IParm = new int[75];
	memset(IParm, 0, 75 * sizeof(int));

	double RParm[25];
	memset(RParm, 0, 25 * sizeof(double));

	// bRecomputeIncompleteFactorization being true means it is first time or it retries upon memory failure
	if (bRecomputeIncompleteFactorization || isTimeDependent()) {
		IParm[13] = 0; // don't reuse anything. used to be 2 for time dependent problems to reuse symbolic incomplete factorization but have memory problem
	} else {
		IParm[13] = 1; // reuse all incomplete factorization.
	}

	// SET ALPHA and OMEGA
	if (eqnBuilder->isElliptic()) {		
		memset(pNew, 0, size * sizeof(double)); // for elliptic case, we always start with zero initial guess
		RParm[0] = 0.0001;
        RParm[1] = 0.9;
	} else {
		RParm[0] = 0.0;
        RParm[1] = 1.0;
	}

	// Call fortran wrapper for pcgpack
	// ---------------------------------
	// Set tolerance
	double PCG_Tolerance = PCG_TOLERANCE;

	SimTool::getInstance()->startTimer(tHndPCG);

	string varname = var->getName();
	double RHSscale = computeRHSscale(size, pRHS, varname);

#ifdef SHOW_MATRIX
		cout << setprecision(10);
		cout << "----SparseLinearSolver----Variable " << var->getName() << " at " << SimTool::getInstance()->getSimulation()->getTime_sec() << "---------------" << endl;
		A->show();
		cout << "--------SparseLinearSolver----RHS-----------" << endl;
		for (int index = 0; index < size; index++){
			//if (pRHS[index] != 0) {
				cout << index << "\t" << pRHS[index] << endl;
			//}
		}
#endif

	PCGWRAPPER(&size, &Nrsp, &symmetricflg, ija, sa, pRHS, pNew, &PCG_Tolerance, IParm, RParm, pcg_workspace, pcg_workspace, &RHSscale); 
	SimTool::getInstance()->stopTimer(tHndPCG);

#ifdef SHOW_IPARM
	cout << endl << "-------SparseLinearSolver----numNonZeros=" << A->getNumNonZeros() << "--------------------" << endl;
	cout << endl << "-------SparseLinearSolver----IPARM--------------------" << endl;
	for (int i = 0; i < 75; i ++) {
		if (IParm[i] != 0) {
			cout << "[" << (i + 1) << "," << IParm[i] << "] ";
		}
	}
	cout << endl;
#endif

#ifdef SHOW_MATRIX
	if (IParm[50] == 0) {
		cout << "--------SparseLinearSolver----Solution-----------" << endl;
		for (int index = 0; index < size; index++){
			//if (pNew[index] != 0) {
				cout << index << "\t" << pNew[index] << endl;
			//}
		}
	}
#endif
	
	// for periodic boundary condition and solve region
	if (IParm[50] == 0) {
		smEqnBuilder->postProcess();
	}
	
	return IParm;
}
