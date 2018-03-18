/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */

#ifdef VCELL_PETSC
 
#include <VCELL/PetscPdeScheduler.h>

#include <algorithm>
#include <iostream>
using std::max;
using std::endl;

#include <VCELL/Element.h>
#include <VCELL/SimTypes.h>
#include <VCELL/Solver.h>
#include <VCELL/VolumeVariable.h>
#include <VCELL/MembraneVariable.h>
#include <VCELL/VolumeRegionVariable.h>
#include <VCELL/MembraneRegionVariable.h>
#include <VCELL/Mesh.h>
#include <VCELL/Feature.h>
#include <VCELL/Membrane.h>
#include <VCELL/VolumeVarContextExpression.h>
#include <VCELL/VolumeRegionVarContextExpression.h>
#include <VCELL/MembraneVarContextExpression.h>
#include <VCELL/MembraneRegionVarContextExpression.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/FVUtils.h>
#include <VCELL/SimTool.h>
#include <VCELL/CartesianMesh.h>
#include <VCELL/VolumeRegion.h>
#include <VCELL/MembraneRegion.h>
#include <VCELL/VCellModel.h>
#include <SimpleSymbolTable.h>
#include <VCELL/SparseMatrixPCG.h>

#include <assert.h>
#include <string.h>
#include <limits.h>

#define epsilon  1e-12

//#define SHOW_RHS
#define PRECOMPUTE_DIFFUSION_COEFFICIENT

static double interp_coeff[3] = {3.0/2, -1.0/2, 0};
static int dirichletExpIndexes[3] = {BOUNDARY_XP_EXP, BOUNDARY_YP_EXP, BOUNDARY_ZP_EXP};
static int velocityExpIndexes[3] = {VELOCITY_X_EXP, VELOCITY_Y_EXP, VELOCITY_Z_EXP};
static int gradExpIndexes[3] = {GRADIENT_X_EXP, GRADIENT_Y_EXP, GRADIENT_Z_EXP};
static int minusMasks[3] = {NEIGHBOR_XM_MASK, NEIGHBOR_YM_MASK, NEIGHBOR_ZM_MASK};
static int plusMasks[3] = {NEIGHBOR_XP_MASK, NEIGHBOR_YP_MASK, NEIGHBOR_ZP_MASK};



PetscPdeScheduler::PetscPdeScheduler(Simulation *sim) : Scheduler(sim)
{
	simulation = (SimulationExpression*)sim;
	mesh = (CartesianMesh*)simulation->getMesh();

	vecY = 0;
	ts = 0;

	numUnknowns = 0;

	currentTime = 0;

	statePointValues = 0;
	neighborStatePointValues = 0;

	diffCoeffs = 0;

	internalDt = sim->getDT_sec();
}


PetscPdeScheduler::~PetscPdeScheduler()
{
	VecDestroy(&vecY);
	VecDestroy(&oldY);
	TSDestroy(&ts);
	SNESDestroy(&snes);
	MatDestroy(&Pmat);

	delete[] statePointValues;

	if (bHasVariableDiffusionAdvection) {
		for (int n = 0; n < 3; n ++) {
			delete[] neighborStatePointValues[n];
		}
		delete neighborStatePointValues;
	}

	delete[] global2Local;
	delete[] local2Global;
	delete[] regionSizes;
	delete[] regionOffsets;
	delete[] volVectorOffsets;

	if (simulation->getNumVolVariables() > 0) {
		int numVolRegions = mesh->getNumVolumeRegions();
		delete[] regionDefinedVolVariableSizes;
		for (int r = 0; r < numVolRegions; r ++) {
			delete[] regionDefinedVolVariableIndexes[r];
		}
		delete[] regionDefinedVolVariableIndexes;
		delete[] bRegionHasTimeDepdentVariables;
		delete[] bRegionHasConstantCoefficients;
	}

	delete[] diffCoeffs;
}

void PetscPdeScheduler::iterate() {
	if (bFirstTime) {
		setupOrderMaps();
		initPetscSolver();
	}
	solve();
	bFirstTime = false;
}

void PetscPdeScheduler::setupOrderMaps() {
	if (numUnknowns > 0) {
		return;
	}

	dimension =  mesh->getDimension();
	double Dx = mesh->getXScale_um();
	double Dy = mesh->getYScale_um();
	double Dz = mesh->getZScale_um();
	Nx = mesh->getNumVolumeX();
	int Ny = mesh->getNumVolumeY();
	int Nz = mesh->getNumVolumeZ();
	Nxy = Nx * Ny;
	int Nxyz = Nxy * Nz;

	VOLUME = mesh->getVolume_cu();

	oneOverH[0] = 1/Dx;
	oneOverH[1] = 1/Dy;
	oneOverH[2] = 1/Dz;

	pVolumeElement = mesh->getVolumeElements();
	pMembraneElement = mesh->getMembraneElements();

	int numVolRegions = mesh->getNumVolumeRegions();
	global2Local = new int[Nxyz];
	local2Global = new int[Nxyz];
	regionSizes = new int[numVolRegions];
	regionOffsets = new int[numVolRegions];
	regionDefinedVolVariableSizes = 0;
	regionDefinedVolVariableIndexes = 0;
	bRegionHasConstantCoefficients = 0;
	bHasAdvection = false;
	bHasVariableDiffusionAdvection = false;

	bRegionHasTimeDepdentVariables = 0;

	int numActivePoints = 0;
	cout << endl << "numVolRegions=" << numVolRegions << endl;
	for (int r = 0; r < numVolRegions; r ++) {
		regionOffsets[r] = numActivePoints;
		for (int k = 0; k < Nz; k ++) {
			for (int j = 0; j < Ny; j ++) {
				for (int i = 0; i < Nx; i ++) {
					int volIndex = k * Nxy + j * Nx + i;
					int regionID = pVolumeElement[volIndex].getRegionIndex();
					if (regionID == r) {
						//global to local
						global2Local[volIndex] = numActivePoints;
						local2Global[numActivePoints] = volIndex;
						numActivePoints ++;
					}
				}
			}
		}
		regionSizes[r] = numActivePoints - regionOffsets[r];
		cout << "Region " << r << ": size=" << regionSizes[r] << ", offset=" << regionOffsets[r] << endl;
	}
	cout << "# of active points = " << numActivePoints << endl;

	// if local to global mapping is not always in increasing order
	// then in the rhs operator, we need a separate loop for reaction and scale volume
	for (int r = 0; r < numVolRegions; r ++) {
		int localIndex0 = regionOffsets[r];
		for (int localIndex = localIndex0 ; localIndex < localIndex0 + regionSizes[r]-1; localIndex ++) {
			int volIndex = local2Global[localIndex];
			int volIndexp = local2Global[localIndex+1];
			if (volIndexp < volIndex) {
				throw " local to global map not in increasing order";
			}
		}
	}

	volVectorOffsets = new int[numVolRegions];
	numUnknowns  = 0;
	if (simulation->getNumVolVariables() > 0) {
		bRegionHasConstantCoefficients = new bool[numVolRegions];
		regionDefinedVolVariableSizes = new int[numVolRegions];
		regionDefinedVolVariableIndexes = new int*[numVolRegions];

#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
		bRegionHasTimeDepdentVariables = new bool[numVolRegions];
#endif

		// Volume PDE/ODE first
		for (int r = 0; r < numVolRegions; r ++) {
			volVectorOffsets[r] = numUnknowns;
			regionDefinedVolVariableSizes[r] = 0;
			regionDefinedVolVariableIndexes[r] = 0;
			bRegionHasConstantCoefficients[r] = true;

#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
			bRegionHasTimeDepdentVariables[r] = false;
#endif
			int firstPointVolIndex = local2Global[regionOffsets[r]];
			Feature* feature = pVolumeElement[firstPointVolIndex].getFeature();
			for (int v = 0; v < simulation->getNumVolVariables(); v ++) {
				if (simulation->isVolumeVariableDefinedInRegion(v, r)) {
					numUnknowns += regionSizes[r];
					regionDefinedVolVariableSizes[r] ++;

					VolumeVariable* var = (VolumeVariable*)simulation->getVolVariable(v);
 					VolumeVarContextExpression* varContext = feature->getVolumeVarContext(var);
					if (var->isDiffusing() && !varContext->hasConstantCoefficients(dimension)) {
						bRegionHasConstantCoefficients[r] = false;
						bHasVariableDiffusionAdvection = true;
					}
					if (var->isAdvecting()) {
						bHasAdvection = true;
#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
						bRegionHasTimeDepdentVariables[r] = true;
#endif
					}
				}
			}
			if (regionDefinedVolVariableSizes[r] > 0) {
				cout << (bRegionHasConstantCoefficients[r] ? "Constant" : "Variable") << " diffusion/advection in region " << mesh->getVolumeRegion(r)->getName() << endl;
				regionDefinedVolVariableIndexes[r] = new int[regionDefinedVolVariableSizes[r]];
				int activeVarCount = 0;
				for (int v = 0; v < simulation->getNumVolVariables(); v ++) {
					if (simulation->isVolumeVariableDefinedInRegion(v, r)) {
						regionDefinedVolVariableIndexes[r][activeVarCount ++] = v;
					}

#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
					if (bRegionHasTimeDepdentVariables[r]) {
						continue;
					}

					VolumeVariable* var = (VolumeVariable*)simulation->getVolVariable(v);
 					VolumeVarContextExpression* varContext = feature->getVolumeVarContext(var);
					if (var->isDiffusing()) {
						if (!varContext->hasConstantDiffusion() &&  !varContext->hasXYZOnlyDiffusion() || var->hasGradient()) {
							bRegionHasTimeDepdentVariables[r] = true;
						}
					}
#endif
				}
			}
		}
	}
	numVolUnknowns = numUnknowns;

	// Membrane ODE/PDE
	memVectorOffset = numUnknowns;
	numUnknowns += mesh->getNumMembraneElements() * simulation->getNumMemVariables();
	// Volume Region
	volRegionVectorOffset = numUnknowns;
	numUnknowns += numVolRegions * simulation->getNumVolRegionVariables();
	// Membrane Region
	memRegionVectorOffset = numUnknowns;
	numUnknowns += mesh->getNumMembraneRegions() * simulation->getNumMemRegionVariables();

	cout << "numUnknowns = " << numUnknowns << endl;
}

PetscErrorCode PetscPdeScheduler::RHS_Function(TS ts, PetscReal t, Vec Y, Vec F, void* ctx)
{
	double *yinput;
	double* rhs;
	VecGetArrayRead(Y, (const double**)&yinput);
	VecGetArray(F, &rhs);

	PetscPdeScheduler* solver = (PetscPdeScheduler*)ctx;
	solver->rhs(t, yinput, rhs);

	VecRestoreArrayRead(Y, (const PetscScalar**)&yinput);
	VecRestoreArray(F, &rhs);

	return 0;
}

/*
   RHSFunction - Evaluates nonlinear function, F(x).

   Input Parameters:
.  ts - the TS context
.  X - input vector
.  ptr - optional user-defined context, as set by TSSetRHSFunction()

   Output Parameter:
.  F - function vector
 */
PetscErrorCode PetscPdeScheduler::rhs(PetscReal t, double* yinput, double* rhs) {
	//cout << yinput[14008] << endl;
	//if (MathUtil::isInfinity(yinput[14008]) || MathUtil::isNaN(yinput[14008])) {
	//	exit(1);
	//}
	memset(rhs, 0, numUnknowns * sizeof(double));

	applyVolumeOperator(t, yinput, rhs);
	applyMembraneDiffusionReactionOperator(t, yinput, rhs);
	applyVolumeRegionReactionOperator(t, yinput, rhs);
	applyMembraneRegionReactionOperator(t, yinput, rhs);
	applyMembraneFluxOperator(t, yinput, rhs);

#ifdef SHOW_RHS
	cout << endl << "-----------RHS----at time " << t << "--------------" << endl;
	for (int i = 0; i < numUnknowns; i ++) {
		cout << i << " " << rhs[i] << endl;
	}
	cout << endl;
#endif

	return 0;
}

PetscErrorCode PetscPdeScheduler::initPetscSolver() {
	if (!bFirstTime) {
		return 0;
	}

	int numVolVar = simulation->getNumVolVariables();
	int numMemVar = simulation->getNumMemVariables();
	int numVolRegionVar = simulation->getNumVolRegionVariables();
	int numMemRegionVar = simulation->getNumMemRegionVariables();
	if (ts == 0)
	{
		numSymbolsPerVolVar = SimTool::getInstance()->getModel()->getNumFeatures() + 1;

		int valueArraySize = simulation->numOfSymbols();

		statePointValues = new double[valueArraySize];
		memset(statePointValues, 0, valueArraySize * sizeof(double));

		if (bHasVariableDiffusionAdvection) {
			neighborStatePointValues = new double*[3];
			for (int n = 0; n < 3; n ++) {
				neighborStatePointValues[n] = new double[valueArraySize];
				memset(neighborStatePointValues[n], 0, valueArraySize * sizeof(double));
			}
		}

		VecCreateSeq(PETSC_COMM_SELF, numUnknowns, &vecY);
		/*
		TSCreate(PETSC_COMM_WORLD,&ts);
		TSSetProblemType(ts,TS_NONLINEAR);
//		TSSetType(ts,TSARKIMEX);
		TSSetType(ts, TSBEULER);
//		TSARKIMEXSetFullyImplicit(ts,PETSC_TRUE);


		TSSetRHSFunction(ts, NULL, RHS_Function, this);

		TSSetSolution(ts, vecY);
		TSSetExactFinalTime(ts, TS_EXACTFINALTIME_MATCHSTEP);
		TSSetFromOptions(ts);

		TSGetSNES(ts, &snes);
*/

		MatCreate(PETSC_COMM_WORLD, &Pmat);
		MatSetSizes(Pmat, PETSC_DECIDE,PETSC_DECIDE, numUnknowns, numUnknowns);
		MatSetType(Pmat, MATAIJ);
		int* nnz = new int[numUnknowns];
		computeNonZeros(nnz);
		MatSeqAIJSetPreallocation(Pmat, PETSC_DECIDE, nnz);
//		MatMPIAIJSetPreallocation(J, PETSC_DECIDE, NULL, PETSC_DECIDE, NULL);
        delete[] nnz;

        SNESCreate(PETSC_COMM_WORLD, &snes);
		SNESSetFromOptions(snes);

		Vec r;
		VecDuplicate(vecY, &r);
		VecDuplicate(vecY, &oldY);
		SNESSetFunction(snes, r, SNES_Function, this);

		Mat Jmf;
		MatCreateSNESMF(snes, &Jmf);   // matrix free J
//		SNESSetJacobian(snes, Jmf, J, SNESComputeJacobianDefault, this);  //SNESComputeJacobianDefaultColor
//		SNESSetJacobian(snes, Jmf, Pmat, MatMFFDComputeJacobian, this);
		SNESSetJacobian(snes, Jmf, Pmat, SNES_Jacobian_Function, this);

//		TSSetRHSJacobian(ts, J, J, ts_Jacobian_Function, this);

		SNESSetLagPreconditioner(snes, -2);
		SNESSetLagPreconditionerPersists(snes, PETSC_TRUE);

		KSP ksp;
		PC pc;
		SNESGetKSP(snes, &ksp);
		KSPSetType(ksp, KSPGMRES);
		KSPSetReusePreconditioner(ksp, PETSC_TRUE);

		KSPGetPC(ksp, &pc);
//		PCSetType(pc, PCNONE);
		PCSetType(pc, PCILU);
		PCFactorSetLevels(pc, 0);
		PCSetReusePreconditioner(pc, PETSC_TRUE);

		KSPGMRESSetRestart(ksp, 5);

		KSPSetFromOptions(ksp);
		PCSetFromOptions(pc);

//		double initial_dt = sundialsSolverOptions.maxStep; //0.0001;
//		TSSetInitialTimeStep(ts, currentTime, initial_dt);
	}

#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
	// initialize diff coeff
	if (bHasVariableDiffusionAdvection) {
		precomputeDiffusionCoefficients();
	}
#endif

	// set up initial conditions
	int count = 0;
	double *y_data;
	VecGetArray(vecY, &y_data);
	if (numVolVar > 0) {
		for (int r = 0; r < mesh->getNumVolumeRegions(); r ++) {
			for (int ri = 0; ri < regionSizes[r]; ri ++) {
				int volIndex = local2Global[ri + regionOffsets[r]];
				for (int activeVarCount = 0; activeVarCount < regionDefinedVolVariableSizes[r]; activeVarCount ++) {
					int varIndex = regionDefinedVolVariableIndexes[r][activeVarCount];
					y_data[count ++] = simulation->getVolVariable(varIndex)->getCurr(volIndex);
				}
			}
		}
	}
	if (simulation->getNumMemVariables() > 0) {
		for (int mi = 0; mi < mesh->getNumMembraneElements(); mi ++) {
			for (int v = 0; v < simulation->getNumMemVariables(); v ++) {
				y_data[count ++] = simulation->getMemVariable(v)->getCurr(mi);
			}
		}
	}

	if (simulation->getNumVolRegionVariables() > 0) {
		for (int r = 0; r < mesh->getNumVolumeRegions(); r ++) {
			for (int v = 0; v < simulation->getNumVolRegionVariables(); v ++) {
				y_data[count ++] = simulation->getVolRegionVariable(v)->getCurr(r);
			}
		}
	}

	if (simulation->getNumMemRegionVariables() > 0) {
		for (int r = 0; r < mesh->getNumMembraneRegions(); r ++) {
			for (int v = 0; v < simulation->getNumMemRegionVariables(); v ++) {
				y_data[count ++] = simulation->getMemRegionVariable(v)->getCurr(r);
			}
		}
	}
	VecRestoreArray(vecY, &y_data);

	cout << endl << "----------------------------------" << endl;
	cout << "petsc pde solver is starting from time " << currentTime << endl;
	cout << "----------------------------------" << endl;

	// only populate once serial scan parameter values
	simulation->populateParameterValuesNew(statePointValues);
	if (bHasVariableDiffusionAdvection) {
		for (int n = 0; n < 3; n ++) {
			simulation->populateParameterValuesNew(neighborStatePointValues[n]);
		}
	}
}

static int numInternalIter = 32;
static int linitsHi = 45;
static int linitsLo = 20;
static int maxInternalIter = numInternalIter;
static double maxInteralIterTime = 0;
PetscErrorCode PetscPdeScheduler::solve() {
	//static string METHOD = "PetscPdeScheduler::solve";
	//cout << "Entry " << METHOD << endl;

	internalDt = simulation->getDT_sec() / numInternalIter;
	for (int iter = 0; iter < numInternalIter; ++ iter)
	{
		VecCopy(vecY, oldY);
		SNESSolve(snes, oldY, vecY);

		currentTime += internalDt;
	}
	int nonlinits, linits;
	KSP ksp;
	PC pc;
	SNESGetKSP(snes, &ksp);
	KSPGetPC(ksp, &pc);
	SNESGetIterationNumber(snes, &nonlinits);
	SNESGetKSP(snes, &ksp);
	KSPGetIterationNumber(ksp, &linits);
	int restart;
	KSPGMRESGetRestart(ksp, &restart);
	KSPGetPC(ksp, &pc);
	PCType pctype;
	PCGetType(pc, &pctype);
	int factorLevel = 0;
	PCFactorGetLevels(pc, &factorLevel);
	PetscBool reuse;
	PCGetReusePreconditioner(pc, &reuse);
	PetscPrintf(PETSC_COMM_WORLD, "\nt %f, internalits %d, internal-dt %f, pctype %s, factor level %d, pc reuse %d, gmres restart %d, "
			"nonlinits %D, linits %D\n", currentTime, numInternalIter, internalDt, pctype, factorLevel, reuse, restart, nonlinits, linits);
	updateSolutions();

	if (linits > linitsHi)
	{
		numInternalIter *= 2;
	}
	else if (linits < linitsLo && numInternalIter >= 2)
	{
		numInternalIter = numInternalIter / 2;
	}
	if (maxInternalIter < numInternalIter)
	{
		maxInternalIter = std::max(maxInternalIter, numInternalIter);
		maxInteralIterTime = currentTime;
	}

	if (std::abs(SimTool::getInstance()->getEndTime() - simulation->getTime_sec()) < epsilon)
	{
		PetscPrintf(PETSC_COMM_WORLD, "\nmaxInternalIter:%d, maxInteralIterTime:%f", maxInternalIter, maxInteralIterTime);
	}
	//cout << "Exit " << METHOD << endl;
}

void PetscPdeScheduler::updateSolutions() {
	double *y_data;
	VecGetArrayRead(vecY, (const PetscScalar**)&y_data);
	int count = 0;

	if (simulation->getNumVolVariables() > 0) {
		for (int r = 0; r < mesh->getNumVolumeRegions(); r ++) {
			bool bDirichletBoundary = mesh->getVolumeRegion(r)->isBoundaryDirichlet();
			for (int ri = 0; ri < regionSizes[r]; ri ++) {
				int volIndex = local2Global[ri + regionOffsets[r]];
				int mask = pVolumeElement[volIndex].neighborMask;

				for (int activeVarCount = 0; activeVarCount < regionDefinedVolVariableSizes[r]; activeVarCount ++) {
					int varIndex = regionDefinedVolVariableIndexes[r][activeVarCount];
					VolumeVariable *var = simulation->getVolVariable(varIndex);

					double sol = y_data[count ++];
					if (var->isDiffusing() && bDirichletBoundary && (mask & BOUNDARY_TYPE_DIRICHLET)) {
						updateVolumeStatePointValues(volIndex, currentTime, y_data, statePointValues);
						Feature* feature = pVolumeElement[volIndex].getFeature();
						VolumeVarContextExpression* varContext = feature->getVolumeVarContext(var);
						if ((mask & NEIGHBOR_XM_BOUNDARY) && (feature->getXmBoundaryType() == BOUNDARY_VALUE)){
							sol = varContext->evaluateExpression(BOUNDARY_XM_EXP, statePointValues);
						} else if ((mask & NEIGHBOR_XP_BOUNDARY) && (feature->getXpBoundaryType() == BOUNDARY_VALUE)){
							sol = varContext->evaluateExpression(BOUNDARY_XP_EXP, statePointValues);
						} else if ((mask & NEIGHBOR_YM_BOUNDARY) && (feature->getYmBoundaryType() == BOUNDARY_VALUE)){
							sol = varContext->evaluateExpression(BOUNDARY_YM_EXP, statePointValues);
						} else if ((mask & NEIGHBOR_YP_BOUNDARY) && (feature->getYpBoundaryType() == BOUNDARY_VALUE)){
							sol = varContext->evaluateExpression(BOUNDARY_YP_EXP, statePointValues);
						} else if ((mask & NEIGHBOR_ZM_BOUNDARY) && (feature->getZmBoundaryType() == BOUNDARY_VALUE)){
							sol = varContext->evaluateExpression(BOUNDARY_ZM_EXP, statePointValues);
						} else if ((mask & NEIGHBOR_ZP_BOUNDARY) && (feature->getZpBoundaryType() == BOUNDARY_VALUE)){
							sol = varContext->evaluateExpression(BOUNDARY_ZP_EXP, statePointValues);
						}
					}
					var->getCurr()[volIndex] = sol;
				}
			}
		}
	}

	// update membrane
	if (simulation->getNumMemVariables() > 0) {
		for (int m = 0; m < mesh->getNumMembraneElements(); m ++) {
			for (int v = 0; v < simulation->getNumMemVariables(); v ++) {
				MembraneVariable *var = (MembraneVariable*)simulation->getMemVariable(v);
				int mask = mesh->getMembraneNeighborMask(m);
				double sol = y_data[count ++];
				if (var->isDiffusing() && (mask & BOUNDARY_TYPE_DIRICHLET)) {
					updateMembraneStatePointValues(pMembraneElement[m], currentTime, y_data, statePointValues);

					Membrane* membrane = pMembraneElement[m].getMembrane();
					MembraneVarContextExpression *varContext = membrane->getMembraneVarContext(var);
					if ((mask & NEIGHBOR_XM_BOUNDARY) && (membrane->getXmBoundaryType() == BOUNDARY_VALUE)){
						sol = varContext->evaluateExpression(BOUNDARY_XM_EXP, statePointValues);
					} else if ((mask & NEIGHBOR_XP_BOUNDARY) && (membrane->getXpBoundaryType() == BOUNDARY_VALUE)){
						sol = varContext->evaluateExpression(BOUNDARY_XP_EXP, statePointValues);
					} else if ((mask & NEIGHBOR_YM_BOUNDARY) && (membrane->getYmBoundaryType() == BOUNDARY_VALUE)){
						sol = varContext->evaluateExpression(BOUNDARY_YM_EXP, statePointValues);
					} else if ((mask & NEIGHBOR_YP_BOUNDARY) && (membrane->getYpBoundaryType() == BOUNDARY_VALUE)){
						sol = varContext->evaluateExpression(BOUNDARY_YP_EXP, statePointValues);
					} else if ((mask & NEIGHBOR_ZM_BOUNDARY) && (membrane->getZmBoundaryType() == BOUNDARY_VALUE)){
						sol = varContext->evaluateExpression(BOUNDARY_ZM_EXP, statePointValues);
					} else if ((mask & NEIGHBOR_ZP_BOUNDARY) && (membrane->getZpBoundaryType() == BOUNDARY_VALUE)){
						sol = varContext->evaluateExpression(BOUNDARY_ZP_EXP, statePointValues);
					}
				}
				var->getCurr()[m] = sol;
			}
		}
	}

	// update volume region variable
	if (simulation->getNumVolRegionVariables() > 0) {
		for (int r = 0; r < mesh->getNumVolumeRegions(); r ++) {
			for (int v = 0; v < simulation->getNumVolRegionVariables(); v ++) {
				VolumeRegionVariable *var = simulation->getVolRegionVariable(v);
				double sol = y_data[count ++];
				var->getCurr()[r] = sol;
			}
		}
	}
	// update membrane region variable
	if (simulation->getNumMemRegionVariables() > 0) {
		for (int r = 0; r < mesh->getNumMembraneRegions(); r ++) {
			for (int v = 0; v < simulation->getNumMemRegionVariables(); v ++) {
				MembraneRegionVariable *var = simulation->getMemRegionVariable(v);
				double sol = y_data[count ++];
				var->getCurr()[r] = sol;
			}
		}
	}
	VecRestoreArrayRead(vecY, (const PetscScalar**)&y_data);
}

void PetscPdeScheduler::applyVolumeOperatorOld(double t, double* yinput, double* rhs) {
	short advectDirs[6] = {1, 1, 1, -1, -1, -1};
	int dirichletExpIndexes[6] = {BOUNDARY_ZM_EXP, BOUNDARY_YM_EXP, BOUNDARY_XM_EXP, BOUNDARY_XP_EXP, BOUNDARY_YP_EXP, BOUNDARY_ZP_EXP};
	int velocityExpIndexes[6] = {VELOCITY_Z_EXP, VELOCITY_Y_EXP, VELOCITY_X_EXP, VELOCITY_X_EXP, VELOCITY_Y_EXP, VELOCITY_Z_EXP};

	for (int r = 0; r < mesh->getNumVolumeRegions(); r ++) {
		int activeVarCount = -1;
		for (int v = 0; v < simulation->getNumVolVariables(); v ++) {
			VolumeVariable* var = simulation->getVolVariable(v);

			if (!simulation->isVolumeVariableDefinedInRegion(v, r)) {
				continue;
			}
			activeVarCount ++;
			int firstPointVolIndex = local2Global[regionOffsets[r]];
			Feature* feature = pVolumeElement[firstPointVolIndex].getFeature();
			VolumeVarContextExpression* varContext = feature->getVolumeVarContext(var);
			bool bHasConstantDiffusion = false;
			double Di = 0;
			if (varContext->hasConstantDiffusion()) {
				bHasConstantDiffusion = true;
				Di = varContext->evaluateConstantExpression(DIFF_RATE_EXP);
			}

			for (int ri = 0; ri < regionSizes[r]; ri ++) {
				int localIndex = ri + regionOffsets[r];
				int volIndex = local2Global[localIndex];
				int mask = pVolumeElement[volIndex].neighborMask;
				int vectorIndex = getVolumeElementVectorOffset(volIndex, r) + activeVarCount;

				// update values
				if (!var->isDiffusing() || !(mask & BOUNDARY_TYPE_DIRICHLET)){
					updateVolumeStatePointValues(volIndex, t, yinput, statePointValues);
					double reactionRate = varContext->evaluateExpression(REACT_RATE_EXP, statePointValues);
					// add reaction
					rhs[vectorIndex] += reactionRate;
				}

				if (!var->isDiffusing()) {
					//validateNumber(var->getName(), volIndex, "RHS", rhs[vectorIndex]);
					continue;
				}

				if (mask & BOUNDARY_TYPE_DIRICHLET) {// dirichlet
					rhs[vectorIndex] = 0;
					continue;
				}

				double lambdaX = oneOverH[0]*oneOverH[0];
				double lambdaY = oneOverH[1]*oneOverH[1];
				double lambdaZ = oneOverH[2]*oneOverH[2];
				double lambdaAreaX = oneOverH[0];
				double lambdaAreaY = oneOverH[1];
				double lambdaAreaZ = oneOverH[2];

				if (mask & NEIGHBOR_BOUNDARY_MASK){   // boundary
					if (mask & NEIGHBOR_X_BOUNDARY_MASK){
						lambdaX *= 2.0;
						lambdaAreaX *= 2.0;
					}
					if (mask & NEIGHBOR_Y_BOUNDARY_MASK){
						lambdaY *= 2.0;
						lambdaAreaY *= 2.0;
					}
					if (mask & NEIGHBOR_Z_BOUNDARY_MASK){
						lambdaZ *= 2.0;
						lambdaAreaZ *= 2.0;
					}

					// add neumann condition
					// boundary flux equals -D*DU/DX at the xm and xp walls, -D*DU/DY at the ym and yp walls
					if ((mask & BOUNDARY_TYPE_MASK) == BOUNDARY_TYPE_NEUMANN) {
						// updateVolumeStatePointValues(volIndex, t, yinput);  // this is needed, but update already above, if you move this block, uncomment this statement
						if (mask & NEIGHBOR_XM_BOUNDARY && feature->getXmBoundaryType() == BOUNDARY_FLUX){
							rhs[vectorIndex] += varContext->evaluateExpression(BOUNDARY_XM_EXP, statePointValues) * lambdaAreaX;
						}
						if (mask & NEIGHBOR_XP_BOUNDARY && feature->getXpBoundaryType() == BOUNDARY_FLUX){
							rhs[vectorIndex] -= varContext->evaluateExpression(BOUNDARY_XP_EXP, statePointValues) * lambdaAreaX;
						}
						if (dimension > 1) {
							if (mask & NEIGHBOR_YM_BOUNDARY && feature->getYmBoundaryType() == BOUNDARY_FLUX){
								rhs[vectorIndex] += varContext->evaluateExpression(BOUNDARY_YM_EXP, statePointValues) * lambdaAreaY;
							}
							if (mask & NEIGHBOR_YP_BOUNDARY && feature->getYpBoundaryType() == BOUNDARY_FLUX){
								rhs[vectorIndex] -= varContext->evaluateExpression(BOUNDARY_YP_EXP, statePointValues) * lambdaAreaY;
							}

							if (dimension > 2) {
								if (mask & NEIGHBOR_ZM_BOUNDARY && feature->getZmBoundaryType() == BOUNDARY_FLUX){
									rhs[vectorIndex] += varContext->evaluateExpression(BOUNDARY_ZM_EXP, statePointValues) * lambdaAreaZ;
								}
								if (mask & NEIGHBOR_ZP_BOUNDARY && feature->getZpBoundaryType() == BOUNDARY_FLUX){
									rhs[vectorIndex] -= varContext->evaluateExpression(BOUNDARY_ZP_EXP, statePointValues) * lambdaAreaZ;
								}
							}
						}
					}
				}

				// compute Di, Vi
				if (!bHasConstantDiffusion) {
					Di = varContext->evaluateExpression(DIFF_RATE_EXP, statePointValues);
				}

				double Vix=0, Viy=0, Viz=0;
				if (var->isAdvecting()) {
					// updateVolumeStatePointValues(volIndex, t, yinput);  // this is needed, but update already above, if you move this block, uncomment this statement
					Vix = varContext->evaluateExpression(VELOCITY_X_EXP, statePointValues);
					if (dimension > 1) {
						Viy = varContext->evaluateExpression(VELOCITY_Y_EXP, statePointValues);
					}
					if (dimension > 2) {
						Viz = varContext->evaluateExpression(VELOCITY_Z_EXP, statePointValues);
					}
				}

				// order is ZM, YM, XM, XP, YP, ZP
				int volumeNeighbors[6] = {
					(dimension < 3 || mask & NEIGHBOR_ZM_MASK) ? -1 : volIndex - Nxy,
					(dimension < 2 || mask & NEIGHBOR_YM_MASK) ? -1 : volIndex - Nx,
					(mask & NEIGHBOR_XM_MASK) ? -1 : volIndex - 1,
					(mask & NEIGHBOR_XP_MASK) ? -1 : volIndex + 1,
					(dimension < 2 || mask & NEIGHBOR_YP_MASK) ? -1 : volIndex + Nx,
					(dimension < 3 || mask & NEIGHBOR_ZP_MASK) ? -1 : volIndex + Nxy
				};

				double neighborLambdas[6] = {lambdaZ, lambdaY, lambdaX, lambdaX, lambdaY, lambdaZ};
				double neighborLambdaAreas[6] = {lambdaAreaZ, lambdaAreaY, lambdaAreaX, lambdaAreaX, lambdaAreaY, lambdaAreaZ};
				double Vis[6] = {Viz, Viy, Vix, Vix, Viy, Viz};

				double Aii = 0;
				// add diffusion and convection
				for (int n = 0; n < 6; n ++) {
					int neighborIndex = volumeNeighbors[n];
					if (neighborIndex < 0) {
						continue;
					}
					int neighborVectorIndex = getVolumeElementVectorOffset(neighborIndex, r) + activeVarCount;
					double lambda = neighborLambdas[n];

					bool bNeighborStatePointValuesComputed = false;
					double D = Di;
					if (!bHasConstantDiffusion) {
						updateVolumeStatePointValues(neighborIndex, t, yinput, statePointValues);
						bNeighborStatePointValuesComputed = true;
						double Dj = varContext->evaluateExpression(DIFF_RATE_EXP, statePointValues);
						D = (Di + Dj < epsilon) ? (0.0) : (2 * Di * Dj/(Di + Dj));
					}
					double yneighbor = yinput[neighborVectorIndex];
					int neighborMask = pVolumeElement[neighborIndex].neighborMask;
					if (neighborMask & BOUNDARY_TYPE_DIRICHLET) {
						if (!bNeighborStatePointValuesComputed) {
							updateVolumeStatePointValues(neighborIndex, t, yinput, statePointValues);
							bNeighborStatePointValuesComputed = true;
						}
						yneighbor = varContext->evaluateExpression(dirichletExpIndexes[n], statePointValues);
					}

					double Aij = 0;
					if (var->isAdvecting()) {
						if (!bNeighborStatePointValuesComputed) {
							updateVolumeStatePointValues(neighborIndex, t, yinput, statePointValues);
							bNeighborStatePointValuesComputed = true;
						}
						int advectDir = advectDirs[n];
						double Vi = Vis[n];
						double Vj = varContext->evaluateExpression(velocityExpIndexes[n], statePointValues);
						double V = 0.5 * (Vi + Vj);
						double lambdaArea = neighborLambdaAreas[n];
						Aij = max<double>(D * lambda + advectDir * 0.5 * V * lambdaArea, max<double>(advectDir * V * lambdaArea, 0));;
						Aii += max<double>(D * lambda - advectDir * 0.5 * V * lambdaArea, max<double>(- advectDir * V * lambdaArea, 0));
					} else {
						Aij = D * lambda;
						Aii += Aij;
					}
					rhs[vectorIndex] += yneighbor * Aij;
				} // end for n
				rhs[vectorIndex] -=  yinput[vectorIndex] * Aii;
				//validateNumber(var->getName(), volIndex, "RHS", rhs[vectorIndex]);
			} // end for ri
		} // end for v
	} // end for r
}

static void applyAdvectionHybridScheme(double diffTerm, double advectTerm, double& Aii, double& Aij) {
	//Aij = max(diffTerm + 0.5 advectTerm, max(advectTerm, 0));
	//Aii = max(diffTerm - 0.5 advectTerm, max(- advectTerm, 0));

	if (diffTerm < 0.5 * fabs(advectTerm)){ // upwind scheme
		Aij = max<double>(advectTerm, 0);
		Aii = max<double>(- advectTerm, 0);
	} else { // central difference scheme
		Aij = diffTerm + 0.5 * advectTerm;
		Aii = diffTerm - 0.5 * advectTerm;
	}
}

void PetscPdeScheduler::applyVolumeOperator(double t, double* yinput, double* rhs) {
	if (simulation->getNumVolVariables() == 0) {
		return;
	}
	for (int r = 0; r < mesh->getNumVolumeRegions(); r ++) {
		if (bRegionHasConstantCoefficients[r]) {
			regionApplyVolumeOperatorConstant(r, t, yinput, rhs);
		} else {
			regionApplyVolumeOperatorVariable(r, t, yinput, rhs);
		}
	}
}

// for dirichlet point, boundary condition has to be used.
void PetscPdeScheduler::dirichletPointSetup(int volIndex, Feature* feature, VarContext* varContext, int mask, int* volumeNeighbors, double& ypoint) {
	volumeNeighbors[0] = -1;
	volumeNeighbors[1] = -1;
	volumeNeighbors[2] = -1;
	if (dimension > 2 && (mask & NEIGHBOR_ZM_MASK))  {
		if (!(mask & NEIGHBOR_ZP_MASK)) {
			volumeNeighbors[2] = volIndex + Nxy;
		}
		if (feature->getZmBoundaryType() == BOUNDARY_VALUE) {
			ypoint = varContext->evaluateExpression(BOUNDARY_ZM_EXP,  statePointValues);
		}
	}
	if (dimension > 1 && (mask & NEIGHBOR_YM_MASK))  {
		if (!(mask & NEIGHBOR_YP_MASK)) {
			volumeNeighbors[1] = volIndex + Nx;
		}
		if (feature->getYmBoundaryType() == BOUNDARY_VALUE) {
			ypoint = varContext->evaluateExpression(BOUNDARY_YM_EXP, statePointValues);
		}
	}
	if (mask & NEIGHBOR_XM_MASK)  {
		// make sure we take care of mixed bc (e.g. neumann/dirichlet)
		// then only xp is a neighbor
		if (!(mask & NEIGHBOR_XP_MASK)) {
			volumeNeighbors[0] = volIndex + 1;
		}
		if (feature->getXmBoundaryType() == BOUNDARY_VALUE) {
			ypoint = varContext->evaluateExpression(BOUNDARY_XM_EXP, statePointValues);
		}
	}
}

// boundary flux equals -D*DU/DX at the xm and xp walls, -D*DU/DY at the ym and yp walls
double PetscPdeScheduler::computeNeumannCondition(Feature* feature, VarContext* varContext, int mask, double* scaleSs) {
	double boundaryCondition = 0;
	if (mask & NEIGHBOR_XM_BOUNDARY && feature->getXmBoundaryType() == BOUNDARY_FLUX){
		boundaryCondition += varContext->evaluateExpression(BOUNDARY_XM_EXP, statePointValues) * oneOverH[0] * scaleSs[0];
	}
	if (mask & NEIGHBOR_XP_BOUNDARY && feature->getXpBoundaryType() == BOUNDARY_FLUX){
		boundaryCondition -= varContext->evaluateExpression(BOUNDARY_XP_EXP, statePointValues) * oneOverH[0] * scaleSs[0];
	}
	if (dimension > 1) {
		if (mask & NEIGHBOR_YM_BOUNDARY && feature->getYmBoundaryType() == BOUNDARY_FLUX){
			boundaryCondition += varContext->evaluateExpression(BOUNDARY_YM_EXP, statePointValues) * oneOverH[1] * scaleSs[1];
		}
		if (mask & NEIGHBOR_YP_BOUNDARY && feature->getYpBoundaryType() == BOUNDARY_FLUX){
			boundaryCondition -= varContext->evaluateExpression(BOUNDARY_YP_EXP, statePointValues) * oneOverH[1] * scaleSs[1];
		}

		if (dimension > 2) {
			if (mask & NEIGHBOR_ZM_BOUNDARY && feature->getZmBoundaryType() == BOUNDARY_FLUX){
				boundaryCondition += varContext->evaluateExpression(BOUNDARY_ZM_EXP, statePointValues) * oneOverH[2] * scaleSs[2];
			}
			if (mask & NEIGHBOR_ZP_BOUNDARY && feature->getZpBoundaryType() == BOUNDARY_FLUX){
				boundaryCondition -= varContext->evaluateExpression(BOUNDARY_ZP_EXP, statePointValues) * oneOverH[2] * scaleSs[2];
			}
		}
	}
	return boundaryCondition;
}

// scale cross sectional area scale
void PetscPdeScheduler::computeScaleS(int mask, double* scaleS) {
	if (mask & NEIGHBOR_X_BOUNDARY_MASK){
		scaleS[1] /= 2; // Y
		scaleS[2] /= 2; // Z
	}
	if (mask & NEIGHBOR_Y_BOUNDARY_MASK){
		scaleS[0] /= 2; // X
		scaleS[2] /= 2; // Z
	}
	if (mask & NEIGHBOR_Z_BOUNDARY_MASK){
		scaleS[0] /= 2; // X
		scaleS[1] /= 2; // Y
	}
}

// order is XP, YP, ZP
#define defineVolumeNeighbors(volIndex, mask) int volumeNeighbors[3] = { \
									(mask & NEIGHBOR_XP_MASK) ? -1 : volIndex + 1, \
									(dimension < 2 || mask & NEIGHBOR_YP_MASK) ? -1 : volIndex + Nx, \
									(dimension < 3 || mask & NEIGHBOR_ZP_MASK) ? -1 : volIndex + Nxy \
								};


// now we update volume flux symmetrically because D*(Uj - Ui)*S/d is the same (opposite sign) for point i and j.
// but what we computed is D*(Uj - Ui)*S/(d * dV) which will be the same for both too, but at the end, we have to
// scale it with volume scale. So now we only have to process neighbor points whose indexes are bigger.
// because we have to scale the flux, we have to make sure
// 1. we process points from small global index to big global index (we checked this)
// 2. we always add reaction at the end because reaction term doesn't have dV (otherwise we need another loop to process reaction).

void PetscPdeScheduler::regionApplyVolumeOperatorConstant(int regionID, double t, double* yinput, double* rhs) {
	if (regionDefinedVolVariableSizes[regionID] == 0) {
		return;
	}

	int firstPointVolIndex = local2Global[regionOffsets[regionID]];
	Feature* feature = pVolumeElement[firstPointVolIndex].getFeature();
	double (*advectTerms)[6] = 0;
	if (bHasAdvection) {
		advectTerms = new double[regionDefinedVolVariableSizes[regionID]][6];
		memset(advectTerms, 0, regionDefinedVolVariableSizes[regionID] * 6 * sizeof(double));

		for (int activeVarCount = 0; activeVarCount < regionDefinedVolVariableSizes[regionID]; activeVarCount ++) {
			int varIndex = regionDefinedVolVariableIndexes[regionID][activeVarCount];
			VolumeVariable* var = simulation->getVolVariable(varIndex);

			if (var->isAdvecting()) {

				VolumeVarContextExpression* varContext = feature->getVolumeVarContext(var);
				double D = varContext->evaluateConstantExpression(DIFF_RATE_EXP);

				double Vi_XYZ[3] = {0, 0, 0};
				Vi_XYZ[0] = varContext->evaluateConstantExpression(VELOCITY_X_EXP);
				if (dimension > 1) {
					Vi_XYZ[1] = varContext->evaluateConstantExpression(VELOCITY_Y_EXP);
				}
				if (dimension > 2) {
					Vi_XYZ[2] = varContext->evaluateConstantExpression(VELOCITY_Z_EXP);
				}

				for (int n = 0; n < 3; n ++) {
					double diffTerm = D * oneOverH[n];
					double diffAdvectTerm = 0;
					double V = Vi_XYZ[n];
					double advectTerm = -V;

					double Aii = 0, Aij = 0;
					applyAdvectionHybridScheme(diffTerm, advectTerm, Aii, Aij);
					advectTerms[activeVarCount][n*2] = Aii;
					advectTerms[activeVarCount][n*2 + 1] = Aij;
				} // end for n
			}
		}
	}

	// loop through points
	for (int regionPointIndex = 0; regionPointIndex < regionSizes[regionID]; regionPointIndex ++) {
		int localIndex = regionPointIndex + regionOffsets[regionID];
		int volIndex = local2Global[localIndex];
		int mask = pVolumeElement[volIndex].neighborMask;

		bool bDirichlet = false;
		double scaleS[3] = {1, 1, 1};
		if (mask & NEIGHBOR_BOUNDARY_MASK) {
			if (mask & BOUNDARY_TYPE_DIRICHLET) {// dirichlet
				bDirichlet = true;
			}

			computeScaleS(mask, scaleS);
		}

		// Macro
		defineVolumeNeighbors(volIndex, mask)

		// update values for this point
		updateVolumeStatePointValues(volIndex, t, yinput, statePointValues);

		int vectorIndexOffset = getVolumeElementVectorOffset(volIndex, regionID);

		// loop through defined variables
		for (int activeVarCount = 0; activeVarCount < regionDefinedVolVariableSizes[regionID]; activeVarCount ++) {
			// I can also increment this, but
			int vectorIndex = vectorIndexOffset + activeVarCount;

			int varIndex = regionDefinedVolVariableIndexes[regionID][activeVarCount];
			VolumeVariable* var = (VolumeVariable*)simulation->getVolVariable(varIndex);
			VolumeVarContextExpression* varContext = feature->getVolumeVarContext(var);

			double reactionRate = 0;
			if (bDirichlet && var->isDiffusing()) {// pde dirichlet
				rhs[vectorIndex] = 0;
				if (mask & (NEIGHBOR_XP_BOUNDARY || NEIGHBOR_YP_BOUNDARY || NEIGHBOR_ZP_BOUNDARY)) {
					continue;
				}
			} else {
				reactionRate = varContext->evaluateExpression(REACT_RATE_EXP, statePointValues);
				if (!var->isDiffusing()) {
					rhs[vectorIndex] += reactionRate;
					continue;
				}
			}

			double boundaryCondition = 0;
			if (!bDirichlet && (mask & NEIGHBOR_BOUNDARY_MASK)) {   // neumann boundary condition
				if ((mask & BOUNDARY_TYPE_MASK) == BOUNDARY_TYPE_NEUMANN) {
					boundaryCondition = computeNeumannCondition(feature, varContext, mask, scaleS);
				}
			}

			double D = varContext->evaluateConstantExpression(DIFF_RATE_EXP);

			double ypoint = yinput[vectorIndex];
			if (bDirichlet) {
				dirichletPointSetup(volIndex, feature, varContext, mask, volumeNeighbors, ypoint);
			}

			// add diffusion and convection
			for (int n = 0; n < 3; n ++) {
				int neighborIndex = volumeNeighbors[n];
				if (neighborIndex < 0) {
					continue;
				}

				int neighborVectorIndex = getVolumeElementVectorOffset(neighborIndex, regionID) + activeVarCount;
				double yneighbor = yinput[neighborVectorIndex];

				//use the real value if the neighbor is dirichlet point
				int neighborMask = pVolumeElement[neighborIndex].neighborMask;
				bool bNeighborDirichlet = false;
				if (neighborMask & BOUNDARY_TYPE_DIRICHLET) {
					bNeighborDirichlet = true;
					updateVolumeStatePointValues(neighborIndex, t, 0, txyzValues);
					yneighbor = varContext->evaluateExpression(dirichletExpIndexes[n], txyzValues);
				}

				double diffAdvectTerm = 0;
				if (var->isAdvecting()) {
					diffAdvectTerm = yneighbor * advectTerms[activeVarCount][n * 2 + 1] - ypoint * advectTerms[activeVarCount][n * 2];
				} else {
					diffAdvectTerm = (yneighbor - ypoint) * D * oneOverH[n];
				}
				diffAdvectTerm *= scaleS[n] * oneOverH[n];
				if (!bDirichlet) {
					rhs[vectorIndex] += diffAdvectTerm;
				}
				if (!bNeighborDirichlet) {
					rhs[neighborVectorIndex] -= diffAdvectTerm;
				}
			} // end for n
			if (bDirichlet) {
				rhs[vectorIndex] = 0;
			} else {
				// add boundary conditon
				rhs[vectorIndex] += boundaryCondition;
				if (mask & NEIGHBOR_BOUNDARY_MASK){
					// apply volume scale
					rhs[vectorIndex] *= (mask&VOLUME_MASK);
				}
				// add reaction
				rhs[vectorIndex] += reactionRate;
			}
		} // end for v
	} // end for ri
	delete[] advectTerms;
}

void PetscPdeScheduler::precomputeDiffusionCoefficients() {
	if (simulation->getNumVolVariables() == 0) {
		return;
	}

	diffCoeffs = NULL;
	for (int regionIndex = 0; regionIndex < mesh->getNumVolumeRegions(); regionIndex ++) {
		int firstPointVolIndex = local2Global[regionOffsets[regionIndex]];
		Feature* feature = pVolumeElement[firstPointVolIndex].getFeature();

		// loop through defined variables
		for (int activeVarCount = 0; activeVarCount < regionDefinedVolVariableSizes[regionIndex]; activeVarCount ++) {

			int varIndex = regionDefinedVolVariableIndexes[regionIndex][activeVarCount];
			VolumeVariable* var = simulation->getVolVariable(varIndex);
			if (!var->isDiffusing()) {
				continue;
			}
			VolumeVarContextExpression* varContext = feature->getVolumeVarContext(var);
			if (!varContext->hasXYZOnlyDiffusion()) {
				continue;
			}

			if (diffCoeffs == NULL) {
				cout << "precomputing diffusion constants" << endl;
				diffCoeffs = new double[numUnknowns];
				memset(diffCoeffs, 0, numUnknowns * sizeof(double));
			}

			// loop through points
			for (int regionPointIndex = 0; regionPointIndex < regionSizes[regionIndex]; regionPointIndex ++) {
				int localIndex = regionPointIndex + regionOffsets[regionIndex];
				int volIndex = local2Global[localIndex];

				// xyz dependent only
				updateVolumeStatePointValues(volIndex, 0, 0, statePointValues);

				int vectorIndexOffset = getVolumeElementVectorOffset(volIndex, regionIndex);
				int vectorIndex = vectorIndexOffset + activeVarCount;

				diffCoeffs[vectorIndex] = varContext->evaluateExpression(DIFF_RATE_EXP, statePointValues);
			} // point
		} // var
	} // region
}

void PetscPdeScheduler::regionApplyVolumeOperatorVariable(int regionID, double t, double* yinput, double* rhs) {
	if (regionDefinedVolVariableSizes[regionID] == 0) {
		return;
	}

	int firstPointVolIndex = local2Global[regionOffsets[regionID]];
	Feature* feature = pVolumeElement[firstPointVolIndex].getFeature();

	// loop through points
	for (int regionPointIndex = 0; regionPointIndex < regionSizes[regionID]; regionPointIndex ++) {
		int localIndex = regionPointIndex + regionOffsets[regionID];
		int volIndex = local2Global[localIndex];
		int mask = pVolumeElement[volIndex].neighborMask;

		bool bDirichlet = false;
		double scaleS[3] = {1, 1, 1};
		if (mask & NEIGHBOR_BOUNDARY_MASK) {
			if (mask & BOUNDARY_TYPE_DIRICHLET) {// dirichlet
				bDirichlet = true;
			}

			computeScaleS(mask, scaleS);
		}

		// Macro
		defineVolumeNeighbors(volIndex, mask)

		// update values for this point
		updateVolumeStatePointValues(volIndex, t, yinput, statePointValues);

#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
		if (bRegionHasTimeDepdentVariables[regionID]) {
#endif
		// update values for neighbor points
		for (int n = 0; n < 3; n ++) {
			int neighborIndex = volumeNeighbors[n];
			if (neighborIndex < 0) {
				continue;
			}
			updateVolumeStatePointValues(neighborIndex, t, yinput, neighborStatePointValues[n]);
		}
#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
		}
#endif

		int vectorIndexOffset = getVolumeElementVectorOffset(volIndex, regionID);

		// loop through defined variables
		for (int activeVarCount = 0; activeVarCount < regionDefinedVolVariableSizes[regionID]; activeVarCount ++) {
			// I can also increment this, but
			int vectorIndex = vectorIndexOffset + activeVarCount;

			int varIndex = regionDefinedVolVariableIndexes[regionID][activeVarCount];
			VolumeVariable* var = simulation->getVolVariable(varIndex);
			VolumeVarContextExpression* varContext = feature->getVolumeVarContext(var);

			double reactionRate = 0;
			if (bDirichlet && var->isDiffusing()) {// pde dirichlet
				rhs[vectorIndex] = 0;
				if (mask & (NEIGHBOR_XP_BOUNDARY || NEIGHBOR_YP_BOUNDARY || NEIGHBOR_ZP_BOUNDARY)) {
					continue;
				}
			} else {
				reactionRate = varContext->evaluateExpression(REACT_RATE_EXP, statePointValues);
				if (!var->isDiffusing()) {
					rhs[vectorIndex] += reactionRate;
					continue;
				}
			}

			double boundaryCondition = 0;
			if (!bDirichlet && (mask & NEIGHBOR_BOUNDARY_MASK)) {   // neumann boundary condition
				if ((mask & BOUNDARY_TYPE_MASK) == BOUNDARY_TYPE_NEUMANN) {
					boundaryCondition = computeNeumannCondition(feature, varContext, mask, scaleS);
				}
			}

			double Di = 0;
			if (varContext->hasConstantDiffusion()) {
				Di = varContext->evaluateConstantExpression(DIFF_RATE_EXP);
			} else {
#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
				if (varContext->hasXYZOnlyDiffusion()) {
					Di = diffCoeffs[vectorIndex];
				} else {
#endif
				Di = varContext->evaluateExpression(DIFF_RATE_EXP, statePointValues);
#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
				}
#endif
			}

			double Vi_XYZ[3] = {0, 0, 0};
			if (var->isAdvecting()) {
				Vi_XYZ[0] = varContext->evaluateExpression(VELOCITY_X_EXP, statePointValues);
				if (dimension > 1) {
					Vi_XYZ[1] = varContext->evaluateExpression(VELOCITY_Y_EXP, statePointValues);
				}
				if (dimension > 2) {
					Vi_XYZ[2] = varContext->evaluateExpression(VELOCITY_Z_EXP, statePointValues);
				}
			}

			double ypoint = yinput[vectorIndex];
			if (bDirichlet) {
				dirichletPointSetup(volIndex, feature, varContext, mask, volumeNeighbors, ypoint);
			}

			// add diffusion and convection
			for (int n = 0; n < dimension; n ++) {
				int neighborIndex = volumeNeighbors[n];
				int neighborMask = neighborIndex < 0 ? 0 : pVolumeElement[neighborIndex].neighborMask;
				int neighborVectorIndex  = neighborIndex < 0 ? -1 : getVolumeElementVectorOffset(neighborIndex, regionID) + activeVarCount;
				bool bNeighborDirichlet = (neighborMask & BOUNDARY_TYPE_DIRICHLET);

				if (neighborIndex < 0) {
					continue;
				}

				double D = Di;
				if (!varContext->hasConstantDiffusion()) {
					double Dj = 0;
#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
					if (varContext->hasXYZOnlyDiffusion()) {
						Dj = diffCoeffs[neighborVectorIndex];
					} else {
#endif
					Dj = varContext->evaluateExpression(DIFF_RATE_EXP, neighborStatePointValues[n]);
#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
					}
#endif
					D = (Di + Dj < epsilon) ? (0.0) : (2 * Di * Dj/(Di + Dj));
				}

				double yneighbor = yinput[neighborVectorIndex];

				//use the real value if the neighbor is dirichlet point
				if (bNeighborDirichlet) {
					yneighbor = varContext->evaluateExpression(dirichletExpIndexes[n], neighborStatePointValues[n]);
				}

				double diffTerm = D * oneOverH[n];
				double diffAdvectTerm = 0;
				if (var->isAdvecting()) {
					double Vi = Vi_XYZ[n];
					double Vj = varContext->evaluateExpression(velocityExpIndexes[n], neighborStatePointValues[n]);
					double V = 0.5 * (Vi + Vj);
					double advectTerm = -V;

					double Aii = 0, Aij = 0;
					applyAdvectionHybridScheme(diffTerm, advectTerm, Aii, Aij);
					diffAdvectTerm = yneighbor * Aij - ypoint * Aii;
				} else {
					diffAdvectTerm = (yneighbor - ypoint) * diffTerm;
				}
				diffAdvectTerm *= scaleS[n] * oneOverH[n];
				if (!bDirichlet) {
					rhs[vectorIndex] += diffAdvectTerm;
				}
				if (!bNeighborDirichlet) {
					rhs[neighborVectorIndex] -= diffAdvectTerm;
				}
			} // end for n
			if (bDirichlet) {
				rhs[vectorIndex] = 0;
			} else {
				// add boundary conditon
				rhs[vectorIndex] += boundaryCondition;
				if (mask & NEIGHBOR_BOUNDARY_MASK){
					// apply volume scale
					rhs[vectorIndex] *= (mask&VOLUME_MASK);
				}
				// add reaction
				rhs[vectorIndex] += reactionRate;
			}
		} // end for v
	} // end for ri
}

void PetscPdeScheduler::applyMembraneDiffusionReactionOperator(double t, double* yinput, double* rhs) {
	if (simulation->getNumMemVariables() == 0) {
		return;
	}

	for (int mi = 0; mi < mesh->getNumMembraneElements(); mi ++) {
		Membrane* membrane = pMembraneElement[mi].getMembrane();
		for (int v = 0; v < simulation->getNumMemVariables(); v ++) {
			MembraneVariable* var = (MembraneVariable*)simulation->getMemVariable(v);
			// variable is not defined on this membrane
			if (var->getStructure() != NULL && var->getStructure() != membrane) {
				continue;
			}

			int vectorIndex = getMembraneElementVectorOffset(mi) + v;
			MembraneVarContextExpression *varContext = membrane->getMembraneVarContext(var);
			int mask = mesh->getMembraneNeighborMask(mi);

			if (!var->isDiffusing() || !(mask & BOUNDARY_TYPE_DIRICHLET)) {   // boundary and dirichlet
				// update values
				updateMembraneStatePointValues(pMembraneElement[mi], t, yinput, statePointValues);
				double reactionRate = varContext->evaluateExpression(REACT_RATE_EXP, statePointValues);
				// add reaction
				rhs[vectorIndex] += reactionRate;
			}

			if (!var->isDiffusing()) {
				//validateNumber(var->getName(), volIndex, "RHS", rhs[vectorIndex]);
				continue;
			}

			if (mask & BOUNDARY_TYPE_DIRICHLET) {   // boundary and dirichlet
				rhs[vectorIndex] = 0;
				continue;
			}

			double Di = varContext->evaluateExpression(DIFF_RATE_EXP, statePointValues);
			double volume = mesh->getMembraneCoupling()->getValue(mi, mi);
			if (mask & BOUNDARY_TYPE_NEUMANN) { // boundary and neumann
				if (dimension == 2) { // all the flux area is 1
					if (mask & NEIGHBOR_XM_BOUNDARY){
						rhs[vectorIndex] += varContext->evaluateExpression(BOUNDARY_XM_EXP, statePointValues)/volume;
					}
					if (mask & NEIGHBOR_XP_BOUNDARY){
						rhs[vectorIndex] -= varContext->evaluateExpression(BOUNDARY_XP_EXP, statePointValues)/volume;
					}
					if (mask & NEIGHBOR_YM_BOUNDARY){
						rhs[vectorIndex] += varContext->evaluateExpression(BOUNDARY_YM_EXP, statePointValues)/volume;
					}
					if (mask & NEIGHBOR_YP_BOUNDARY){
						rhs[vectorIndex] -= varContext->evaluateExpression(BOUNDARY_YP_EXP, statePointValues)/volume;
					}
				} else if (dimension == 3) {
					double* boundaryFluxArea = mesh->getMembraneFluxArea(mi);

					if (mask & NEIGHBOR_XM_BOUNDARY){
						rhs[vectorIndex] += varContext->evaluateExpression(BOUNDARY_XM_EXP, statePointValues) * boundaryFluxArea[BL_Xm]/volume;
					}
					if (mask & NEIGHBOR_XP_BOUNDARY){
						rhs[vectorIndex] -= varContext->evaluateExpression(BOUNDARY_XP_EXP, statePointValues) * boundaryFluxArea[BL_Xp]/volume;
					}
					if (mask & NEIGHBOR_YM_BOUNDARY){
						rhs[vectorIndex] += varContext->evaluateExpression(BOUNDARY_YM_EXP, statePointValues) * boundaryFluxArea[BL_Ym]/volume;
					}
					if (mask & NEIGHBOR_YP_BOUNDARY){
						rhs[vectorIndex] -= varContext->evaluateExpression(BOUNDARY_YP_EXP, statePointValues) * boundaryFluxArea[BL_Yp]/volume;
					}
					if (mask & NEIGHBOR_ZM_BOUNDARY){
						rhs[vectorIndex] += varContext->evaluateExpression(BOUNDARY_ZM_EXP, statePointValues) * boundaryFluxArea[BL_Zm]/volume;
					}
					if (mask & NEIGHBOR_ZP_BOUNDARY){
						rhs[vectorIndex] -= varContext->evaluateExpression(BOUNDARY_ZP_EXP, statePointValues) * boundaryFluxArea[BL_Zp]/volume;
					}
				}
			}

			int32* membraneNeighbors;
			double* s_over_d;
			int numMembraneNeighbors = mesh->getMembraneCoupling()->getColumns(mi, membraneNeighbors, s_over_d);

			double Aii = 0;
			for (long j = 0; j < numMembraneNeighbors; j ++) {
				int32 neighborIndex = membraneNeighbors[j];
				int neighborVectorIndex = getMembraneElementVectorOffset(neighborIndex) + v;

				updateMembraneStatePointValues(pMembraneElement[neighborIndex], t, yinput, statePointValues);
				double Dj = varContext->evaluateExpression(DIFF_RATE_EXP, statePointValues);
				double D = (Di + Dj < epsilon)?(0.0):(2 * Di * Dj/(Di + Dj));

				double yneighbor = yinput[neighborVectorIndex];
				int neighborMask = mesh->getMembraneNeighborMask(neighborIndex);
				if (neighborMask & BOUNDARY_TYPE_DIRICHLET) {
					if ((neighborMask & NEIGHBOR_XM_BOUNDARY) && (membrane->getXmBoundaryType() == BOUNDARY_VALUE)){
						yneighbor = varContext->evaluateExpression(BOUNDARY_XM_EXP, statePointValues);
					}else if ((neighborMask & NEIGHBOR_XP_BOUNDARY) && (membrane->getXpBoundaryType() == BOUNDARY_VALUE)){
						yneighbor = varContext->evaluateExpression(BOUNDARY_XP_EXP, statePointValues);
					} else if ((neighborMask & NEIGHBOR_YM_BOUNDARY) && (membrane->getYmBoundaryType() == BOUNDARY_VALUE)){
						yneighbor = varContext->evaluateExpression(BOUNDARY_YM_EXP, statePointValues);
					}else if ((neighborMask & NEIGHBOR_YP_BOUNDARY) && (membrane->getYpBoundaryType() == BOUNDARY_VALUE)){
						yneighbor = varContext->evaluateExpression(BOUNDARY_YP_EXP, statePointValues);
					}else if ((neighborMask & NEIGHBOR_ZM_BOUNDARY) && (membrane->getZmBoundaryType() == BOUNDARY_VALUE)){
						yneighbor = varContext->evaluateExpression(BOUNDARY_ZM_EXP, statePointValues);
					}else if ((neighborMask & NEIGHBOR_ZP_BOUNDARY) && (membrane->getZpBoundaryType() == BOUNDARY_VALUE)){
						yneighbor = varContext->evaluateExpression(BOUNDARY_ZP_EXP, statePointValues);
					}
				}
				double Aij = D * s_over_d[j] / volume;
				Aii += Aij;
				rhs[vectorIndex] += yneighbor * Aij;
			}
			rhs[vectorIndex] -=  yinput[vectorIndex] * Aii;
		}
	}
}

void PetscPdeScheduler::applyVolumeRegionReactionOperator(double t, double* yinput, double* rhs) {
	if (simulation->getNumVolRegionVariables() == 0) {
		return;
	}

	for (int r = 0; r < mesh->getNumVolumeRegions(); r ++) {
		VolumeRegion* volRegion = mesh->getVolumeRegion(r);
		Feature* feature = volRegion->getFeature();
		for (int v = 0; v < simulation->getNumVolRegionVariables(); v ++) {
			VolumeRegionVariable* var = simulation->getVolRegionVariable(v);
			VolumeRegionVarContextExpression* volRegionVarContext = feature->getVolumeRegionVarContext(var);
			if (volRegionVarContext == 0) {
				continue;
			}
			int vectorIndex = getVolumeRegionVectorOffset(r) + v;

			updateRegionStatePointValues(r, t, yinput, true, statePointValues);
			rhs[vectorIndex] = volRegionVarContext->evaluateExpression(UNIFORM_RATE_EXP, statePointValues);

			double volume = volRegion->getSize();
			int numElements = volRegion->getNumElements();
			double volumeIntegral = 0.0;
			for(int j = 0; j < numElements; j ++){
				int volIndex = volRegion->getElementIndex(j);
				updateVolumeStatePointValues(volIndex, t, yinput, statePointValues);
				volumeIntegral += volRegionVarContext->evaluateExpression(REACT_RATE_EXP, statePointValues) * mesh->getVolumeOfElement_cu(volIndex);
			}
			rhs[vectorIndex] += volumeIntegral/volume;

			int numMembraneRegions = volRegion->getNumMembraneRegions();
			double surfaceIntegral = 0.0;
			for(int k = 0; k < numMembraneRegions; k ++){
				MembraneRegion *memRegion = volRegion->getMembraneRegion(k);
				numElements = memRegion->getNumElements();
				for(int j = 0; j < numElements; j ++){
					int memIndex = memRegion->getElementIndex(j);
					updateMembraneStatePointValues(pMembraneElement[memIndex], t, yinput, statePointValues);
					surfaceIntegral += volRegionVarContext->evaluateJumpCondition(&pMembraneElement[memIndex], statePointValues) * pMembraneElement[memIndex].area;
				}
			}
			rhs[vectorIndex] += surfaceIntegral/volume;
		}
	}
}

void PetscPdeScheduler::applyMembraneRegionReactionOperator(double t, double* yinput, double* rhs) {
	if (simulation->getNumMemRegionVariables() == 0) {
		return;
	}
	for (int r = 0; r < mesh->getNumMembraneRegions(); r ++) {
		MembraneRegion *memRegion = mesh->getMembraneRegion(r);
		Membrane* membrane = memRegion->getMembrane();
		for (int v = 0; v < simulation->getNumMemRegionVariables(); v ++) {
			MembraneRegionVariable* var = simulation->getMemRegionVariable(v);
			MembraneRegionVarContextExpression * memRegionvarContext = membrane->getMembraneRegionVarContext(var);
			if (memRegionvarContext == 0) {
				continue;
			}
			int vectorIndex = getMembraneRegionVectorOffset(r) + v;

			updateRegionStatePointValues(r, t, yinput, false, statePointValues);
			rhs[vectorIndex] = memRegionvarContext->evaluateExpression(UNIFORM_RATE_EXP, statePointValues);

			double surface = memRegion->getSize();
			long numElements = memRegion->getNumElements();
			double surfaceIntegral = 0.0;
			for(int j = 0; j < numElements; j ++) {
				int memIndex = memRegion->getElementIndex(j);
				updateMembraneStatePointValues(pMembraneElement[memIndex], t, yinput, statePointValues);
				surfaceIntegral += memRegionvarContext->evaluateExpression(REACT_RATE_EXP, statePointValues) * pMembraneElement[memIndex].area;
			}
			rhs[vectorIndex] += surfaceIntegral/surface;
		}
	}
}

void PetscPdeScheduler::applyMembraneFluxOperator(double t, double* yinput, double* rhs) {
	if (simulation->getNumVolPde() == 0) {
		return;
	}

	for (int m = 0; m < mesh->getNumMembraneElements(); m ++) {
		MembraneElement& me = pMembraneElement[m];

		int loRegionID = pVolumeElement[me.vindexFeatureLo].getRegionIndex();
		int hiRegionID = pVolumeElement[me.vindexFeatureHi].getRegionIndex();

		double loVolume = mesh->getVolumeOfElement_cu(me.vindexFeatureLo);
		double hiVolume = mesh->getVolumeOfElement_cu(me.vindexFeatureHi);

		// vector index of inside near and outside near
		int vi2 = getVolumeElementVectorOffset(me.vindexFeatureLo, loRegionID);
		int vi3 = getVolumeElementVectorOffset(me.vindexFeatureHi, hiRegionID);

		updateMembraneStatePointValues(me, t, yinput, statePointValues);

		for (int activeVarCount = 0; activeVarCount < regionDefinedVolVariableSizes[loRegionID]; activeVarCount ++) {
			int varIndex = regionDefinedVolVariableIndexes[loRegionID][activeVarCount];
			VolumeVariable* var = simulation->getVolVariable(varIndex);
			if (!var->isDiffusing()) {
				continue;
			}

			VolumeVarContextExpression* varContext = pVolumeElement[me.vindexFeatureLo].getFeature()->getVolumeVarContext(var);
			double flux = varContext->evaluateJumpCondition(&me, statePointValues);
			rhs[vi2 + activeVarCount] += flux * me.area / loVolume;
			//validateNumber(var->getName(), me.insideIndexNear, "Membrane Flux", rhs[vi2 + activeVarCountInside]);
		}
		for (int activeVarCount = 0; activeVarCount < regionDefinedVolVariableSizes[hiRegionID]; activeVarCount ++) {
			int varIndex = regionDefinedVolVariableIndexes[hiRegionID][activeVarCount];
			VolumeVariable* var = simulation->getVolVariable(varIndex);
			if (!var->isDiffusing()) {
				continue;
			}

			VolumeVarContextExpression* varContext = pVolumeElement[me.vindexFeatureHi].getFeature()->getVolumeVarContext(var);
			double flux = varContext->evaluateJumpCondition(&me, statePointValues);
			rhs[vi3 + activeVarCount] += flux * me.area / hiVolume;
			//validateNumber(var->getName(), me.outsideIndexNear, "Membrane Flux", rhs[vi3 + activeVarCountOutside]);
		}
	}
}

/*
int PetscPdeScheduler::pcSetup_callback(realtype t, N_Vector y, N_Vector fy, booleantype jok, booleantype *jcurPtr, realtype gamma,
                   void *P_data, N_Vector tmp1, N_Vector tmp2, N_Vector tmp3) {
	PetscPdeScheduler* solver = (PetscPdeScheduler*)P_data;
	return solver->pcSetup(t, y, fy, jok, jcurPtr, gamma);
}

int PetscPdeScheduler::pcSolve_callback(realtype t, N_Vector y, N_Vector fy, N_Vector r, N_Vector z, realtype gamma, realtype delta,
                  int lr, void *P_data, N_Vector tmp) {
	PetscPdeScheduler* solver = (PetscPdeScheduler*)P_data;
	return solver->pcSolve(t, y, fy, r, z, gamma, delta, lr);
}
*/
// M approximates I - gamma*J
void PetscPdeScheduler::computeNonZeros(int* nnz) {
	static string METHOD = "PetscPdeScheduler::computeNonZeros";
	cout << "Entry " << METHOD << endl;

	int GENERAL_MAX_NONZERO_PERROW[4] = {0, 3, 5, 7};


	if (simulation->getNumVolVariables() > 0)
	{
		for (int r = 0; r < mesh->getNumVolumeRegions(); r ++) {
			if (regionDefinedVolVariableSizes[r] == 0) {
				continue;
			}

			for (int ri = 0; ri < regionSizes[r]; ri ++) {
				int localIndex = ri + regionOffsets[r];
				int volIndex = local2Global[localIndex];
				int mask = pVolumeElement[volIndex].neighborMask;

				int volumeNeighbors[6] = {
					(dimension < 3 || mask & NEIGHBOR_ZM_MASK) ? -1 : volIndex - Nxy,
					(dimension < 2 || mask & NEIGHBOR_YM_MASK) ? -1 : volIndex - Nx,
					(mask & NEIGHBOR_XM_MASK) ? -1 : volIndex - 1,
					(mask & NEIGHBOR_XP_MASK) ? -1 : volIndex + 1,
					(dimension < 2 || mask & NEIGHBOR_YP_MASK) ? -1 : volIndex + Nx,
					(dimension < 3 || mask & NEIGHBOR_ZP_MASK) ? -1 : volIndex + Nxy
				};

				for (int activeVarCount = 0; activeVarCount < regionDefinedVolVariableSizes[r]; activeVarCount ++)
				{
					int varIndex = regionDefinedVolVariableIndexes[r][activeVarCount];
					VolumeVariable* var = (VolumeVariable*)simulation->getVolVariable(varIndex);
					int vectorIndex = getVolumeElementVectorOffset(volIndex, r) + activeVarCount;

					int colCount = 1;  // diagonal
					if (var->isDiffusing() && !(mask & BOUNDARY_TYPE_DIRICHLET))
					{
						// diffusion and not dirichlet
						for (int n = 0; n < 6; n ++)
						{
							int neighborIndex = volumeNeighbors[n];
							if (neighborIndex >= 0)
							{
								colCount ++;
							}
						} // end for n
					}
					nnz[vectorIndex] = colCount;
				} // end for activeVarCount
			} // end for ri
		} // end for r
	}

	// membrane variable
	if (simulation->getNumMemVariables() > 0)
	{
		for (int mi = 0; mi < mesh->getNumMembraneElements(); mi ++)
		{
			for (int v = 0; v < simulation->getNumMemVariables(); v ++)
			{
				int vectorIndex = getMembraneElementVectorOffset(mi) + v;
        int colCount = 1; // diagonal
				if (simulation->getMemVariable(v)->isDiffusing())
				{
					int32* membraneNeighbors;
					double* s_over_d;
					int numMembraneNeighbors = mesh->getMembraneCoupling()->getColumns(mi, membraneNeighbors, s_over_d);
					colCount += numMembraneNeighbors;
				}
				nnz[vectorIndex] = colCount;
			}
		}
	}
	// volume region variable
	if (simulation->getNumVolRegionVariables() != 0) {
		for (int r = 0; r < mesh->getNumVolumeRegions(); r ++)
		{
			for (int v = 0; v < simulation->getNumVolRegionVariables(); v ++)
			{
				int vectorIndex = getVolumeRegionVectorOffset(r) + v;
				nnz[vectorIndex] = 1;
			}
		}
	}
	// membrane region variable
	if (simulation->getNumMemRegionVariables() != 0) {
		for (int r = 0; r < mesh->getNumMembraneRegions(); r ++) {
			for (int v = 0; v < simulation->getNumMemRegionVariables(); v ++)
			{
				int vectorIndex = getMembraneRegionVectorOffset(r) + v;
				nnz[vectorIndex] = 1;
			}
		}
	}
	cout << "Exit " << METHOD << endl;
}


void PetscPdeScheduler::buildJ_Volume(double t, double* yinput, Mat Pmat) {
//	static string METHOD = "PetscPdeScheduler::buildJ_Volume";
//	cout << "Entry " << METHOD << endl;

	if (simulation->getNumVolVariables() == 0) {
//		cout << METHOD << ", no volume variables, return" << endl;
		return;
	}

	int colCount;
	int32 columns[50];
	double values[50];
	int diagIndex = 0;

	for (int r = 0; r < mesh->getNumVolumeRegions(); r ++) {
		for (int ri = 0; ri < regionSizes[r]; ri ++) {
			int localIndex = ri + regionOffsets[r];
			int volIndex = local2Global[localIndex];
			int mask = pVolumeElement[volIndex].neighborMask;

			int firstPointVolIndex = local2Global[regionOffsets[r]];
			Feature* feature = pVolumeElement[firstPointVolIndex].getFeature();

			for (int activeVarCount = 0; activeVarCount < regionDefinedVolVariableSizes[r]; activeVarCount ++)
			{
				int varIndex = regionDefinedVolVariableIndexes[r][activeVarCount];
				VolumeVariable* var = (VolumeVariable*)simulation->getVolVariable(varIndex);
				int vectorIndex = getVolumeElementVectorOffset(volIndex, r) + activeVarCount;

				if (!var->isDiffusing() || (mask & BOUNDARY_TYPE_DIRICHLET))
				{
					// ODE or dirichlet, diagnoal=1.0;
					double value = 1.0;
					MatSetValues(Pmat, 1, &vectorIndex, 1, &vectorIndex, &value, INSERT_VALUES);
					continue;
				}

				VolumeVarContextExpression* varContext = feature->getVolumeVarContext(var);
				double Di = 0;
				if (varContext->hasConstantDiffusion()) {
					Di = varContext->evaluateConstantExpression(DIFF_RATE_EXP);
				}

				assert(varContext);

				double lambdaX = oneOverH[0] * oneOverH[0];
				double lambdaY = oneOverH[1] * oneOverH[1];
				double lambdaZ = oneOverH[2] * oneOverH[2];
				double lambdaAreaX = oneOverH[0];
				double lambdaAreaY = oneOverH[1];
				double lambdaAreaZ = oneOverH[2];

				// update values
				if (!varContext->hasConstantDiffusion()) {
#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
					if (varContext->hasXYZOnlyDiffusion()) {
						Di = diffCoeffs[vectorIndex];
					} else {
#endif
					updateVolumeStatePointValues(volIndex, t, yinput, statePointValues);
					Di = varContext->evaluateExpression(DIFF_RATE_EXP, statePointValues);
#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
					}
#endif
				}

				double Aii = 0;
				if (mask & NEIGHBOR_BOUNDARY_MASK){   // boundary
					if (mask & NEIGHBOR_X_BOUNDARY_MASK){
						lambdaX *= 2.0;
						lambdaAreaX *= 2.0;
					}
					if (mask & NEIGHBOR_Y_BOUNDARY_MASK){
						lambdaY *= 2.0;
						lambdaAreaY *= 2.0;
					}
					if (mask & NEIGHBOR_Z_BOUNDARY_MASK){
						lambdaZ *= 2.0;
						lambdaAreaZ *= 2.0;
					}
				}

				int volumeNeighbors[6] = {
					(dimension < 3 || mask & NEIGHBOR_ZM_MASK) ? -1 : volIndex - Nxy,
					(dimension < 2 || mask & NEIGHBOR_YM_MASK) ? -1 : volIndex - Nx,
					(mask & NEIGHBOR_XM_MASK) ? -1 : volIndex - 1,
					(mask & NEIGHBOR_XP_MASK) ? -1 : volIndex + 1,
					(dimension < 2 || mask & NEIGHBOR_YP_MASK) ? -1 : volIndex + Nx,
					(dimension < 3 || mask & NEIGHBOR_ZP_MASK) ? -1 : volIndex + Nxy
				};

				double neighborLambdas[6] = {lambdaZ, lambdaY, lambdaX, lambdaX, lambdaY, lambdaZ};
				double neighborLambdaAreas[6] = {lambdaAreaZ, lambdaAreaY, lambdaAreaX, lambdaAreaX, lambdaAreaY, lambdaAreaZ};

//				int numColumns = M->getColumns(vectorIndex, columns, values);
				diagIndex = -1;
				colCount = 0;
				for (int n = 0; n < 6; n ++) {
					int neighborIndex = volumeNeighbors[n];
					if (neighborIndex < 0) {
						continue;
					}
					int neighborVectorIndex = getVolumeElementVectorOffset(neighborIndex, r) + activeVarCount;
					double lambda = neighborLambdas[n];

					double D = Di;
					if (!varContext->hasConstantDiffusion()) {
						double Dj = 0;
#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
						if (varContext->hasXYZOnlyDiffusion()) {
							Dj = diffCoeffs[neighborVectorIndex];
						} else {
#endif
						updateVolumeStatePointValues(neighborIndex, t, yinput, statePointValues);
						Dj = varContext->evaluateExpression(DIFF_RATE_EXP, statePointValues);
#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
						}
#endif
						D = (Di + Dj < epsilon) ? (0.0) : (2 * Di * Dj/(Di + Dj));
					}

					if (diagIndex == -1 && vectorIndex < neighborVectorIndex)
					{
						diagIndex = colCount;
						++ colCount;
					}
					double Aij = 0;
					Aij = D * lambda;
					Aii += Aij;
					columns[colCount] = neighborVectorIndex;
					values[colCount] = Aij;
					++ colCount;
				} // end for n
//				assert(colCount == numColumns);
				if (diagIndex == -1)
				{
					diagIndex = colCount;
					++ colCount;
				}
				columns[diagIndex] = vectorIndex;
				values[diagIndex] = -Aii;
				MatSetValues(Pmat, 1, &vectorIndex, colCount, columns, values, INSERT_VALUES);
//				M->setDiag(vectorIndex, 1.0 + gamma * Aii);
			} // end for v
		} // end for ri
	} // end for r
//	cout << "Exit " << METHOD << endl;
}

void PetscPdeScheduler::buildJ_Membrane(double t, double* yinput, Mat Pmat) {
//	static string METHOD = "PetscPdeScheduler::buildJ_Membrane";
//	cout << "Entry " << METHOD << endl;

	if (simulation->getNumMemVariables() == 0) {
//		cout << METHOD << ", no membrane variables, return" << endl;
		return;
	}

	for (int mi = 0; mi < mesh->getNumMembraneElements(); mi ++) {
		Membrane* membrane = pMembraneElement[mi].getMembrane();
		for (int v = 0; v < simulation->getNumMemVariables(); v ++) {
			MembraneVariable* var = (MembraneVariable*)simulation->getMemVariable(v);
			// variable is not defined on this membrane
			if (var->getStructure() != NULL && var->getStructure() != membrane) {
				continue;
			}

			if (!var->isDiffusing()) {
				continue;
			}

			int mask = mesh->getMembraneNeighborMask(mi);
			if ((mask & NEIGHBOR_BOUNDARY_MASK) && (mask & BOUNDARY_TYPE_DIRICHLET)) {   // boundary and dirichlet
				continue;
			}

			int vectorIndex = getMembraneElementVectorOffset(mi) + v;

			int32 columns[50];
			double values[50];
//			int numColumns = M->getColumns(vectorIndex, columns, values);

			Membrane* membrane = pMembraneElement[mi].getMembrane();
			MembraneVarContextExpression *varContext = membrane->getMembraneVarContext(var);

			updateMembraneStatePointValues(pMembraneElement[mi], t, yinput, statePointValues);
			double Di = varContext->evaluateExpression(DIFF_RATE_EXP, statePointValues);

			int32* membraneNeighbors;
			double* s_over_d;
			int numMembraneNeighbors = mesh->getMembraneCoupling()->getColumns(mi, membraneNeighbors, s_over_d);
//			assert(numColumns == numMembraneNeighbors);
			double volume = mesh->getMembraneCoupling()->getValue(mi, mi);
			double Aii = 0;
			int colCount = 0;
			int diagIndex = -1;
			for (long j = 0; j < numMembraneNeighbors; j ++)
			{
				int32 neighborIndex = membraneNeighbors[j];
				int neighborVectorIndex = getMembraneElementVectorOffset(neighborIndex) + v;

				updateMembraneStatePointValues(pMembraneElement[neighborIndex], t, yinput, statePointValues);
				double Dj = varContext->evaluateExpression(DIFF_RATE_EXP, statePointValues);
				double D = (Di + Dj < epsilon)?(0.0):(2 * Di * Dj/(Di + Dj));

				if (diagIndex == -1 && vectorIndex < neighborVectorIndex)
				{
					diagIndex = colCount;
					++ colCount;
				}
				columns[colCount] = neighborVectorIndex;
				double Aij = D * s_over_d[j] / volume;
				values[colCount] = Aij;
				Aii += Aij;
				++ colCount;
			}
			if (diagIndex == -1)
			{
				diagIndex = colCount;
				++ colCount;
			}
			columns[diagIndex] = vectorIndex;
			values[diagIndex] = -Aii;
			MatSetValues(Pmat, 1, &vectorIndex, colCount, columns, values, INSERT_VALUES);
//			M->setDiag(vectorIndex, 1.0 + gamma * Aii);
		}
	}
//	cout << "Exit " << METHOD << endl;
}

int PetscPdeScheduler::getVolumeElementVectorOffset(int volIndex, int regionID) {
	return volVectorOffsets[regionID] + (global2Local[volIndex] - regionOffsets[regionID]) * regionDefinedVolVariableSizes[regionID];
}

int PetscPdeScheduler::getMembraneElementVectorOffset(int meindex) {
	return memVectorOffset + meindex * simulation->getNumMemVariables();
}

int PetscPdeScheduler::getVolumeRegionVectorOffset(int regionID) {
	return volRegionVectorOffset + regionID * simulation->getNumVolRegionVariables();
}

int PetscPdeScheduler::getMembraneRegionVectorOffset(int regionID) {
	return memRegionVectorOffset + regionID * simulation->getNumMemRegionVariables();
}

void PetscPdeScheduler::updateVolumeStatePointValues(int volIndex, double t, double* yinput, double* values) {
	values[simulation->symbolIndexOfT()] = t;

	WorldCoord wc = mesh->getVolumeWorldCoord(volIndex);

	int xyzIndex = simulation->symbolIndexOfXyz();
	values[xyzIndex ++] = wc.x;
	values[xyzIndex ++] = wc.y;
	values[xyzIndex ++] = wc.z;

	if (yinput == 0) {
		return;
	}

	simulation->populateParticleVariableValuesNew(values, true, volIndex);
	simulation->populateRegionSizeVariableValuesNew(values, true, pVolumeElement[volIndex].getRegionIndex());
	simulation->populateFieldValuesNew(values, volIndex);
	simulation->populateRandomValuesNew(values, volIndex);

	int regionID = pVolumeElement[volIndex].getRegionIndex();
	if (simulation->getNumVolVariables() > 0) {
		int vi = getVolumeElementVectorOffset(volIndex, regionID);
		for (int activeVarCount = 0; activeVarCount < regionDefinedVolVariableSizes[regionID]; activeVarCount ++) {
			int varIndex = regionDefinedVolVariableIndexes[regionID][activeVarCount];
			values[simulation->symbolIndexOfVolVar() + varIndex * numSymbolsPerVolVar] = yinput[vi + activeVarCount];
		}
	}

	if (simulation->getNumVolRegionVariables() > 0) {
		// fill in volume region variable values
		int volumeRegionElementVectorOffset = getVolumeRegionVectorOffset(regionID);
		int numVolRegionVariables = simulation->getNumVolRegionVariables();
		for (int varIndex = 0; varIndex < numVolRegionVariables; varIndex ++) {
			values[simulation->symbolIndexOfVolRegionVar() + varIndex * numSymbolsPerVolVar] = yinput[volumeRegionElementVectorOffset + varIndex];
		}
	}
	// if field data is used in expressions other than initial conditions, we
	// need to fill the double array the values from field data from value proxy
	//
	// we also need to fill parameter values for parameter optimization.
}

void PetscPdeScheduler::updateMembraneStatePointValues(MembraneElement& me, double t, double* yinput, double* values) {

	WorldCoord wc = mesh->getMembraneWorldCoord(&me);

	values[simulation->symbolIndexOfT()] = t;
	int xyzIndex = simulation->symbolIndexOfXyz();
	values[xyzIndex ++] = wc.x;
	values[xyzIndex ++] = wc.y;
	values[xyzIndex ++] = wc.z;

	if (yinput == 0) {
		return;
	}

	simulation->populateParticleVariableValuesNew(values, false, me.index);
	simulation->populateRegionSizeVariableValuesNew(values, false, me.getRegionIndex());
	simulation->populateFieldValuesNew(values, me.index);
	simulation->populateRandomValuesNew(values, me.index);

	if (simulation->getNumVolVariables() > 0) {
		// fill in INSIDE and OUTSIDE values
		int loRegionID = pVolumeElement[me.vindexFeatureLo].getRegionIndex();
		int hiRegionID = pVolumeElement[me.vindexFeatureHi].getRegionIndex();

		int vi1 = me.vindexFeatureLoFar < 0 ? -1 : getVolumeElementVectorOffset(me.vindexFeatureLoFar, loRegionID);
		int vi2 = getVolumeElementVectorOffset(me.vindexFeatureLo, loRegionID);
		int vi3 = getVolumeElementVectorOffset(me.vindexFeatureHi, hiRegionID);
		int vi4 = me.vindexFeatureHiFar < 0 ? -1 : getVolumeElementVectorOffset(me.vindexFeatureHiFar, hiRegionID);

		for (int activeVarCount = 0; activeVarCount < regionDefinedVolVariableSizes[loRegionID]; activeVarCount ++) {
			int varIndex = regionDefinedVolVariableIndexes[loRegionID][activeVarCount];

			int iin = simulation->symbolIndexOfVolVar() + varIndex * numSymbolsPerVolVar + 1 + pVolumeElement[me.vindexFeatureLo].getFeature()->getIndex();
			if (vi1 < 0) {
				values[iin] = yinput[vi2 + activeVarCount];
			} else {
				values[iin] = interp_coeff[0] * yinput[vi2 + activeVarCount] + interp_coeff[1] * yinput[vi1 + activeVarCount];
			}
		}

		for (int activeVarCount = 0; activeVarCount < regionDefinedVolVariableSizes[hiRegionID]; activeVarCount ++) {
			int varIndex = regionDefinedVolVariableIndexes[hiRegionID][activeVarCount];
			int iout = simulation->symbolIndexOfVolVar() + varIndex * numSymbolsPerVolVar + 1 + pVolumeElement[me.vindexFeatureHi].getFeature()->getIndex();
			if (vi4 < 0) {
				values[iout] = yinput[vi3 + activeVarCount];
			} else {
				values[iout] = interp_coeff[0] * yinput[vi3 + activeVarCount] + interp_coeff[1] * yinput[vi4 + activeVarCount];
			}
		}
	}

	if (simulation->getNumMemVariables() > 0) {
		// fill in membrane variable values
		int membraneElementVectorOffset = getMembraneElementVectorOffset(me.index);
		memcpy(values + simulation->symbolIndexOfMemVar(), yinput + membraneElementVectorOffset, simulation->getNumMemVariables() * sizeof(double));
	}

	if (simulation->getNumMemRegionVariables() > 0) {
		// fill in membrane region variable values
		int membraneRegionElementVectorOffset = getMembraneRegionVectorOffset(me.getRegionIndex());
		memcpy(values + simulation->symbolIndexOfMemRegionVar(), yinput + membraneRegionElementVectorOffset, simulation->getNumMemRegionVariables() * sizeof(double));
	}
}

void PetscPdeScheduler::updateRegionStatePointValues(int regionID, double t, double* yinput, bool bVolumeRegion, double* values) {

	values[simulation->symbolIndexOfT()] = t;

	if (yinput == 0) {
		return;
	}

	simulation->populateRegionSizeVariableValuesNew(values, bVolumeRegion, regionID);

	if (bVolumeRegion) {
		// fill in volume region variable values
		int volumeRegionElementVectorOffset = getVolumeRegionVectorOffset(regionID);
		memcpy(values + simulation->symbolIndexOfVolRegionVar(), yinput + volumeRegionElementVectorOffset, simulation->getNumVolRegionVariables() * sizeof(double));
	} else {
		// fill in membrane region variable values
		int membraneRegionElementVectorOffset = getMembraneRegionVectorOffset(regionID);
		memcpy(values + simulation->symbolIndexOfMemRegionVar(), yinput + membraneRegionElementVectorOffset, simulation->getNumMemRegionVariables() * sizeof(double));

		MembraneRegion *mr = mesh->getMembraneRegion(regionID);

		int numVolRegionVariables = simulation->getNumVolRegionVariables();
		VolumeRegion *vr1 = mr->getVolumeRegion1();
		VolumeRegion *vr2 = mr->getVolumeRegion2();
		for (int varIndex = 0; varIndex < numVolRegionVariables; varIndex ++) {
			int offset = simulation->symbolIndexOfVolRegionVar() + varIndex * numSymbolsPerVolVar + 1;
			{
				int volumeRegionElementVectorOffset = getVolumeRegionVectorOffset(vr1->getIndex());
				values[offset + vr1->getFeature()->getIndex()] = yinput[volumeRegionElementVectorOffset + varIndex];
			}
			{

				int volumeRegionElementVectorOffset = getVolumeRegionVectorOffset(vr2->getIndex());
				values[offset + vr2->getFeature()->getIndex()] = yinput[volumeRegionElementVectorOffset + varIndex];
			}
		}
	}
}

PetscErrorCode PetscPdeScheduler::ts_buildPmat(PetscReal t, Vec Y, Mat Pmat)
{
	PetscErrorCode ierr;
	double *yinput;
	VecGetArrayRead(Y, (const double**)&yinput);

	buildJ_Volume(t, yinput, Pmat);
	buildJ_Membrane(t, yinput, Pmat);

	// volume region variables, diagonal = 1.0;
	if (simulation->getNumVolRegionVariables() != 0) {
		for (int r = 0; r < mesh->getNumVolumeRegions(); r ++)
		{
			for (int v = 0; v < simulation->getNumVolRegionVariables(); v ++)
			{
				int vectorIndex = getVolumeRegionVectorOffset(r) + v;
				double value = 1.0;
				MatSetValues(Pmat, 1, &vectorIndex, 1, &vectorIndex, &value, INSERT_VALUES);
			}
		}
	}
	// membrane region variables, diagonal = 1.0;
	if (simulation->getNumMemRegionVariables() != 0) {
		for (int r = 0; r < mesh->getNumMembraneRegions(); r ++) {
			for (int v = 0; v < simulation->getNumMemRegionVariables(); v ++)
			{
				int vectorIndex = getMembraneRegionVectorOffset(r) + v;
				double value = 1.0;
				MatSetValues(Pmat, 1, &vectorIndex, 1, &vectorIndex, &value, INSERT_VALUES);
			}
		}
	}

	VecRestoreArrayRead(Y, (const PetscScalar**)&yinput);
	return 0;
}

PetscErrorCode PetscPdeScheduler::TS_Jacobian_Function(TS ts, PetscReal t, Vec Y, Mat Amat, Mat Pmat, void *ctx)
{
	PetscPdeScheduler* solver = (PetscPdeScheduler*)ctx;
	return solver->ts_jacobian(ts, t, Y, Amat, Pmat);
}

PetscErrorCode PetscPdeScheduler::ts_jacobian(TS ts, PetscReal t, Vec Y, Mat Amat, Mat Pmat)
{
	ts_buildPmat(t, Y, Pmat);

	MatAssemblyBegin(Pmat, MAT_FINAL_ASSEMBLY);
	MatAssemblyEnd(Pmat, MAT_FINAL_ASSEMBLY);
	if (Pmat != Amat)
	{
		MatAssemblyBegin(Amat, MAT_FINAL_ASSEMBLY);
	  MatAssemblyEnd(Amat, MAT_FINAL_ASSEMBLY);
	}

	static int count = 0;
	++ count;
//	char filename[256];
//	char prefix[30] = "Pmat_TS";
//	sprintf(filename, "%s_%d.m", prefix, count);
//	PetscViewer viewer;
//	PetscViewerASCIIOpen(PETSC_COMM_WORLD, filename, &viewer);
//	PetscViewerPushFormat(viewer, PETSC_VIEWER_ASCII_MATLAB);
//	string name = prefix;
//	PetscObjectSetName((PetscObject)Pmat, name.c_str());
//	MatView(Pmat, viewer);
//	PetscViewerDestroy(&viewer);

	cout << " # ts_jacobian called " << count << endl;
	return 0;
}

PetscErrorCode PetscPdeScheduler::SNES_Function(SNES snes, Vec Y, Vec F, void *ctx)
{
	PetscPdeScheduler* solver = (PetscPdeScheduler*)ctx;
	return solver->snes_function(snes, Y, F);
}

PetscErrorCode PetscPdeScheduler::snes_function(SNES snes, Vec Y, Vec F)
{
	PetscErrorCode ierr;
	double *yinput;
	double *f;
	VecGetArrayRead(Y, (const double**)&yinput);
	VecGetArray(F, (double**)&f);

	rhs(currentTime, yinput, f);

	VecRestoreArrayRead(Y, (const PetscScalar**)&yinput);
	VecRestoreArray(F, (PetscScalar**)&f);

	VecAXPBY(F, 1.0 , -internalDt, Y);

	return 0;
}

void PetscPdeScheduler::buildPmat_Volume(double t, double gamma, double* yinput, Mat Pmat) {
//	static string METHOD = "PetscPdeScheduler::buildJ_Volume";
//	cout << "Entry " << METHOD << endl;

	if (simulation->getNumVolVariables() == 0) {
//		cout << METHOD << ", no volume variables, return" << endl;
		return;
	}

	int colCount;
	int32 columns[50];
	double values[50];
	int diagIndex = 0;

	for (int r = 0; r < mesh->getNumVolumeRegions(); r ++) {
		for (int ri = 0; ri < regionSizes[r]; ri ++) {
			int localIndex = ri + regionOffsets[r];
			int volIndex = local2Global[localIndex];
			int mask = pVolumeElement[volIndex].neighborMask;

			int firstPointVolIndex = local2Global[regionOffsets[r]];
			Feature* feature = pVolumeElement[firstPointVolIndex].getFeature();

			for (int activeVarCount = 0; activeVarCount < regionDefinedVolVariableSizes[r]; activeVarCount ++)
			{
				int varIndex = regionDefinedVolVariableIndexes[r][activeVarCount];
				VolumeVariable* var = (VolumeVariable*)simulation->getVolVariable(varIndex);
				int vectorIndex = getVolumeElementVectorOffset(volIndex, r) + activeVarCount;

				if (!var->isDiffusing() || (mask & BOUNDARY_TYPE_DIRICHLET))
				{
					// ODE or dirichlet, diagnoal=1.0;
					double value = 1.0;
					MatSetValues(Pmat, 1, &vectorIndex, 1, &vectorIndex, &value, INSERT_VALUES);
					continue;
				}

				VolumeVarContextExpression* varContext = feature->getVolumeVarContext(var);
				double Di = 0;
				if (varContext->hasConstantDiffusion()) {
					Di = varContext->evaluateConstantExpression(DIFF_RATE_EXP);
				}

				assert(varContext);

				double lambdaX = oneOverH[0] * oneOverH[0];
				double lambdaY = oneOverH[1] * oneOverH[1];
				double lambdaZ = oneOverH[2] * oneOverH[2];
				double lambdaAreaX = oneOverH[0];
				double lambdaAreaY = oneOverH[1];
				double lambdaAreaZ = oneOverH[2];

				// update values
				if (!varContext->hasConstantDiffusion()) {
#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
					if (varContext->hasXYZOnlyDiffusion()) {
						Di = diffCoeffs[vectorIndex];
					} else {
#endif
					updateVolumeStatePointValues(volIndex, t, yinput, statePointValues);
					Di = varContext->evaluateExpression(DIFF_RATE_EXP, statePointValues);
#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
					}
#endif
				}

				double Aii = 0;
				if (mask & NEIGHBOR_BOUNDARY_MASK){   // boundary
					if (mask & NEIGHBOR_X_BOUNDARY_MASK){
						lambdaX *= 2.0;
						lambdaAreaX *= 2.0;
					}
					if (mask & NEIGHBOR_Y_BOUNDARY_MASK){
						lambdaY *= 2.0;
						lambdaAreaY *= 2.0;
					}
					if (mask & NEIGHBOR_Z_BOUNDARY_MASK){
						lambdaZ *= 2.0;
						lambdaAreaZ *= 2.0;
					}
				}

				int volumeNeighbors[6] = {
					(dimension < 3 || mask & NEIGHBOR_ZM_MASK) ? -1 : volIndex - Nxy,
					(dimension < 2 || mask & NEIGHBOR_YM_MASK) ? -1 : volIndex - Nx,
					(mask & NEIGHBOR_XM_MASK) ? -1 : volIndex - 1,
					(mask & NEIGHBOR_XP_MASK) ? -1 : volIndex + 1,
					(dimension < 2 || mask & NEIGHBOR_YP_MASK) ? -1 : volIndex + Nx,
					(dimension < 3 || mask & NEIGHBOR_ZP_MASK) ? -1 : volIndex + Nxy
				};

				double neighborLambdas[6] = {lambdaZ, lambdaY, lambdaX, lambdaX, lambdaY, lambdaZ};

//				int numColumns = M->getColumns(vectorIndex, columns, values);
				diagIndex = -1;
				colCount = 0;
				for (int n = 0; n < 6; n ++) {
					int neighborIndex = volumeNeighbors[n];
					if (neighborIndex < 0) {
						continue;
					}
					int neighborVectorIndex = getVolumeElementVectorOffset(neighborIndex, r) + activeVarCount;
					double lambda = neighborLambdas[n];

					double D = Di;
					if (!varContext->hasConstantDiffusion()) {
						double Dj = 0;
#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
						if (varContext->hasXYZOnlyDiffusion()) {
							Dj = diffCoeffs[neighborVectorIndex];
						} else {
#endif
						updateVolumeStatePointValues(neighborIndex, t, yinput, statePointValues);
						Dj = varContext->evaluateExpression(DIFF_RATE_EXP, statePointValues);
#ifdef PRECOMPUTE_DIFFUSION_COEFFICIENT
						}
#endif
						D = (Di + Dj < epsilon) ? (0.0) : (2 * Di * Dj/(Di + Dj));
					}

					if (diagIndex == -1 && vectorIndex < neighborVectorIndex)
					{
						diagIndex = colCount;
						++ colCount;
					}
					double Aij = 0;
					Aij = D * lambda;
					Aii += Aij;
					columns[colCount] = neighborVectorIndex;
					values[colCount] = -gamma * Aij;
					++ colCount;
				} // end for n
//				assert(colCount == numColumns);
				if (diagIndex == -1)
				{
					diagIndex = colCount;
					++ colCount;
				}
				columns[diagIndex] = vectorIndex;
				values[diagIndex] = 1.0 + gamma * Aii;
				MatSetValues(Pmat, 1, &vectorIndex, colCount, columns, values, INSERT_VALUES);
//				M->setDiag(vectorIndex, 1.0 + gamma * Aii);
			} // end for v
		} // end for ri
	} // end for r
//	cout << "Exit " << METHOD << endl;
}

void PetscPdeScheduler::buildPmat_Membrane(double t, double gamma, double* yinput, Mat Pmat) {
//	static string METHOD = "PetscPdeScheduler::buildJ_Membrane";
//	cout << "Entry " << METHOD << endl;

	if (simulation->getNumMemVariables() == 0) {
//		cout << METHOD << ", no membrane variables, return" << endl;
		return;
	}

	for (int mi = 0; mi < mesh->getNumMembraneElements(); mi ++) {
		Membrane* membrane = pMembraneElement[mi].getMembrane();
		for (int v = 0; v < simulation->getNumMemVariables(); v ++) {
			MembraneVariable* var = (MembraneVariable*)simulation->getMemVariable(v);

			int vectorIndex = getMembraneElementVectorOffset(mi) + v;
			int mask = mesh->getMembraneNeighborMask(mi);
			// variable is not defined on this membrane OR ode OR boundary and dirichlet, set diagonal to be 1.
			if ( ((var->getStructure() != NULL) && (var->getStructure() != membrane))
					|| !var->isDiffusing()
					|| ((mask & NEIGHBOR_BOUNDARY_MASK) && (mask & BOUNDARY_TYPE_DIRICHLET))) {
				double value = 1.0;
				MatSetValues(Pmat, 1, &vectorIndex, 1, &vectorIndex, &value, INSERT_VALUES);
				continue;
			}

			int32 columns[50];
			double values[50];
//			int numColumns = M->getColumns(vectorIndex, columns, values);

			Membrane* membrane = pMembraneElement[mi].getMembrane();
			MembraneVarContextExpression *varContext = membrane->getMembraneVarContext(var);

			updateMembraneStatePointValues(pMembraneElement[mi], t, yinput, statePointValues);
			double Di = varContext->evaluateExpression(DIFF_RATE_EXP, statePointValues);

			int32* membraneNeighbors;
			double* s_over_d;
			int numMembraneNeighbors = mesh->getMembraneCoupling()->getColumns(mi, membraneNeighbors, s_over_d);
//			assert(numColumns == numMembraneNeighbors);
			double volume = mesh->getMembraneCoupling()->getValue(mi, mi);
			double Aii = 0;
			int colCount = 0;
			int diagIndex = -1;
			for (long j = 0; j < numMembraneNeighbors; j ++)
			{
				int32 neighborIndex = membraneNeighbors[j];
				int neighborVectorIndex = getMembraneElementVectorOffset(neighborIndex) + v;

				updateMembraneStatePointValues(pMembraneElement[neighborIndex], t, yinput, statePointValues);
				double Dj = varContext->evaluateExpression(DIFF_RATE_EXP, statePointValues);
				double D = (Di + Dj < epsilon)?(0.0):(2 * Di * Dj/(Di + Dj));

				if (diagIndex == -1 && vectorIndex < neighborVectorIndex)
				{
					diagIndex = colCount;
					++ colCount;
				}
				columns[colCount] = neighborVectorIndex;
				double Aij = D * s_over_d[j] / volume;
				values[colCount] = -gamma * Aij;
				Aii += Aij;
				++ colCount;
			}
			if (diagIndex == -1)
			{
				diagIndex = colCount;
				++ colCount;
			}
			columns[diagIndex] = vectorIndex;
			values[diagIndex] = 1.0 + gamma * Aii;
			MatSetValues(Pmat, 1, &vectorIndex, colCount, columns, values, INSERT_VALUES);
//			M->setDiag(vectorIndex, 1.0 + gamma * Aii);
		}
	}
//	cout << "Exit " << METHOD << endl;
}

PetscErrorCode PetscPdeScheduler::snes_buildPmat(Vec Y, Mat Pmat)
{
	double *yinput;
	VecGetArrayRead(Y, (const double**)&yinput);

	buildPmat_Volume(currentTime, internalDt, yinput, Pmat);
	buildPmat_Membrane(currentTime, internalDt, yinput, Pmat);

	// volume region variables, diagonal = 1.0;
	if (simulation->getNumVolRegionVariables() != 0) {
		for (int r = 0; r < mesh->getNumVolumeRegions(); r ++)
		{
			for (int v = 0; v < simulation->getNumVolRegionVariables(); v ++)
			{
				int vectorIndex = getVolumeRegionVectorOffset(r) + v;
				double value = 1.0;
				MatSetValues(Pmat, 1, &vectorIndex, 1, &vectorIndex, &value, INSERT_VALUES);
			}
		}
	}
	// membrane region variables, diagonal = 1.0;
	if (simulation->getNumMemRegionVariables() != 0) {
		for (int r = 0; r < mesh->getNumMembraneRegions(); r ++) {
			for (int v = 0; v < simulation->getNumMemRegionVariables(); v ++)
			{
				int vectorIndex = getMembraneRegionVectorOffset(r) + v;
				double value = 1.0;
				MatSetValues(Pmat, 1, &vectorIndex, 1, &vectorIndex, &value, INSERT_VALUES);
			}
		}
	}

	VecRestoreArrayRead(Y, (const PetscScalar**)&yinput);
}

PetscErrorCode PetscPdeScheduler::SNES_Jacobian_Function(SNES snes, Vec Y, Mat Amat, Mat Pmat, void *ctx)
{
	PetscPdeScheduler* solver = (PetscPdeScheduler*)ctx;
	return solver->snes_jacobian(snes, Y, Amat, Pmat);
}

PetscErrorCode PetscPdeScheduler::snes_jacobian(SNES snes, Vec Y, Mat Amat, Mat Pmat)
{
	snes_buildPmat(Y, Pmat);

	MatAssemblyBegin(Pmat, MAT_FINAL_ASSEMBLY);
	MatAssemblyEnd(Pmat, MAT_FINAL_ASSEMBLY);
	if (Pmat != Amat)
	{
		MatAssemblyBegin(Amat, MAT_FINAL_ASSEMBLY);
		MatAssemblyEnd(Amat, MAT_FINAL_ASSEMBLY);
	}

	static int count = 0;
	++ count;
	/*
	char prefix[30] = "Pmat_SNES";
	char filename[256];
	sprintf(filename, "%s_%d.m", prefix, count);
	PetscViewer viewer;
	PetscViewerASCIIOpen(PETSC_COMM_WORLD, filename, &viewer);
	PetscViewerPushFormat(viewer, PETSC_VIEWER_ASCII_MATLAB);
	string name = prefix;
	PetscObjectSetName((PetscObject)Pmat, name.c_str());
	MatView(Pmat, viewer);
	PetscViewerDestroy(&viewer);
*/
	//cout << " # snes_jacobian called " << count << endl;
	return 0;
}

#endif
