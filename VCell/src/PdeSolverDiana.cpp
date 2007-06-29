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
#include <VCELL/TriDiagMatrix.h>
#include <VCELL/SparseMatrix.h>
#include <VCELL/VolumeVariable.h>
#include <VCELL/EqnBuilder.h>
#include <VCELL/Simulation.h>
#include <VCELL/VolumeRegion.h>
#include <VCELL/Solver.h>
#include <VCELL/VarContext.h>
#include <VCELL/Feature.h>
#include <VCELL/PdeSolverDiana.h>
#include <VCELL/FVUtils.h>
#include <VCELL/SimTool.h>
#include <VCELL/CartesianMesh.h>
#include <iomanip>
using namespace std;


#define UNDER_MASK	1
#define SOUTH_MASK	2
#define WEST_MASK	4
#define EAST_MASK	8
#define NORTH_MASK	16
#define ABOVE_MASK	32

//#define SHOW_MATRIX
//#define SHOW_IPARM

PdeSolverDiana::PdeSolverDiana(VolumeVariable *Var, 
			       CartesianMesh  *mesh,
			       int             arg_symmflg,
			       int             numSolveRegions, 
			       int            *solveRegions,
			       bool         AbTimeDependent)
: StructuredPDESolver(Var, mesh, AbTimeDependent)
{
	pMatx = NULL;
	Smat  = NULL;
	arraySize = 0;
	symmflg = arg_symmflg;
	enableRetry = true;

	long given_numUnknowns;
	if (numSolveRegions > 0) {
		// If we are solving in one or more subregions - reordered
		solveWholeMesh = false;

		// Compute number of unknowns (union of regions to solve)
		given_numUnknowns = 0;
		int j;
		for (j = 0; j<numSolveRegions; j++) {
			int rID = solveRegions[j];
			VolumeRegion *regionToSolve = mesh->getVolumeRegion(rID);
			long numInRegion = regionToSolve->getNumElements();
			given_numUnknowns += numInRegion;
			//       printf(" regionToSolve %ld num of elements %ld \n", rID, numInRegion);
		}
		arraySize= given_numUnknowns;
		// Define arrays Gridmap and Neworder
		Gridmap = new long[given_numUnknowns];
		Neworder = new long[sizeX*sizeY*sizeZ];

		//changes from Diana April 24, 2003
		// initialize Neworder to an invalid index value
		for (long in=0; in<sizeX*sizeY*sizeZ ; in++) {
			Neworder[in] = -1;
		}
		//end of changes

		long index = 0;
		for (j = 0; j<numSolveRegions; j++) {
			int rID = solveRegions[j];
			VolumeRegion *regionToSolve = mesh->getVolumeRegion(rID);
			long numInRegion = regionToSolve->getNumElements();

			for ( long indexInRegion = 0; indexInRegion < numInRegion; indexInRegion++) {
				// find gridindex associated with index
				long gridindex = regionToSolve->getIndex(indexInRegion);
				Gridmap[index] = gridindex  ;         // maps gridpoint in new order into natural order 
				Neworder[gridindex] = index ;         // maps natural order gridpoint into new order
				index++;
			}
		}
	}else {
		// If we are solving for the solution on the whole mesh in natural ordering:
		solveWholeMesh = true;
		// size of solution array
		given_numUnknowns = mesh->getNumVolumeElements();
	}

	initMatrix(given_numUnknowns);   
}

void PdeSolverDiana::initMatrix(long given_numUnknowns, int additional) {

	switch (mesh->getDimension()){
		case 1:
			delete pMatx;
			pMatx = new TriDiagMatrix();
			pMatx->init(sizeX);
			u_array = new double[sizeX];
			break;
		case 2:                                // 2D case
		{
			long nonz2D, size;
			long Nwork;

			delete Smat;
			Smat = new SparseMatrix();       // define a new sparse matrix
			//   symmetric storage, upper triangle
			//   symmflg = 1;    
			//   general (un-symmetric storage)
			//   symmflg = 0;

			// size of work array for pcg
			// NOTE: this estimate might not work for larger fill-in parameters (3 or 4)
			// NOTE IMPORTANT Nwork is different for general storage !!!
			if (symmflg != 0) {
				Nwork = given_numUnknowns*9;
			//       printf("2D, symmetric storage , Nwork = %ld\n",Nwork);
			}else {
				Nwork = given_numUnknowns*30;
			//       printf("2D, general storage   , Nwork = %ld\n",Nwork);
			}
			Nwork += additional;
			size = given_numUnknowns;
			if (solveWholeMesh) {
				if (symmflg != 0) {  // symmetric half-storage
					nonz2D = 3*size-sizeX-sizeY+1;
				}else {             // general storage
					nonz2D = 5*size-sizeX-sizeY+2;
				}
			}else {
				if (size != sizeX*sizeY) {
					throw "PdeSolverDiana::initMatrix() 2D: inconsistent size";
				}
				// find upper bound for number of non-zero coefficients (can't compute exactly in general case)
				if (symmflg != 0) {  // symmetric half-storage
					nonz2D = 3*size;
				}else {             // general storage
					nonz2D = 5*size;
				}
			}
			Smat->init(size, nonz2D, Nwork, symmflg);
		}
		break;
		case 3:                                // 3D case
		{
			delete Smat;
			Smat = new SparseMatrix();       // define a new sparse matrix
			long nonz3D, size;
			long Nwork;
			// int symmflg = 0;              //    0: general storage
			// int symmflg = 1;              //    1: symmetric storage, upper triangle
			// size of work array for pcg
			// NOTE: this estimate might not work for larger fill-in parameters (3 or 4)
			// NOTE IMPORTANT Nwork is different for general storage !!!
			if (symmflg != 0) {
				Nwork = (long)(given_numUnknowns*11.5+2208);
			}else {
				Nwork = given_numUnknowns*40;
			}
			Nwork += additional;
			size = given_numUnknowns;
			if (solveWholeMesh) {
				if (symmflg != 0) {  // symmetric half-storage
					long nonz2D = 3*sizeX*sizeY-sizeX-sizeY+1;
					nonz3D = nonz2D + (sizeZ-1)*(nonz2D+sizeX*sizeY) ;
				}else {             // general storage
					long nonz2D = 5*sizeX*sizeY-sizeX-sizeY+2;
					nonz3D = nonz2D + (sizeZ-1)*(nonz2D+2*sizeX*sizeY) + 1;
				}
			}else {
				if (size > sizeX*sizeY*sizeZ) {
					throw "PdeSolverDiana::initMatrix() 3D : inconsistent size";
				}
				if (symmflg != 0) {  // symmetric half-storage
					nonz3D = 4*size;
				}else {             // general storage
					nonz3D = 7*size;
				}
			}
			Smat->init(size, nonz3D, Nwork, symmflg);
		}
		break;
		default:
			throw "PdeSolverDiana::initMatrix() : more than 3D is not supposed";
			break;
	}
}


PdeSolverDiana::~PdeSolverDiana()
{
#ifndef GCC
	PDESolver::~PDESolver();
#endif   
	if (pMatx) delete pMatx;
	if (Smat) delete Smat;
}

bool PdeSolverDiana::solveEqn(double dT_sec, 
				 int volumeIndexStart, int volumeIndexSize, 
				 int membraneIndexStart, int membraneIndexSize, bool bFirstTime)
{
	switch (mesh->getDimension()){
		case 1:{                       // 1D case: solve tridiangonal system
			return solveEqn1D(bFirstTime);
			//break;
		}
		case 2:                        // 2D and 3D cases: use a PCG solver
		case 3:{                       // 
			//bool reuseflg = Smat->getReuse();           // flag for re-computing matrix or re-using factorinzation
			long numUnknowns  = Smat->getSize();           // size of solution vector

			// I - Define GlobalScale -  Scaling factor for linear equations (matrix and RHS) in order to improve
			//                       convergence of PCG method 
			//        NOTE: I use the diag element at an arbitrary interior point.
			//        In order to do this right, I should use different values in different regions (?)
			//        (i.e. not one global value), but one has to be careful not to affect symmetry.
			//        should be done in EqnBuilder
			//						
			if (bFirstTime || isTimeDependent()){	                     // First Pass: compute GlobalScale once
				// choose first non-Dirichlet interior grid index and find diag coefficient for that point
				//
				DiscreteEqn tempEqu;
				long index = 0;
				
				if (solveWholeMesh) {
					while (IsDirichlet(index) && index < numUnknowns-1) {
						index++;
					}
					tempEqu = eqn[index];
				}else {
					while (IsDirichlet(Gridmap[index]) && index < numUnknowns-1) {
						index++;
					}
					tempEqu = eqn[Gridmap[index]];
				}
						
				// Define scale
				GlobalScale = 1./(tempEqu.Ap+tempEqu.Ap0);
				// test with no scale : GlobalScale = 1.;
			}

			double scale = GlobalScale;
			
			//
			// II - Form matrix and RHS in sparse format (for PCG)
			//
			if (solveWholeMesh) {
				//      printf(" solve whole mesh size %ld, Globalscale= %lg \n", numUnknowns,scale);
				if (!PCGFormSys(scale, bFirstTime)) return false;       
			}else {
				//  printf(" solve part reordrd mesh (region) size %ld, Globalscale= %lg \n", numUnknowns,scale);
				if (!PCGFormSys_Reorder(scale, bFirstTime)) return false;       
			}

			//
			// III - Solve system using PCG solver
			//
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
							initMatrix(Smat->getSize(), IParm[53]);
							delete[] IParm;
							return solveEqn(dT_sec, volumeIndexStart, volumeIndexSize, membraneIndexStart, membraneIndexSize, true);
						} else {
							handlePCGExceptions(IParm[50], IParm[53]);
							delete[] IParm;
							return false;
						}
						break;
					default:				
						printf("PdeSolverDiana::PCGSolve, error solving system\n");
						handlePCGExceptions(IParm[50], IParm[53]);
						delete[] IParm;
						return false;
				}
			}
			delete[] IParm;

			return true;
			//break;
		}
		return false;
	}
	//The execution should never get here
	return false;			//Added by Daniel Lucio
}

bool PdeSolverDiana::solveEqn1D(bool bFirstTime)
{
	DiscreteEqn *tempEquPtr;
	double *old;
	long x;

	//fprintf(stdout,"\n");
	//showEqn(stdout);
	if (isTimeDependent() || bFirstTime){
		tempEquPtr = eqn;
		double *pA = pMatx->getA();
		double *pB = pMatx->getB();
		double *pC = pMatx->getC();
		for (x=0;x<sizeX;x++){
			*pA++ = - tempEquPtr->Ax_minus;
			*pB++ = tempEquPtr->Ap + tempEquPtr->Ap0;
			*pC++ = - tempEquPtr->Ax_plus; 
			tempEquPtr++;
		}
	}

	double *pR = pMatx->getR();
	tempEquPtr = eqn;
	old = var->getOld();
	for (x=0;x<sizeX;x++){
		*pR++ = tempEquPtr->B + (*old++)*tempEquPtr->Ap0;
		tempEquPtr++;
	}

#ifdef SHOW_MATRIX
	cout << "---------Time " << theApplication->getSimulation()->getTime_sec() << "-----------------------" << endl;
	cout << endl << "PDESolverDiana BEFORE" << endl;
	pMatx->show();
	fflush(stdout);
#endif

	if (!pMatx->Solve()){
		printf("PdeSolverDiana::solveEqn1D(), error solving matrix\n");
		return false;
	}
	pMatx->getU(u_array,sizeX);
	((VolumeVariable*)var)->setLineX(0, 0, u_array, sizeX);

#ifdef SHOW_MATRIX
	cout << endl << "PDESolverDiana AFTER" << endl;
	pMatx->show();
	fflush(stdout);
#endif

	return true;
}


// -------------
// NEW CODE: PCG 
// -------------

// ----------------------------------------------------------------
bool PdeSolverDiana::PCGFormSys(double scale, bool bFirstTime)
// ----------------------------------------------------------------
{
	//
	//  Form the matrix MA and the RHS for linear system in general or
	//  symmetric sparse format to call pcgpak.  
	//  MA is only formed/updated if reuse is false
	//
	DiscreteEqn tempEqu, NeighEqu;
	long gridindex;
	double valueRHS,RHSscale;

	long   XY, zend, zoffset;
	long   Ntot  = Smat->getSize();            // size of solution vector
	double *pRHS = Smat->getRhs();             // right hand side
	int    symmetricflg  = Smat->getSymflg();  // general or symmetric storage format

	RHSscale = scale;
	// printf(" RHSscale =  %lg  \n",RHSscale);

	//
	//                   Set offset values for 2D and 3D cases
	//

	if (sizeZ > 1) {                        // 3D case
		XY   = sizeX*sizeY;
		zend = sizeZ;
	}else{                                  //2D
		XY = 0;
		zend = 1;
	}

	//
	//  Form RHS - Steps:
	//  Step 1: Initialize RHS at boundary points. Make a list of Neumann points with a Dirichlet neighbor (mixed boundary)
	//  Step 2: Define RHS for interior points and update RHS with boundary conditions (at points near a Dirichlet boundary)
	//  Step 3: Update RHS for mixed boundary type points, using the list created in step 1
	//  Form Matrix -
	//  Step 4: Define elements of the matrix in sparse form

	//  
	long  z;

	//  1. Initialize RHS for boundary points
	for (z=0;z<zend;z++){ 
		zoffset = z*XY;
		for (long y=0;y<sizeY;y++){
			for (long x=0;x<sizeX;x++){
				if ( ((sizeZ>1) &&                                            // 3D
						(x*y*z==0 || (x-sizeX+1)*(y-sizeY+1)*(z-sizeZ+1)==0)) ||    // boundary point, or:
						((sizeZ==1) &&                                           // 2D
						(x*y==0 || (x-sizeX+1)*(y-sizeY+1)==0))) {                 // boundary point,
					gridindex = x + y*sizeX + zoffset;
					tempEqu = eqn[gridindex];
					if (IsDirichlet(gridindex)) {                             // Dirichlet boundary - set rhs
						valueRHS = tempEqu.B ;
					}else {                                                   // Neumann boundary - set rhs
						valueRHS = tempEqu.B + var->getOld(gridindex)*tempEqu.Ap0 ;
						valueRHS *= RHSscale;                                   // scaled rhs value:
						if (bFirstTime){	                                      // First Pass: find mixed boundary type points
							//                                                       and define mixedBoundary structure
							MixedBoundaryInfo mixedPoint;
							mixedPoint.mixedBoundaryMask = 0 ;
							mixedPoint.index = -1;
							
							if (sizeZ>1 && (x==0 || x==sizeX-1 || y == 0 || y == sizeY-1) ) {
								if ( (z>0) && (IsDirichlet(gridindex-XY ))) {
									mixedPoint.mixedBoundaryMask |= UNDER_MASK ;
									mixedPoint.index = gridindex;
								}
								if ( (z<sizeZ-1) && (IsDirichlet(gridindex+XY))) {
									mixedPoint.mixedBoundaryMask |= ABOVE_MASK ;
									mixedPoint.index = gridindex;
								}
							}
							if (x==0 || x==sizeX-1 || z == 0 || z == sizeZ-1) {
								if ( y>0 && IsDirichlet(gridindex-sizeX)) {
									mixedPoint.mixedBoundaryMask |= SOUTH_MASK ;
									mixedPoint.index = gridindex;
								}
								if ( y < sizeY-1 && IsDirichlet(gridindex+sizeX)) {
									mixedPoint.mixedBoundaryMask |= NORTH_MASK ;
									mixedPoint.index = gridindex;
								}
							}
							if (y==0 || y==sizeY-1 || z == 0 || z == sizeZ-1) {
								if ( x>0 && IsDirichlet(gridindex-1)) {
									mixedPoint.mixedBoundaryMask |= WEST_MASK ;
									mixedPoint.index = gridindex;
								}
								if ( x<sizeX-1 && IsDirichlet(gridindex+1)) {
									mixedPoint.mixedBoundaryMask |= EAST_MASK ;
									mixedPoint.index = gridindex;
								}
							}
				
							if (mixedPoint.index>=0) {
								mixedBPoints.push_back(mixedPoint);
							}
						}
					}
					Smat->setRhs(gridindex, valueRHS);                        // set value in Sparse Matrix         
				} 
			} // end x loop
		} // end y loop
	} // end z loop
	      
  //
  // 2.                            Define RHS for interior points, update with boundary conditions
  // 
	if (sizeZ > 1) {                        // 3D case
		XY   = sizeX*sizeY;
		zend = sizeZ;
	}else{                                  //2D
		XY = 0;
		zend = 3;
	}
	for (z=1;z<zend-1;z++){ 
		zoffset = z*XY;
		for (long y=1;y<sizeY-1;y++){
			for (long x=1;x<sizeX-1;x++){
				gridindex = x + y*sizeX + zoffset;

				tempEqu = eqn[gridindex];
				valueRHS = tempEqu.B + var->getOld(gridindex)*tempEqu.Ap0;
				//
				//                        Update RHS at points next to a boundary 
				//                                   when a neighbor is a Dirichlet point
				//                                             
				if (x==1)    {                              // West wall
					// Dirichlet bc?
					if (IsDirichlet(gridindex-1)) {
						NeighEqu = eqn[gridindex-1];
						valueRHS += NeighEqu.B*tempEqu.Ax_minus;
					}
				} 
				if (x==sizeX-2) {                           // East wall
					if (IsDirichlet(gridindex+1)) {
						NeighEqu = eqn[gridindex+1];
						valueRHS += NeighEqu.B*tempEqu.Ax_plus;
					}
				}
				if (y==1)    {                              // South wall
					if (IsDirichlet(gridindex-sizeX)) {
						NeighEqu = eqn[gridindex-sizeX];
						valueRHS += NeighEqu.B*tempEqu.Ay_minus;
					}
				}
				if (y==sizeY-2) {                           // North wall
					if (IsDirichlet(gridindex+sizeX)) {
						NeighEqu = eqn[gridindex+sizeX];
						valueRHS += NeighEqu.B*tempEqu.Ay_plus;
					}
				}
				if (sizeZ > 1) {              //3D case
					if (z==1)    {                            // Floor
						if (IsDirichlet(gridindex-XY)) {
							NeighEqu = eqn[gridindex-XY];
							valueRHS += NeighEqu.B*tempEqu.Az_minus;
						}
					}
					if (z==sizeZ-2) {                         // Ceiling
						if (IsDirichlet(gridindex+XY)) {
							NeighEqu = eqn[gridindex+XY];
							valueRHS += NeighEqu.B*tempEqu.Az_plus;
						}
					}
				}
				// Scale and set RHS for this point
				valueRHS *= RHSscale;
				Smat->setRhs(gridindex, valueRHS); 
			} // end x loop
		} // end y loop
	} // end z loop

  //
  // 3.                                   Update RHS for Mixed boundary type points
  //
	if (!mixedBPoints.empty()) {
		RHS_Mixed_Boundary_Update(RHSscale);
	}
	//
	// 4.                                   Define elements of the matrix in sparse form
	//

	if (bFirstTime || isTimeDependent()){
		if( symmetricflg != 0) {
			if(!PCGFormSymmetricMA(scale)) {
				printf(" PCGFormSymmetricMA Error forming matrix \n");
				return false;
			}
		}else {
			if(!PCGFormGeneralMA(scale)) {
				printf(" PCGFormGeneralMA Error forming matrix \n");
				return false;
			}
		}
	}

	//  Diana's NOTE (Jan 15, 2002 ) Why does it say the following - NOT true (I believe written by Daniel)?
	//The execution should never get here
	//  printf(" I am here - End of PCGFormSys \n ");
	return true;			//Added by DALO
}


// -------------------------------------------------------------------------
bool PdeSolverDiana::PCGFormSys_Reorder(double scale, bool bFirstTime)
// -------------------------------------------------------------------------
{
	//
	//  Form the matrix MA and the RHS for linear system in symmetric sparse format
	//  to call pcgpak.  MA is only formed/updated if reuse is false
	//
	DiscreteEqn tempEqu, NeighEqu;
	long gridindex,index;
	double valueRHS,RHSscale;

	long   XY;
	long   numUnknowns  = Smat->getSize();     // size of solution vector
	int    symmetricflg  = Smat->getSymflg(); 
	//  printf( " PCGFormSys_reorder symmetry flag: %ld", symmetricflg);

	double *pRHS = Smat->getRhs();             // right hand side

	RHSscale = scale;
	// printf(" RHSscale =  %lg  \n",RHSscale);

	//
	//                   Set offset values for 2D and 3D cases
	//

	if (sizeZ > 1) {                        // 3D case
		XY   = sizeX*sizeY;
	}else{                                  //2D
		XY = 0;
	}

	//
	//  Form RHS - Steps:
	//  Step 1: Initialize RHS at boundary points. Make a list of Neumann points with a Dirichlet neighbor (mixed boundary)
	//  Step 2: Define RHS for interior points and update RHS with boundary conditions (at points near a Dirichlet boundary)
	//  Step 3: Update RHS for mixed boundary type points, using the list created in step 1
	//  Form Matrix -
	//  Step 4: Define elements of the matrix in sparse form
	//  
	//  1. Initialize RHS for boundary points
	// Loop through list of index

	for (index = 0; index < numUnknowns ; index++) {
		// =======================================================
		//  determine the (x,y,z) coordinates in cartesian grid
		//  I need to write a function which computes the x,y,z
		//  coordinates corresponding to the index point in this reordering
		//  (x,y,z) = gridmap(index); 

		// test case: natural ordering (standard grid ordering)
		gridindex = Gridmap[index];
		//    gridindex = index;

		// in this case, the x,y,z coordinates are
		//    WorldCoord wc = mesh->getVolumeWorldCoord(index);
		//    int x = wc.x;
		//    int y = wc.y;
		//    int z = wc.z;
		//    gridindex = x + y*sizeX + z*XY;
		// can also be computed as:

		long z;
		if (XY == 0) {
			z = 0;
		}else {
			z = gridindex/XY;
		}
		long tt = gridindex - z*XY ;
		long y = tt/sizeX;
		long x = tt - y*sizeX;
		// =======================================================


		if ( ((sizeZ>1) &&                                            // 3D
				(x*y*z==0 || (x-sizeX+1)*(y-sizeY+1)*(z-sizeZ+1)==0)) ||    // boundary point, or:
				((sizeZ==1) &&                                           // 2D
				(x*y==0 || (x-sizeX+1)*(y-sizeY+1)==0))) {                 // boundary point,
			tempEqu = eqn[gridindex];
			if (IsDirichlet(gridindex)) {                             // Dirichlet boundary - set rhs
				valueRHS = tempEqu.B ;
			}else {                                                   // Neumann boundary - set rhs
				valueRHS = tempEqu.B + var->getOld(gridindex)*tempEqu.Ap0 ;
				valueRHS *= RHSscale;                                   // scaled rhs value:
				
				if (bFirstTime){	                                      // First Pass: find mixed boundary type points
					//                                                       and define mixedBoundary structure
					MixedBoundaryInfo mixedPoint;
					mixedPoint.mixedBoundaryMask = 0 ;
					mixedPoint.index = -1;
					  
					if (sizeZ>1 && (x==0 || x==sizeX-1 || y == 0 || y == sizeY-1) ) {
						if ( (z>0) && (IsDirichlet(gridindex-XY ))) {
							mixedPoint.mixedBoundaryMask |= UNDER_MASK ;
							mixedPoint.index = gridindex;
						}
						if ( (z<sizeZ-1) && (IsDirichlet(gridindex+XY))) {
							mixedPoint.mixedBoundaryMask |= ABOVE_MASK ;
							mixedPoint.index = gridindex;
						}
					}
					if (x==0 || x==sizeX-1 || z == 0 || z == sizeZ-1) {
						if ( y>0 && IsDirichlet(gridindex-sizeX)) {
							mixedPoint.mixedBoundaryMask |= SOUTH_MASK ;
							mixedPoint.index = gridindex;
						}
						if ( y < sizeY-1 && IsDirichlet(gridindex+sizeX)) {
							mixedPoint.mixedBoundaryMask |= NORTH_MASK ;
							mixedPoint.index = gridindex;
						}
					}
					if (y==0 || y==sizeY-1 || z == 0 || z == sizeZ-1) {
						if ( x>0 && IsDirichlet(gridindex-1)) {
							mixedPoint.mixedBoundaryMask |= WEST_MASK ;
							mixedPoint.index = gridindex;
						}
						if ( x<sizeX-1 && IsDirichlet(gridindex+1)) {
							mixedPoint.mixedBoundaryMask |= EAST_MASK ;
							mixedPoint.index = gridindex;
						}
					}
					  
					if (mixedPoint.index>=0) {
						mixedBPoints.push_back(mixedPoint);
					}
				}
			}
			// NOTE: index is not necessarily equal to gridindex
			Smat->setRhs(index, valueRHS);                        // set value in Sparse Matrix         
			//
		}else {                                                 // interior point
			//
			// 2.                            Define RHS for interior points, update with boundary conditions
			// 
			// =======================================================

			tempEqu = eqn[gridindex];
			valueRHS = tempEqu.B + var->getOld(gridindex)*tempEqu.Ap0;
			//
			//                        Update RHS at points next to a boundary 
			//                                   when a neighbor is a Dirichlet point
			//                                             
			if (x==1)    {                              // West wall
				// Dirichlet bc?
				if (IsDirichlet(gridindex-1)) {
					NeighEqu = eqn[gridindex-1];
					valueRHS += NeighEqu.B*tempEqu.Ax_minus;
				}
			} 
			if (x==sizeX-2) {                           // East wall
				if (IsDirichlet(gridindex+1)) {
					NeighEqu = eqn[gridindex+1];
					valueRHS += NeighEqu.B*tempEqu.Ax_plus;
				}
			}
			if (y==1)    {                              // South wall
				if (IsDirichlet(gridindex-sizeX)) {
					NeighEqu = eqn[gridindex-sizeX];
					valueRHS += NeighEqu.B*tempEqu.Ay_minus;
				}
			}
			if (y==sizeY-2) {                           // North wall
				if (IsDirichlet(gridindex+sizeX)) {
					NeighEqu = eqn[gridindex+sizeX];
					valueRHS += NeighEqu.B*tempEqu.Ay_plus;
				}
			}
			if (sizeZ > 1) {              //3D case
				if (z==1)    {                            // Floor
					if (IsDirichlet(gridindex-XY)) {
						NeighEqu = eqn[gridindex-XY];
						valueRHS += NeighEqu.B*tempEqu.Az_minus;
					}
				}
				if (z==sizeZ-2) {                         // Ceiling
					if (IsDirichlet(gridindex+XY)) {
						NeighEqu = eqn[gridindex+XY];
						valueRHS += NeighEqu.B*tempEqu.Az_plus;
					}
				}
			}
			// Scale and set RHS for this point
			valueRHS *= RHSscale;
			Smat->setRhs(index, valueRHS); 
		}
	}

	//
	// 3.                                   Update RHS for Mixed boundary type points
	//
	if (!mixedBPoints.empty()) {
		RHS_Mixed_Boundary_Update(RHSscale);
	}

	//    int     symmetricflg  = Smat->getSymflg(); 

	// 4.                                   Define elements of the matrix in sparse form
	//
	//  printf( " PCGFormSys_reorder Form matrix, flag: %ld \n", symmetricflg);

	if (bFirstTime || isTimeDependent()){
		if( symmetricflg != 0) {
			if(!PCGFormSymmetricMA_Reorder(scale)) {
				printf(" PCGFormSymmetricMA_Reorder Error forming matrix \n");
				return false;
			}
		}else {
			if(!PCGFormGeneralMA_Reorder(scale)) {
				printf(" PCGFormGeneralMA_Reorder Error forming matrix \n");
				return false;
			}
		}
	}
	// Diana's NOTE (Jan 15, 2002 ) Why does it say the following - NOT true (I believe written by Daniel)?
	//The execution should never get here
	//  printf(" I am here - End of PCGFormSys_Reorder \n ");
	return true;			//Added by DALO
}


// --------------------------------------------------
bool PdeSolverDiana::PCGFormSymmetricMA(double scale)
// --------------------------------------------------
{
	//  Form the matrix MA in symmetric sparse form

	long   XY, xstart, xend;

	// Set offset values for 2D and 3D cases
	XY   = 0;
	if (sizeZ > 1) {           // 3D case
		XY   = sizeX*sizeY;
	}

	// Form matrix in sparse symmetric form (compact storage for pcg)
	INT32  *ija  = Smat->getIja();;     // array of pointers for pcg format
	long   Ntot  = Smat->getSize();     // size of system
	// in this case, we solve for the whole box:
	ASSERTION(sizeX*sizeY*sizeZ==Ntot);
	  
	DiscreteEqn tempEqu;
	double valueA;
	  
	long   j, zoffset, gridwest, OffDiagCounter, valueIJA;

	Smat->setija(0, Ntot+2);    // points to first off-diagonal element
	//
	// By default, define diagonal elements for all points to be 1 (Dirichlet case)
	//  
	for (long i=0;i<Ntot;i++){
		Smat->setAm(i, 1.);
	}
	// initialize j (off-diagonal locator)
	j = Ntot+1;
	//
	//  Define diagonal and off-diagonal elements of the matrix
	for (long z=0;z<sizeZ;z++){
		zoffset = z*XY; 
		for (long y=0;y<sizeY;y++){
			gridwest = y*sizeX + zoffset;
			if (IsDirichlet(gridwest)) {   // case x=0 (west wall) Dirichlet case: zero off-diagonals
				Smat->setSameija(gridwest,gridwest);  //i.e. no off-diagonal elements
				xstart = 1;
			}else {
				xstart = 0;
			}
			long indx = gridwest+sizeX-1 ; // case x=N-1 (east wall) 
			if (IsDirichlet(indx)) {   // Dirichlet case:
				xend = sizeX-1;
			}else {
				xend = sizeX;
			}
			for (long x=xstart;x<xend;x++){ // interior points

				indx = gridwest+x ; 
				tempEqu = eqn[indx];

				if ( sizeZ>1 &&                           // (3D case)
					(z == 0 || z==sizeZ-1) &&             // floor and ceiling 
					(IsDirichlet(indx)))       {   // Dirichlet case: zero off-diagonals
					Smat->setSameija(indx,indx);
				}else if ( ( (y == 0) || (y==sizeY-1) ) &&  // (south and north walls) 
					(IsDirichlet(indx) ))  {    // Dirichlet case: zero off-diagonals
					Smat->setSameija(indx,indx);
				}else {                                   // interior gridpoint or Neumann point: 
					valueA     = tempEqu.Ap0 + tempEqu.Ap;  // compute diagonal element
					Smat->setAm(indx, scale*valueA);   // scale and define diagonal element
					  
					OffDiagCounter = 0 ;
					  
					if ((x != sizeX-1) && (!IsDirichlet(indx+1))) {  //if east neighbor not Dirichlet
						valueA = -tempEqu.Ax_plus;
						//	    check for non-zero off-diagonal values (e.g. near membrane)
						if (valueA != 0.0){  
							//increase off-diagonal counter
							OffDiagCounter++;
							valueIJA = indx+2; //
							Smat->setija(j, valueIJA);
							Smat->setAm(j, scale*valueA);
							j++;
						}
					}

					if (y != sizeY-1) {
						if ( !IsDirichlet(indx+sizeX)) {  //if north neighbor not Dirichlet
							valueA = -tempEqu.Ay_plus; 
							//	    check for non-zero off-diagonal values
							if (valueA != 0.0){//increase off-diagonal counter
								OffDiagCounter++;
								valueIJA = indx+sizeX+1; // 
								Smat->setija(j, valueIJA);
								Smat->setAm(j, scale*valueA);
								j++;
							}
						}
					}

					if (z != sizeZ-1) {

						if ( !IsDirichlet(indx+XY)) {  //if top neighbor not Dirichlet
							valueA = -tempEqu.Az_plus; 
							//	    check for non-zero off-diagonal values
							if (valueA != 0.0){//increase off-diagonal counter
								OffDiagCounter++;
								valueIJA = indx+XY+1; // 
								Smat->setija(j, valueIJA);
								Smat->setAm(j, scale*valueA);
								j++;
							}
						}
					}
					// determine pointer to off-diagonal elements
					valueIJA = ija[indx] + OffDiagCounter;
					Smat->setija(indx+1,valueIJA); 
				}  
		
			} // end x loop
			indx = gridwest+sizeX-1 ; // case x=N-1 (east wall) 
			if (IsDirichlet(indx)) {   // Dirichlet case: zero off-diagonals
				Smat->setSameija(indx,indx);
			}

		} // end y loop
	} // end z loop
	  
	// Show sparse system:
	//  printf(" Sparse matrix (symmetric) and rhs \n");
	// Smat->show(1,5);
	//  Smat->show(XY+sizeX+1,XY+sizeX+3);
	  
	return true;
}


// --------------------------------------------------
bool PdeSolverDiana::PCGFormGeneralMA(double scale)
// --------------------------------------------------
{
  //  Form the matrix MA in general sparse form

	long   XY, xstart, xend;

	// Set offset values for 2D and 3D cases
	XY   = 0;
	if (sizeZ > 1) {           // 3D case
		XY   = sizeX*sizeY;
	}

	// Form matrix in sparse general form (not necessarily symmetric)
	INT32  *ija  = Smat->getIja();;     // array of pointers for pcg format
	long   Ntot  = Smat->getSize();     // size of system
	// in this case, we solve for the whole box:
	ASSERTION(sizeX*sizeY*sizeZ==Ntot);

	DiscreteEqn tempEqu;
	double valueA;

	long   j, zoffset, gridwest, OffDiagCounter, valueIJA;

	Smat->setija(0, Ntot+2);    // points to first off-diagonal element
	//
	// By default, define diagonal elements for all points to be 1 (Dirichlet case)
	//  
	for (long i=0;i<Ntot;i++){
		Smat->setAm(i, 1.);
	}
	// initialize j (off-diagonal locator)
	j = Ntot+1;
	//
	//  Define diagonal and off-diagonal elements of the matrix
	for (long z=0;z<sizeZ;z++){
		zoffset = z*XY; 
		for (long y=0;y<sizeY;y++){
			gridwest = y*sizeX + zoffset;
			if (IsDirichlet(gridwest)) {   // case x=0 (west wall) Dirichlet case: zero off-diagonals
				Smat->setSameija(gridwest,gridwest);  //i.e. no off-diagonal elements
				xstart = 1;
			}else {
				xstart = 0;
			}
			long indx = gridwest+sizeX-1 ; // case x=N-1 (east wall) 
			if (IsDirichlet(indx)) {   // Dirichlet case:
				xend = sizeX-1;
			}else {
				xend = sizeX;
			}
			for (long x=xstart;x<xend;x++){ // interior points

				indx = gridwest+x ; 
				tempEqu = eqn[indx];

				if ( sizeZ>1 &&                           // (3D case)
						(z == 0 || z==sizeZ-1) &&             // floor and ceiling 
						(IsDirichlet(indx)))       {   // Dirichlet case: zero off-diagonals
					Smat->setSameija(indx,indx);
				}else if ( ( (y == 0) || (y==sizeY-1) ) &&  // (south and north walls) 
					(IsDirichlet(indx) ))  {    // Dirichlet case: zero off-diagonals
					Smat->setSameija(indx,indx);
				}else {                                   
					// COMPUTE DIAGONAL ELEMENT for interior gridpoints or Neumann points
					valueA     = tempEqu.Ap0 + tempEqu.Ap;  
					Smat->setAm(indx, scale*valueA);   // scale and define diagonal element
				  
					// COMPUTE OFF-DIAGONAL ELEMENTS
					OffDiagCounter = 0 ;
		  
					// (i) lowest diag (3D)
					if (sizeZ > 1) {
						if ((z > 1) || ( z == 1 && !IsDirichlet(indx-XY))) {  
							valueA = -tempEqu.Az_minus;
						//	    check for non-zero off-diagonal values
							if (valueA != 0.0){
								OffDiagCounter++; //increase off-diagonal counter
								valueIJA = indx-XY+1;
								Smat->setija(j, valueIJA);
								Smat->setAm(j, scale*valueA);
								j++;
							}
						}
					}
					// (ii) middle lower diag
					if ((y > 1) || ( y == 1 && !IsDirichlet(indx-sizeX))) {  
						valueA = -tempEqu.Ay_minus; 
						if (valueA != 0.0){
							OffDiagCounter++; //increase off-diagonal counter
							valueIJA = indx-sizeX+1; // 
							Smat->setija(j, valueIJA);
							Smat->setAm(j, scale*valueA);
							j++;
						}
					}
					// (iii) lower diag
					if ((x > 1) || ( x == 1 && !IsDirichlet(indx-1))) {  
						valueA = -tempEqu.Ax_minus;	      
						if (valueA != 0.0){  
							//increase off-diagonal counter
							OffDiagCounter++;
							valueIJA = indx; //  indx-1+1; (fortran index)
							Smat->setija(j, valueIJA);
							//
							Smat->setAm(j, scale*valueA);
							j++;
						}
					}
					// (iv) upper diag
					if ((x < sizeX-2) || ( x == sizeX-2 && !IsDirichlet(indx+1))) {  
						valueA = -tempEqu.Ax_plus;
						//	    check for non-zero off-diagonal values (e.g. near membrane)
						if (valueA != 0.0){  
							//increase off-diagonal counter
							OffDiagCounter++;
							valueIJA = indx+2; //
							Smat->setija(j, valueIJA);
							Smat->setAm(j, scale*valueA);
							j++;
						}
					}

					// (v) middle upper diag
					if ((y < sizeY-2) || ( y == sizeY-2 && !IsDirichlet(indx+sizeX))) {  
						valueA = -tempEqu.Ay_plus; 
						//	    check for non-zero off-diagonal values
						if (valueA != 0.0){//increase off-diagonal counter
							OffDiagCounter++;
							valueIJA = indx+sizeX+1; // 
							Smat->setija(j, valueIJA);
							Smat->setAm(j, scale*valueA);
							j++;
						}
					}
					// (vi) upper-most diag (3D)
					if (sizeZ > 1) {
						if ((z < sizeZ-2) || ( z == sizeZ-2 && !IsDirichlet(indx+XY))) {  
							valueA = -tempEqu.Az_plus; 
							//	    check for non-zero off-diagonal values
							if (valueA != 0.0){//increase off-diagonal counter
								OffDiagCounter++;
								valueIJA = indx+XY+1; // 
								Smat->setija(j, valueIJA);
								Smat->setAm(j, scale*valueA);
								j++;
							}
						}
					}
					// determine pointer to off-diagonal elements
					valueIJA = ija[indx] + OffDiagCounter;
					Smat->setija(indx+1,valueIJA); 
				}  
		
			} // end x loop
			indx = gridwest+sizeX-1 ; // case x=N-1 (east wall) 
			if (IsDirichlet(indx)) {   // Dirichlet case: zero off-diagonals
				Smat->setSameija(indx,indx);
			}

		} // end y loop
	} // end z loop
	  
	// Show sparse system:
	  
	/*
	printf(" Sparse matrix (general, non-symmetric) and rhs \n");
	Smat->show();
	*/
	  
	return true;
}

// --------------------------------------------------
bool PdeSolverDiana::PCGFormSymmetricMA_Reorder(double scale)
// --------------------------------------------------
{
		//  Form the matrix MA in symmetric sparse form
		//  This code handles cases when the points are not ordered
		//  in the natural order for a box grid. In particular,
		//  this code handles one or more subsets of the whole grid

		long   XY, index;

		// Set offset values for 2D and 3D cases
		if (sizeZ > 1) {           // 3D case
			XY   = sizeX*sizeY;
		}else {
			XY   = 0;
		}

		// Form matrix in sparse symmetric form (compact storage for pcg)
		INT32  *ija  = Smat->getIja();;     // array of pointers for pcg format
		long   numUnknowns  = Smat->getSize();     // size of system
		  
		DiscreteEqn tempEqu, neighEqu;
		double valueA;
		  
		long   j, gridindex, OffDiagCounter, valueIJA;

		// set scaling factor

		Smat->setija(0, numUnknowns+2);    // points to first off-diagonal element

		// initialize j (off-diagonal locator)
		j = numUnknowns+1;
		//
		//  Define diagonal and off-diagonal elements of the matrix

		// Loop through list of index
		for (index = 0; index < numUnknowns ; index++) {
			// By default, define diagonal elements for all points to be 1 (Dirichlet case)
			Smat->setAm(index, 1.0);
			// 

			// =======================================================
			//  determine the (x,y,z) coordinates in cartesian grid
			//  I need to write a function which computes the x,y,z
			//  coordinates corresponding to the index point in this reordering
			// test
			gridindex = Gridmap[index];
			//    gridindex = index;

			// in this case, the x,y,z coordinates are
			//    WorldCoord wc = mesh->getVolumeWorldCoord(index);
			//    int x = wc.x;
			//    int y = wc.y;
			//    int z = wc.z;
			//    gridindex = x + y*sizeX + z*XY;
			// can also be computed as:

			long z;
			if (XY == 0) {
				z = 0;
			}else {
				z = gridindex/XY;
			}
			long tt = gridindex - z*XY ;
			long y = tt/sizeX;
			long x = tt - y*sizeX;

			// =======================================================

			if ((  x == 0 || x == sizeX-1 || 
				y == 0 || y == sizeY-1 || 
				( sizeZ > 1 && ( z == 0 || z == sizeZ-1 ) ))
				&& IsDirichlet(gridindex)) {                                     // Dirichlet case: zero off-diagonals
				Smat->setSameija(index,index);  //i.e. no off-diagonal elements
			}else                  {                                             // interior gridpoint or Neumann point: 
				tempEqu = eqn[gridindex];
				valueA     = tempEqu.Ap0 + tempEqu.Ap;                             // compute diagonal element
				Smat->setAm(index, scale*valueA);                                  // scale and define diagonal element
				OffDiagCounter = 0 ;
		      
			if ((x < sizeX-2) || ( x == sizeX-2 && !IsDirichlet(gridindex+1))) {  
				valueA = -tempEqu.Ax_plus;	      // check for zero off-diagonal values (e.g. near membrane)
				if (valueA != 0.0){  
					// increase off-diagonal counter
					OffDiagCounter++;
					// function in Region
					//	  int newindex = gridindex+1;
					long newindex = Neworder[gridindex+1];
					// test: for now, use identity  int newindex = gridindex+1;
					// add 1 for fortran indexes
					valueIJA = newindex+1; 
					Smat->setija(j, valueIJA);
					Smat->setAm(j, scale*valueA);
					j++;
				}
				// NOTE - This is a Test: Check for symmetry ; not necessary once code is tested
				//        as long as we know the matrix will be symmetric
				neighEqu = eqn[gridindex+1];
				if (valueA != -neighEqu.Ax_minus) {
					printf(" matrix not symmetric indx %ld x %ld y %ld z %ld \n",index,x,y,z);
					printf(" Ax_plus(i) %lg Ax_minus(i+1) %lg \n",valueA,-neighEqu.Ax_minus);
					return false;
				}
			}else { // right-most Neumann point
			// or a point to the left of a Dirichlet boundary
			// in this case, off-diagonal is zero:    valueA = 0.0;
			}
		      
			if ((y < sizeY-2) || ( y == sizeY-2 && !IsDirichlet(gridindex+sizeX))) {  
				valueA = -tempEqu.Ay_plus;      //	  check for zero off-diagonal values (e.g. near membrane)
				if (valueA != 0.0){  
					// increase off-diagonal counter
					OffDiagCounter++;
					// function in Region
					long newindex = Neworder[gridindex+sizeX];
					// test: for now, use identity
					//	  int newindex = gridindex+sizeX; 
					valueIJA = newindex+1; //fortran indexes
					Smat->setija(j, valueIJA);
					Smat->setAm(j, scale*valueA);
					j++;
				}
				// NOTE - This is a Test: Check for symmetry ; not necessary once code is tested
				//        as long as we know the matrix will be symmetric
				neighEqu = eqn[gridindex+sizeX];
				if (valueA != -neighEqu.Ay_minus) {
					printf(" matrix not symmetric indx %ld x %ld y %ld z %ld \n",index,x,y,z);
					printf(" Ay_plus(i) %lg Ay_minus(i+1) %lg \n",valueA,-neighEqu.Ax_minus);
					return false;
				}
			}else { // north Neumann point or a point to the south of a Dirichlet boundary
				//       off-diagonal is set to zero valueA = 0.0;
			}
		      
			if (sizeZ > 1) {
				if ((z < sizeZ-2) || ( z == sizeZ-2 && !IsDirichlet(gridindex+XY))) {  
					valueA = -tempEqu.Az_plus;
					//	    check for non-zero off-diagonal values (e.g. near membrane)
					if (valueA != 0.0){  
						//increase off-diagonal counter
						OffDiagCounter++;
						long newindex = Neworder[gridindex+XY]; 
						// test: for now, use identity
						//int newindex = gridindex+XY; 
						valueIJA = newindex+1; //fortran indexes
						Smat->setija(j, valueIJA);
						Smat->setAm(j, scale*valueA);
						j++;
					}
					// NOTE - This is a Test: Check for symmetry ; not necessary once code is tested
					//        as long as we know the matrix will be symmetric
					neighEqu = eqn[gridindex+XY];
					if (valueA != -neighEqu.Az_minus) {
						printf(" matrix not symmetric indx %ld x %ld y %ld z %ld \n",index,x,y,z);
						printf(" Az_plus(i) %lg Az_minus(i+1) %lg \n",valueA,-neighEqu.Ax_minus);
						return false;
					}
				}else { // top Neumann point or a point to the south of a Dirichlet boundary
				//      valueA = 0.0;
				}
			}
			// determine pointer to off-diagonal elements
			valueIJA = ija[index] + OffDiagCounter;
			Smat->setija(index+1,valueIJA); 
		}
	}
		  
	// Show sparse system:
	// 
	//  printf(" Sparse matrix (symmetric, Re-ordered) and rhs \n");
	//  Smat->show(1,5);
		
	return true;
}


// --------------------------------------------------
bool PdeSolverDiana::PCGFormGeneralMA_Reorder(double scale)
// --------------------------------------------------
{
	//  Form the matrix MA in general (not necessarily symmetric) 
	//  sparse form
	//  This code handles cases when the points are not ordered
	//  in the natural order for a box grid. In particular,
	//  this code handles one or more subsets of the whole grid

	long   XY, index;

	// Set offset values for 2D and 3D cases
	if (sizeZ > 1) {           // 3D case
		XY   = sizeX*sizeY;
	}else {
		XY   = 0;
	}

	// Form matrix in general sparse form (compact storage for pcg)
	INT32  *ija  = Smat->getIja();;     // array of pointers for pcg format
	long   numUnknowns  = Smat->getSize();     // size of system
	  
	DiscreteEqn tempEqu;
	double valueA;
	  
	long   j, gridindex, OffDiagCounter, valueIJA;

	// set scaling factor

	Smat->setija(0, numUnknowns+2);    // points to first off-diagonal element

	// initialize j (off-diagonal locator)
	j = numUnknowns+1;
	//
	//  Define diagonal and off-diagonal elements of the matrix

	// Loop through list of index
	for (index = 0; index < numUnknowns ; index++) {
		// By default, define diagonal elements for all points to be 1 (Dirichlet case)
		Smat->setAm(index, 1.0);
		// 

		// =======================================================
		//  determine the (x,y,z) coordinates in cartesian grid
		//  I need to write a function which computes the x,y,z
		//  coordinates corresponding to the index point in this reordering
		// test
		gridindex = Gridmap[index];
		//    gridindex = index;

		// in this case, the x,y,z coordinates are
		//    WorldCoord wc = mesh->getVolumeWorldCoord(index);
		//    int x = wc.x;
		//    int y = wc.y;
		//    int z = wc.z;
		//    gridindex = x + y*sizeX + z*XY;
		// can also be computed as:

		long z;
		if (XY == 0) {
			z = 0;
		}else {
			z = gridindex/XY;
		}
		long tt = gridindex - z*XY ;
		long y = tt/sizeX;
		long x = tt - y*sizeX;

		// =======================================================

		if ((  x == 0 || x == sizeX-1 || 
			y == 0 || y == sizeY-1 || 
			( sizeZ > 1 && ( z == 0 || z == sizeZ-1 ) ))
			&& IsDirichlet(gridindex)) {                                     // Dirichlet case: zero off-diagonals
			Smat->setSameija(index,index);  //i.e. no off-diagonal elements
		}else {                                             // interior gridpoint or Neumann point: 
			tempEqu = eqn[gridindex];
			// COMPUTE DIAGONAL ELEMENT
			valueA     = tempEqu.Ap0 + tempEqu.Ap;                             
			Smat->setAm(index, scale*valueA);                                  // scale and define diagonal element
			OffDiagCounter = 0 ;
		      
			// OFF-DIAGONAL ELEMENTS
			// (i) lowest diagonal, z minus
			//     If bottom Neumann point or a point on top of a Dirichlet boundary
			//     the off-diag value is zero valueA = 0.0;
			//     else:
			if (sizeZ > 1) {
				if ((z > 1) || ( z == 1 && !IsDirichlet(gridindex-XY))) {  
					valueA = -tempEqu.Az_minus;
				// check for zero off-diagonal values (e.g. near membrane)
					if (valueA != 0.0){  
						OffDiagCounter++;
						long newindex = Neworder[gridindex-XY]; 
						valueIJA = newindex+1; //fortran indexes
						Smat->setija(j, valueIJA);
						Smat->setAm(j, scale*valueA);
						j++;
					}
				} 
			}

			// (ii) middle lower diagonal, y minus
			//      If south Neumann point or a point to the south of a Dirichlet boundary
			//      off-diagonal is set to zero valueA = 0.0;
			//      else:
			if ((y > 1) || ( y == 1 && !IsDirichlet(gridindex-sizeX))) {  
				valueA = -tempEqu.Ay_minus;      //	  check for zero off-diagonal values (e.g. near membrane)
				if (valueA != 0.0){  
					OffDiagCounter++;
					long newindex = Neworder[gridindex-sizeX];
					valueIJA = newindex+1; //fortran indexes
					Smat->setija(j, valueIJA);
					Smat->setAm(j, scale*valueA);
					j++;
				}
			} 

			// (iii) lower diagonal, x minus
			//       if west Neumann point or a point east of a west-Dirichlet boundary
			//       off-diagonal is zero:    valueA = 0.0;
			//       else:
			if ((x > 1) || ( x == 1 && !IsDirichlet(gridindex-1))) {  
				valueA = -tempEqu.Ax_minus;	      // check for zero off-diagonal values (e.g. near membrane)
				if (valueA != 0.0){  
					OffDiagCounter++;                      // increase off-diagonal counter
					long newindex = Neworder[gridindex-1];
					valueIJA = newindex+1; 	         // add 1 for fortran indexes
					Smat->setija(j, valueIJA);
					Smat->setAm(j, scale*valueA);
					j++;
				}
			} 

			// (iv) upper diag. x plus
			//      If East Neumann point or a point to the left of a Dirichlet boundary
			//      off-diagonal is set to zero:    valueA = 0.0;
			//      else:
			if ((x < sizeX-2) || ( x == sizeX-2 && !IsDirichlet(gridindex+1))) {  
				valueA = -tempEqu.Ax_plus;	      
				if (valueA != 0.0){                   // increase off-diagonal counter
					OffDiagCounter++;                   
					long newindex = Neworder[gridindex+1];// function in Region
					valueIJA = newindex+1; 	       // add 1 for fortran indexes
					Smat->setija(j, valueIJA);
					Smat->setAm(j, scale*valueA);
					j++;
				}
	
			}
	    
			// (v) middle upper diag, y plus      
			//     if North Neumann point or a point south of a North Dirichlet boundary,
			//     the off-diagonal is set to zero valueA = 0.0;
			//     else,
			if ((y < sizeY-2) || ( y == sizeY-2 && !IsDirichlet(gridindex+sizeX))) {  
				valueA = -tempEqu.Ay_plus;      //	  check for zero off-diagonal values (e.g. near membrane)
				if (valueA != 0.0){  
					OffDiagCounter++;	  // increase off-diagonal counter
					long newindex = Neworder[gridindex+sizeX];
					valueIJA = newindex+1; //fortran indexes
					Smat->setija(j, valueIJA);
					Smat->setAm(j, scale*valueA);
					j++;
				}
			}

			// (vi) top upper diagonal z-plus
			//      if top Neumann point or a point to the under a Dirichlet boundary
			//      off-diag valueA = 0.0;
			//      else:      
			if (sizeZ > 1) {
				if ((z < sizeZ-2) || ( z == sizeZ-2 && !IsDirichlet(gridindex+XY))) {  
					valueA = -tempEqu.Az_plus;
					if (valueA != 0.0){  
						OffDiagCounter++;
						long newindex = Neworder[gridindex+XY]; 
						valueIJA = newindex+1; //fortran indexes
						Smat->setija(j, valueIJA);
						Smat->setAm(j, scale*valueA);
						j++;
					}
				}
			}
				// determine pointer to off-diagonal elements
			valueIJA = ija[index] + OffDiagCounter;
			Smat->setija(index+1,valueIJA); 
		}
	}
  
	// Show sparse system:
	//  printf(" Sparse matrix (general, non-symmetric, re-ordered) and rhs \n");
	//  Smat->show(1,5);

	return true;
}

// --------------------------------------------
bool PdeSolverDiana::IsDirichlet(long index)
// --------------------------------------------
{
	//  Check if gridpoint index is a Dirichlet boundary point

	if (index < 0 || index >= mesh->getNumVolumeElements()) {
		throw "PdeSolverDiana::IsDirichlet() : index of out bound";
	}
		
	VolumeElement *pVolumeElement = mesh->getVolumeElements() + index;
	int mask = pVolumeElement->neighborMask;
	if(mask & BOUNDARY_TYPE_DIRICHLET){
		return true;
	}else{
		return false;
	}
}

void PdeSolverDiana::RHS_Mixed_Boundary_Update(double RHSscale)
// ------------------------------------------------------------------
{
  //  Update RHS for Neumann boundary points which have neighbors that are Dirichlet
  //             boundary points. Use vector mixedBPoints

	DiscreteEqn tempEqu,NeighEqu;
	long XY;
	double *pRHS = Smat->getRhs();             // right hand side

	if (sizeZ > 1) {                        // 3D case
		XY   = sizeX*sizeY;
	}else{                                  //2D
		XY = 0;
	}
 
	for (int i = 0; i < (int)mixedBPoints.size(); ++i){
		long gridindex = mixedBPoints[i].index;
		tempEqu = eqn[gridindex];
		int  bMask = mixedBPoints[i].mixedBoundaryMask;

		//changes from Diana April 24, 2003
		double valueRHS;
		long   index;
		if (!solveWholeMesh) {
			index = Neworder[gridindex];
		}else {
			index = gridindex;
		}
		if (index >= 0) {
			valueRHS = pRHS[index];
		//end of changes      
			if (bMask & UNDER_MASK){ // zm neighbor (down) is Dirichlet
				NeighEqu = eqn[gridindex-XY];
				valueRHS += NeighEqu.B*tempEqu.Az_minus*RHSscale;
			}
      
			if (bMask & SOUTH_MASK){ // ym neighbor (south) is Dirichlet
				NeighEqu = eqn[gridindex-sizeX];
				valueRHS += NeighEqu.B*tempEqu.Ay_minus*RHSscale;
			}
      
			if (bMask & WEST_MASK){ // xm neighbor (west) is Dirichlet
				NeighEqu = eqn[gridindex-1];
				valueRHS += NeighEqu.B*tempEqu.Ax_minus*RHSscale;
			}
      
			if (bMask & EAST_MASK){ // xp neighbor (east) is Dirichlet
				NeighEqu = eqn[gridindex+1];
				valueRHS += NeighEqu.B*tempEqu.Ax_plus*RHSscale;
			}
			if (bMask & NORTH_MASK){ // yp neighbor (north) is Dirichlet
				NeighEqu = eqn[gridindex+sizeX];
				valueRHS += NeighEqu.B*tempEqu.Ay_plus*RHSscale;
			}
      
			if (bMask & ABOVE_MASK){ // zp neighbor (up) is Dirichlet
				NeighEqu = eqn[gridindex+XY];
				valueRHS += NeighEqu.B*tempEqu.Az_plus*RHSscale;
			}
			// Update RHS for this point
			//changes from Diana April 24, 2003
		      
			Smat->setRhs(index, valueRHS);
		}
	}
}

// --------------------------------------------------
int* PdeSolverDiana::PCGSolve(bool bFirstTime)
// --------------------------------------------------
{
	//  prepare data to call pcgpak
	//
	double *pAm      = Smat->getAm();
	double *pRHS     = Smat->getRhs();
	double *pWork    = Smat->getRSP();
	INT32  *ija      = Smat->getIja(); //before was long
	long   NonZ      = Smat->getSizeA();
	long   Nrsp      = Smat->getSizeRSP();
	double *pNew     = var->getCurr();
	int    symmetricflg  = Smat->getSymflg();  // general or symmetric storage format
	// set number of unknowns (size of linear system to solve)
	long   numUnknowns      = Smat->getSize();
	int i;

	TimerHandle tHndPCG = SimTool::getInstance()->getTimerHandle(var->getName() + " PCG");

	int* IParm = new int[75];
	double  RParm[25];

	for (i=0;i<75;i++){
		IParm[i] = 0;
	}
	for (i=0;i<25;i++){
		RParm[i] = 0.0;
	}

	if (bFirstTime) {
		IParm[13] = 0; // compute without reusing any information.
	} else if (isTimeDependent()) {
		IParm[13] = 0; // don't reuse anything. used to be 2 to reuse symbolic incomplete factorization but have memory problem
	} else {
		IParm[13] = 1; // reuse all incomplete factorization.
	}

	// Call fortran wrapper for pcgpack
	// ---------------------------------
	// Set tolerance
	double PCG_Tolerance = PCG_TOLERANCE;

	SimTool::getInstance()->startTimer(tHndPCG);

	double RHSscale = computeRHSscale(numUnknowns, pRHS, var->getName());

	if (solveWholeMesh) {
		// initial guess is set to current solution 
		PCGWRAPPER(&numUnknowns,&Nrsp,&symmetricflg,ija,pAm,pRHS,pNew,&PCG_Tolerance,IParm,RParm,pWork,pWork, &RHSscale);
	}else{
		double *pCurrSol;
		pCurrSol = new double[numUnknowns];
		// map current mesh sol into temporary array
		// or set initial guess to zero (usually makes little difference)
		for (i=0; i<numUnknowns; i++) {
			pCurrSol[i] = pNew[Gridmap[i]];
		}

		PCGWRAPPER(&numUnknowns,&Nrsp,&symmetricflg,ija,pAm,pRHS,pCurrSol,&PCG_Tolerance,IParm,RParm,pWork,pWork, &RHSscale);

		if (IParm[50] == 0) { 
			for (i=0; i<numUnknowns; i++) {
				pNew[Gridmap[i]] = pCurrSol[i];
			}
		}
		delete[] pCurrSol;
	} 

	SimTool::getInstance()->stopTimer(tHndPCG);

#ifdef SHOW_IPARM
	cout << endl << "------PDESolverDiana----numNonZeros=" << NonZ << "----------------------" << endl;
	cout << endl << "------PDESolverDiana IPARM--------------------" << endl;
	for (int i = 0; i < 75; i ++) {
		if (IParm[i] != 0) {
			cout << "[" << (i + 1) << "," << IParm[i] << "] ";
		}
	}
	cout << endl;
#endif

#ifdef SHOW_MATRIX
	if (IParm[50] != 0) {
		cout << setprecision(10);
		cout << "----PdeSolverDiana----Variable " << var->getName() << " at " << theApplication->getSimulation()->getTime_sec() << "---------------" << endl;
		Smat->show();
		cout << "--------PdeSolverDiana----RHS-----------" << endl;
		for (int index = 0; index < numUnknowns; index++){
			if (pRHS[index] != 0) {
				cout << index << "\t" << pRHS[index] / RHSscale << endl;
			}
		}
		cout << "--------PdeSolverDiana----Solution-----------" << endl;
		for (int index = 0; index < numUnknowns; index++){
			if (pNew[index] != 0) {
				cout << index << "\t" << pNew[index] << endl;
			}
		}
	}
#endif

	return IParm;
}


// ---------------------------------------------
double PdeSolverDiana::getResidual(double *xsol)
// ---------------------------------------------
{
	//  Compute the residual for a given solution to the linear system
	//  Note: This only computes the residual for interior points
	//  Therefore, it is only completely correct for Dirichlet case, but
	//  it can be used also with other bc
	//
	long zoffset, zend, gridindex;
	DiscreteEqn tempEqu;
	double Ap;
	double res=0.0, maxse=0.0, rhsnorm=0.0, sr, matvec;
	long XY;
	if (sizeZ > 1) {
		zend = sizeZ-1;
		XY = sizeX*sizeY;
	}else {
		zend = 2;
		XY = 0;
	}
	/*
	temp2 = eqn[4001];
	for (long indx=1; indx < 51*51*51 ; indx++) {
		tempEqu = eqn[indx];
		Ap = tempEqu.Ap + tempEqu.Ap0;  //Diagonal element
		if ( (tempEqu.Ax_minus - temp2.Ax_minus) +
		(tempEqu.Ax_plus - temp2.Ax_plus) + (tempEqu.Ay_minus - temp2.Ay_minus) +
		(tempEqu.Ay_plus - temp2.Ay_plus) + (tempEqu.Az_minus - temp2.Az_minus) +
		(tempEqu.Az_plus - temp2.Az_plus) != 0   ) {
		//      printf(" indx %ld   ", indx);
		//      printf(" %lg  %lg  %lg  %lg  %lg  %lg  %lg \n", Ap ,tempEqu.Ax_minus, tempEqu.Ax_plus ,
		//     tempEqu.Ay_minus , tempEqu.Ay_plus, tempEqu.Az_minus, tempEqu.Az_plus);
		}
	}
	*/
	//   printf(" xsol and matrix \n");
	for (long z=1;z<zend;z++){
		zoffset = z*XY;
		for (long y=1;y<sizeY-1;y++){
			for (long x=1;x<sizeX-1;x++){
				gridindex = x + y*sizeX + zoffset;
				tempEqu = eqn[gridindex];
				Ap = tempEqu.Ap + tempEqu.Ap0;  //Diagonal element
				// Compute matrix*current solution:
				matvec =  xsol[gridindex]* Ap 
					- xsol[gridindex-1]* tempEqu.Ax_minus
					- xsol[gridindex+1]* tempEqu.Ax_plus 
					- xsol[gridindex-sizeX]* tempEqu.Ay_minus 
					- xsol[gridindex+sizeX]* tempEqu.Ay_plus;
				if (sizeZ>1) {
					matvec +=  - xsol[gridindex-XY]* tempEqu.Az_minus
						- xsol[gridindex+XY]* tempEqu.Az_plus;
				}
				// Compute rhs(gridindex) (Note: rhs should be precomputed)
				sr = tempEqu.B + var->getOld(gridindex)*tempEqu.Ap0;
				rhsnorm += sr*sr; // to compute 2-norm of rhs
				sr  -= matvec;    // residual sr(gridindex)
				res += sr*sr;     // to compute 2-norm of residual
				maxse = max(maxse, sr*sr); // If inf-norm is desired
			} // end x loop
		} // end y loop
	} // end z loop
	//  printf("res (2-norm) = %lg %lg \n", sqrt(res) ,sqrt(sr));
	//  res   = sqrt(res/rhsnorm); // relative residual
	res   = sqrt(res); // absolute residual
	//  maxse = sqrt(maxse/rhsnorm); // inf norm (mixed with 2-norm - fix this
	maxse = sqrt(maxse); // inf norm 
	//printf("res (2-norm) = %lg res (max-norm) = %lg   norm rhs  %lg\n", res , maxse, sqrt(rhsnorm));
	return res;
}
