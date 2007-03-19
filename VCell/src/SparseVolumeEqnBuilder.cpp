/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifdef WIN32
#include <Windows.h>
#else
#include <UnixDefs.h>
#endif

#include <stdio.h>
#include <VCELL/App.h>
#include <VCELL/SimTypes.h>
#include <VCELL/Solver.h>
#include <VCELL/Variable.h>
#include <VCELL/Mesh.h>
#include <VCELL/Feature.h>
#include <VCELL/Element.h>
#include <VCELL/VarContext.h>
#include <VCELL/SparseVolumeEqnBuilder.h>
#include <VCELL/Simulation.h>
#include <VCELL/FVUtils.h>

#ifndef WIN32
#define max(a,b) (((a)>(b))?(a):(b))
#endif

//#define USE_DIANA_SCALE

static int GENERAL_MAX_NONZERO_PERROW[4] = {0, 3, 5, 7};
static int TRIANGULAR_MAX_NONZERO_PERROW[4] = {0, 2, 3, 4};
SparseVolumeEqnBuilder::SparseVolumeEqnBuilder(VolumeVariable *Aspecies, CartesianMesh *Amesh, bool arg_bNoConvection) : SparseMatrixEqnBuilder(Aspecies, Amesh)
{
	bSymmetricStorage = arg_bNoConvection;
	N = mesh->getNumVolumeElements();
	int sizeX = Amesh->getNumVolumeX();
	int sizeY = Amesh->getNumVolumeY();
	int sizeZ = Amesh->getNumVolumeZ();
	int dim = mesh->getDimension();

	int numNonZeros;
	switch (dim) {
		case 1:
			if (bSymmetricStorage) {
				numNonZeros = TRIANGULAR_MAX_NONZERO_PERROW[dim] * N;
			} else {
				numNonZeros = GENERAL_MAX_NONZERO_PERROW[dim] * N;
			}
			break;
		case 2:
			if (bSymmetricStorage) { // symmetric half-storage
				numNonZeros = TRIANGULAR_MAX_NONZERO_PERROW[dim] * N - sizeX - sizeY + 1;
			}else {  // general storage
				numNonZeros = GENERAL_MAX_NONZERO_PERROW[dim] * N - sizeX - sizeY + 2;
			}
			break;
		case 3:
			if (bSymmetricStorage) {  // symmetric half-storage
				numNonZeros = TRIANGULAR_MAX_NONZERO_PERROW[2] * sizeX * sizeY - sizeX - sizeY + 1; //2D
				numNonZeros = numNonZeros + (sizeZ - 1) * (numNonZeros + sizeX * sizeY);
			} else { // general storage
				numNonZeros = GENERAL_MAX_NONZERO_PERROW[2] * sizeX * sizeY - sizeX - sizeY + 2; //2D
				numNonZeros = numNonZeros + (sizeZ - 1) * (numNonZeros + 2 * sizeX * sizeY) + 1;
			}

	}
	if (bSymmetricStorage) {
		A = new SparseMatrixPCG(N, numNonZeros, MATRIX_SYMMETRIC); // only store upper triangle
	} else {
		A = new SparseMatrixPCG(N, numNonZeros, MATRIX_GENERAL);
	}
	B = new double[N];
	memset(B, 0, N * sizeof(double)); 

	globalScale = 1.0;	
#ifdef USE_DIANA_SCALE
	cout << endl << "Using SparseVolumeEqnBuilder and Diana's old scale for variable " << var->getName() << endl;
#else 
	cout << endl << "Using SparseVolumeEqnBuilder and deltaT/deltaV as scale for variable " << var->getName() << endl;
#endif
	bPeriodicPointsInitialized = false;
}

SparseVolumeEqnBuilder::~SparseVolumeEqnBuilder() {
	delete A;
	delete[] B;
	for (int i = 0; i < (int)dirichletNeighbors.size(); i ++) {
		delete dirichletNeighbors[i];
	}
	dirichletNeighbors.clear();
	for (int i = 0; i < (int)periodicCoupledPoints.size(); i ++) {
		delete periodicCoupledPoints[i];
	}
	periodicCoupledPoints.clear();
}

long SparseVolumeEqnBuilder::getN() {
	return N;
}

//------------------------------------------------------------------
//
// Left Hand Side
//
//------------------------------------------------------------------
boolean SparseVolumeEqnBuilder::initEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize)
{    
	/**
	 * we decided to scale both sides with deltaT/VOLUME
	 * we defined volumeScale_i such that VOLUME * volumeScale_i = V_i
	 * 
	 * Aii = volumeScale + sum(Aij)
	 * Aij = - Di * lambda, lambda is lambdaX or lambdaY or lambdaZ, for example,
	 * lambdaX = deltaT / (deltaX * deltaX)	 
	 * lambda is scaled for boundary points
	 *
	 * B_i = U_old * volumeScale_i + R * deltaT + boundary conditions
	 **/

	if (!bPeriodicPointsInitialized) {
		initPeriodicPoints();
	}

	double epsilon = 1e-10;    // zero diffusion threshold at 1e-10 micron^2/second

	ASSERTION(solver->getVar() == var);

	int DIM =  mesh->getDimension();
	double DELTAX = ((CartesianMesh*)mesh)->getXScale_um();
	double DELTAY = ((CartesianMesh*)mesh)->getYScale_um();
	double DELTAZ = ((CartesianMesh*)mesh)->getZScale_um();
	double AREAX  = ((CartesianMesh*)mesh)->getXArea_squm();
	double AREAY  = ((CartesianMesh*)mesh)->getYArea_squm();
	double AREAZ  = ((CartesianMesh*)mesh)->getZArea_squm();
	double VOLUME = ((CartesianMesh*)mesh)->getVolume_cu();
	int SIZEX = ((CartesianMesh*)mesh)->getNumVolumeX();
	int SIZEY = ((CartesianMesh*)mesh)->getNumVolumeY();
	int SIZEZ = ((CartesianMesh*)mesh)->getNumVolumeZ();
	int SIZEXY = SIZEX * SIZEY;	
	VolumeElement* pVolumeElement = mesh->getVolumeElements();

#ifdef USE_DIANA_SCALE
	// Diana used the first diagonal value of a non dirichlet point as global scale. 
	int lastNonScaledIndex = 0;
	bool bFoundScale = false;   // change to true to use 1.0 as global scale
	globalScale = 1.0; 	
#else 
	// this is new global scale, but we don't really use it explicity
	// because something cancels out. We first cancel out then compute. 
	globalScale = deltaTime / VOLUME; 
#endif

	A->clear();  // if it's time dependent diffusion, we need to start over	
	for (int i = 0; i < (int)dirichletNeighbors.size(); i ++) {
		delete dirichletNeighbors[i];
	}
	dirichletNeighbors.clear();  // to store Aij where point j is dirichlet condition now and later move to right hand side	
	double (*neighborConvectionCoeffs)[4] = 0; // to store convection coeffecients, only allocate once and release once each timestep
	if (!bSymmetricStorage) {
		neighborConvectionCoeffs = new double[6][4];
	}
	for (int index = volumeIndexStart; index < volumeIndexStart + volumeIndexSize; index ++){
		A->addNewRow();
		Feature* feature = pVolumeElement[index].feature;
		VolumeVarContext* varContext = feature->getVolumeVarContext( (VolumeVariable*)var);		
		if (varContext != NULL) {
			double Aii = 0.0;
			int mask = pVolumeElement[index].neighborMask;

			if (mask & BOUNDARY_TYPE_DIRICHLET // dirichlet 				
				|| (mask & NEIGHBOR_XP_BOUNDARY && feature->getXpBoundaryType() == BOUNDARY_PERIODIC)  // periodic plus direction
				|| (mask & NEIGHBOR_YP_BOUNDARY && feature->getYpBoundaryType() == BOUNDARY_PERIODIC) 
				|| (mask & NEIGHBOR_ZP_BOUNDARY && feature->getZpBoundaryType() == BOUNDARY_PERIODIC)) {   
				Aii = 1.0;			
			} else { // non-dirichlet condition, including neumann or periodic minus or interior
				double volumeScale = 1.0;	
#ifdef USE_DIANA_SCALE
				double lambdaX = AREAX/DELTAX;
				double lambdaY = AREAY/DELTAY;
				double lambdaZ = AREAZ/DELTAZ;
				double lambdaAreaX = AREAX;   
				double lambdaAreaY = AREAY;
				double lambdaAreaZ = AREAZ;

#else
				double lambdaX = deltaTime/(DELTAX * DELTAX);
				double lambdaY = deltaTime/(DELTAY * DELTAY);
				double lambdaZ = deltaTime/(DELTAZ * DELTAZ);
				double lambdaAreaX = deltaTime/DELTAX;   
				double lambdaAreaY = deltaTime/DELTAY;
				double lambdaAreaZ = deltaTime/DELTAZ;
#endif
				double Di = varContext->getDiffusionRate(index);
				validateNumber(var->getName(), index, "Diffusion term", Di);

				if (mask & NEIGHBOR_BOUNDARY_MASK){   // boundary
					volumeScale /= (mask & VOLUME_MASK);

					if (mask & NEIGHBOR_X_BOUNDARY_MASK){
						lambdaY /= 2.0;
						lambdaZ /= 2.0;
						lambdaAreaY /= 2.0;
						lambdaAreaZ /= 2.0;
					}
					if (mask & NEIGHBOR_Y_BOUNDARY_MASK){
						lambdaX /= 2.0;
						lambdaZ /= 2.0;
						lambdaAreaX /= 2.0;
						lambdaAreaZ /= 2.0;
					}
					if (mask & NEIGHBOR_Z_BOUNDARY_MASK){
						lambdaX /= 2.0;
						lambdaY /= 2.0;
						lambdaAreaX /= 2.0;
						lambdaAreaY /= 2.0;
					}
				}
				
				int neighborInfos[6][2] = {
					{NEIGHBOR_ZM_MASK, index - SIZEXY},  // boundary mask, neighbor index
					{NEIGHBOR_YM_MASK, index - SIZEX},
					{NEIGHBOR_XM_MASK, index - 1},
					{NEIGHBOR_XP_MASK, index + 1},
					{NEIGHBOR_YP_MASK, index + SIZEX},
					{NEIGHBOR_ZP_MASK, index + SIZEXY}
				};
				
				int startNeighbor = 0;
				int endNeighbor = 5;
				switch (DIM) {
					case 1:
						startNeighbor = 2;
                        endNeighbor = 3;
						break;
					case 2:
						startNeighbor = 1;
                        endNeighbor = 4;
						break;
					case 3:
						startNeighbor = 0;
                        endNeighbor = 5;
						break;
				}

				// for periodic boundary condition, have to sort to make sure order
				{
					if (mask & NEIGHBOR_XM_BOUNDARY && feature->getXmBoundaryType() == BOUNDARY_PERIODIC) {
						// make sure XM and XP are in the same feature for each periodic bounary point
						int xpindex = index + SIZEX - 1;
						if (feature != pVolumeElement[xpindex].feature) {
							throw "Periodic Boundary Condition (X- and X+): compartments don't match";
						}
						int xpmask = pVolumeElement[xpindex].neighborMask;
						// inherit membrane from XP 
						mask |= (xpmask & NEIGHBOR_XM_MEMBRANE);
					}
					if (mask & NEIGHBOR_YM_BOUNDARY && feature->getYmBoundaryType() == BOUNDARY_PERIODIC) {
						int ypindex = index + (SIZEY - 1) * SIZEX;
						if (feature != pVolumeElement[ypindex].feature) {
							throw "Periodic Boundary Condition (Y- and Y+): compartments don't match";
						}
						int ypmask = pVolumeElement[ypindex].neighborMask;
						mask |= (ypmask & NEIGHBOR_YM_MEMBRANE);
					}
					if (mask & NEIGHBOR_ZM_BOUNDARY && feature->getZmBoundaryType() == BOUNDARY_PERIODIC) {
						int zpindex = index + (SIZEZ - 1) * SIZEXY;
						if (feature != pVolumeElement[zpindex].feature) {
							throw "Periodic Boundary Condition (Z- and Z+): compartments don't match";
						}
						int zpmask = pVolumeElement[zpindex].neighborMask;
						mask |= (zpmask & NEIGHBOR_ZM_MEMBRANE);
					}

					bool bSort = false;

					if ((mask & NEIGHBOR_XM_BOUNDARY) && feature->getXmBoundaryType() == BOUNDARY_PERIODIC) {
						// remove boundary (keep membrane) if it's periodic
						neighborInfos[2][0] = NEIGHBOR_XM_MEMBRANE;
						neighborInfos[2][1] = index + (SIZEX - 2);
						volumeScale *= 2;
						lambdaY *= 2.0;
						lambdaZ *= 2.0;
						lambdaAreaY *= 2.0;
						lambdaAreaZ *= 2.0;
						bSort = true;
					} 
					if (DIM > 1 && (mask & NEIGHBOR_YM_BOUNDARY) && feature->getYmBoundaryType() == BOUNDARY_PERIODIC) {
						neighborInfos[1][0] = NEIGHBOR_YM_MEMBRANE;
						neighborInfos[1][1] = index + (SIZEY - 2) * SIZEX;
						volumeScale *= 2;
						lambdaX *= 2.0;
						lambdaZ *= 2.0;
						lambdaAreaX *= 2.0;
						lambdaAreaZ *= 2.0;
						bSort = true;
					} 
					if (DIM > 2 && (mask & NEIGHBOR_ZM_BOUNDARY) && feature->getZmBoundaryType() == BOUNDARY_PERIODIC) {
						neighborInfos[0][0] = NEIGHBOR_ZM_MEMBRANE;
						neighborInfos[0][1] = index + (SIZEZ - 2) * SIZEXY;
						volumeScale *= 2;
						lambdaX *= 2.0;
						lambdaY *= 2.0;
						lambdaAreaX *= 2.0;
						lambdaAreaY *= 2.0;
						bSort = true;
					}

					int neighborIndex;
					int neighborMask;
					Feature* neighborFeature = 0;
					neighborIndex = neighborInfos[3][1];
					if (neighborIndex < N) {
						neighborMask = pVolumeElement[neighborIndex].neighborMask;
						neighborFeature = pVolumeElement[neighborIndex].feature;				
						if (neighborMask & NEIGHBOR_XP_BOUNDARY && neighborFeature->getXpBoundaryType() == BOUNDARY_PERIODIC) {
							neighborInfos[3][1] = index - (SIZEX - 2);	
							bSort = true;
						}
					}

					neighborIndex = neighborInfos[4][1];
					if (DIM > 1 && neighborIndex < N) {
						neighborMask = pVolumeElement[neighborIndex].neighborMask;
						neighborFeature = pVolumeElement[neighborIndex].feature;				
						if (neighborMask & NEIGHBOR_YP_BOUNDARY && neighborFeature->getYpBoundaryType() == BOUNDARY_PERIODIC) {
							neighborInfos[4][1] = index - (SIZEY - 2) * SIZEX;
							bSort = true;
						}
					}

					neighborIndex = neighborInfos[5][1];
					if (DIM > 2 && neighborIndex < N) {
						neighborMask = pVolumeElement[neighborIndex].neighborMask;
						neighborFeature = pVolumeElement[neighborIndex].feature;				
						if (neighborMask & NEIGHBOR_ZP_BOUNDARY && neighborFeature->getZpBoundaryType() == BOUNDARY_PERIODIC) {
							neighborInfos[5][1] = index - (SIZEZ - 2) * SIZEXY;
							bSort = true;
						}
					}

					// sort
					if (bSort) {
						for (int n = startNeighbor; n < endNeighbor; n ++) {
							for (int m = n + 1; m <= endNeighbor; m ++) {
								if (neighborInfos[n][1] > neighborInfos[m][1]) {
									// switch
									int mask = neighborInfos[m][0];
									int index = neighborInfos[m][1];
									neighborInfos[m][0] = neighborInfos[n][0];
									neighborInfos[m][1] = neighborInfos[n][1];
									neighborInfos[n][0] = mask;
									neighborInfos[n][1] = index;
								}
							}
						}
					}
				}
				
				double neighborLambdas[6] = {lambdaZ, lambdaY, lambdaX, lambdaX, lambdaY, lambdaZ};

				// compute these values only when there is convection				
				double Vxi = 0.0, Vyi = 0.0, Vzi = 0.0;				
				if (!bSymmetricStorage) {
					Vxi = varContext->getConvectionVelocity_X(index);
					Vyi = varContext->getConvectionVelocity_Y(index);
					Vzi = varContext->getConvectionVelocity_Z(index);
					double preComputedCoeffs[6][4] = { 
						{1.0, Vzi, (mask & neighborInfos[0][0]) ? 0 : varContext->getConvectionVelocity_Z(neighborInfos[0][1]), lambdaAreaZ},  //direction, Vi, Vj, lamdaArea
						{1.0, Vyi, (mask & neighborInfos[1][0]) ? 0 : varContext->getConvectionVelocity_Y(neighborInfos[1][1]), lambdaAreaY},
						{1.0, Vxi, (mask & neighborInfos[2][0]) ? 0 : varContext->getConvectionVelocity_X(neighborInfos[2][1]), lambdaAreaX},
						{-1.0, Vxi, (mask & neighborInfos[3][0]) ? 0 : varContext->getConvectionVelocity_X(neighborInfos[3][1]), lambdaAreaX},
						{-1.0, Vyi, (mask & neighborInfos[4][0]) ? 0 : varContext->getConvectionVelocity_Y(neighborInfos[4][1]), lambdaAreaY},
						{-1.0, Vzi, (mask & neighborInfos[5][0]) ? 0 : varContext->getConvectionVelocity_Z(neighborInfos[5][1]), lambdaAreaZ}
					};
					memcpy(neighborConvectionCoeffs, preComputedCoeffs, 6 * 4 * sizeof(double));
				} // end if (!bSymmetricStorage)


#ifdef USE_DIANA_SCALE
				Aii = volumeScale * VOLUME / deltaTime;
#else
				Aii = volumeScale;
#endif
				// loop through neighbors to compute Aii and Aij
				// if there is no convection, we only store upper triangle (neighborIndex > index)
				// if one of the neighbors is dirichlet point, we store the value into a vector, 
				// then later when building the right hand side, we added the these values to right hand side.
				for (int n = startNeighbor; n <= endNeighbor; n ++) {
					int boundaryMask = neighborInfos[n][0];				
					if (!(mask & boundaryMask)){ // if it is not next to a boundary (wall or membrane)
						int neighborIndex = neighborInfos[n][1];
						int neighborMask = pVolumeElement[neighborIndex].neighborMask;
												
						double lambda = neighborLambdas[n];
						double Dj = varContext->getDiffusionRate(neighborIndex);
						double D = (Di + Dj < epsilon) ? (0.0) : (2 * Di * Dj/(Di + Dj));
						double Aij = 0.0;
						if (bSymmetricStorage) {
							Aij = D * lambda;
							Aii += Aij;
						} else { // if there is convection
							double convectionDirection = neighborConvectionCoeffs[n][0];
							double Vi = neighborConvectionCoeffs[n][1], Vj = neighborConvectionCoeffs[n][2], lamdaArea = neighborConvectionCoeffs[n][3];
							validateNumber(var->getName(), index, "Velocity term", Vi);
							validateNumber(var->getName(), neighborIndex, "Velocity term", Vj);
							double V = 0.5 * (Vi + Vj);
							Aij = max(D * lambda + convectionDirection * 0.5 * V * lamdaArea, max(convectionDirection * V * lamdaArea, 0));
							Aii += max(D * lambda - convectionDirection * 0.5 * V * lamdaArea, max(- convectionDirection * V * lamdaArea, 0));
						}
						if (Aij != 0.0) {	
							validateNumber(var->getName(), index, "LHS", Aij);
#ifdef USE_DIANA_SCALE
							Aij *= globalScale;
#endif
							if (neighborMask & BOUNDARY_TYPE_DIRICHLET) { // dirichlet 
								dirichletNeighbors.push_back(new DirichletNeighbor(index, neighborIndex, - Aij));
							} else if (!bSymmetricStorage || neighborIndex > index) {
								A->setCol(neighborIndex, - Aij);								
							}
						} // end if (Aij != 0.0)
					} // end if (!(mask & boundaryMask))
				} // end for n
				validateNumber(var->getName(), index, "LHS", Aii);
#ifdef USE_DIANA_SCALE
				Aii *= globalScale;
				if (!bFoundScale) {
					globalScale = 1.0/Aii;
					bFoundScale = true;
					lastNonScaledIndex = index;
				}
#endif
			} // end if else (mask & BOUNDARY_TYPE_DIRICHLET)
			A->setDiag(index, Aii);
		} // end if (varContext!=NULL) 		
    } // end for index

	if (!bSymmetricStorage) {
		delete[] neighborConvectionCoeffs;
	}	
	A->close();

#ifdef USE_DIANA_SCALE
	// we didn't scale the rows before we found the scale
	for (int index = volumeIndexStart; index <= lastNonScaledIndex; index ++){
		int mask = pVolumeElement[index].neighborMask;
		if (mask & BOUNDARY_TYPE_DIRICHLET){   // skip dirichlet points
			continue;
		}
		// diagonal
		double diag = A->getValue(index, index);
		A->setValue(index, index, diag * globalScale);
		// off diagonal
		INT32* columns;
		double* values;
		int numColumns = A->getColumns(index, columns, values);
		for (int i = 0; i < numColumns; i ++) {
			values[i] *= globalScale;
		}
	}
	for (int i = 0; i < (int)dirichletNeighbors.size(); i ++) {		
		DirichletNeighbor* dn = dirichletNeighbors[i];
		if (dn->centerIndex <= lastNonScaledIndex) {
			dn->coeff *= globalScale;
		} else {
			break;
		}
	}	
#endif
    return TRUE;
}

//------------------------------------------------------------------
//
// Right Hand side
//
//------------------------------------------------------------------
boolean SparseVolumeEqnBuilder::buildEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize)
{    
	int DIM =  mesh->getDimension();
	double DELTAX = ((CartesianMesh*)mesh)->getXScale_um();
	double DELTAY = ((CartesianMesh*)mesh)->getYScale_um();
	double DELTAZ = ((CartesianMesh*)mesh)->getZScale_um();
	double AREAX  = ((CartesianMesh*)mesh)->getXArea_squm();
	double AREAY  = ((CartesianMesh*)mesh)->getYArea_squm();
	double AREAZ  = ((CartesianMesh*)mesh)->getZArea_squm();
	int SIZEX = ((CartesianMesh*)mesh)->getNumVolumeX();
	int SIZEY = ((CartesianMesh*)mesh)->getNumVolumeY();
	int SIZEZ = ((CartesianMesh*)mesh)->getNumVolumeZ();
	int SIZEXY = SIZEX * SIZEY;	
	double VOLUME = ((CartesianMesh*)mesh)->getVolume_cu();

	Simulation *sim = theApplication->getSimulation();
	VolumeElement *pVolumeElement = mesh->getVolumeElements();
	MembraneElement *pMembraneElement = mesh->getMembraneElements();

	for (int index = volumeIndexStart; index < volumeIndexStart + volumeIndexSize; index++){
		Feature* feature = pVolumeElement[index].feature;
		VolumeVarContext* varContext = feature->getVolumeVarContext((VolumeVariable*)var);
		int mask = pVolumeElement[index].neighborMask;

		if (mask & BOUNDARY_TYPE_DIRICHLET){		
			if ((mask & NEIGHBOR_XM_BOUNDARY) && (feature->getXmBoundaryType() == BOUNDARY_VALUE)){
				sim->advanceTimeOn();
				B[index] = varContext->getXmBoundaryValue(index);
				sim->advanceTimeOff();

			} else if ((mask & NEIGHBOR_XP_BOUNDARY) && (feature->getXpBoundaryType() == BOUNDARY_VALUE)){
				sim->advanceTimeOn();
				B[index] = varContext->getXpBoundaryValue(index);
				sim->advanceTimeOff();

			} else if ((mask & NEIGHBOR_YM_BOUNDARY) && (feature->getYmBoundaryType() == BOUNDARY_VALUE)){
				sim->advanceTimeOn();
				B[index] = varContext->getYmBoundaryValue(index);
				sim->advanceTimeOff();

			} else if ((mask & NEIGHBOR_YP_BOUNDARY) && (feature->getYpBoundaryType() == BOUNDARY_VALUE)){
				sim->advanceTimeOn();
				B[index] = varContext->getYpBoundaryValue(index);
				sim->advanceTimeOff();

			} else if ((mask & NEIGHBOR_ZM_BOUNDARY) && (feature->getZmBoundaryType() == BOUNDARY_VALUE)){
				sim->advanceTimeOn();
				B[index] = varContext->getZmBoundaryValue(index);
				sim->advanceTimeOff();

			} else if ((mask & NEIGHBOR_ZP_BOUNDARY) && (feature->getZpBoundaryType() == BOUNDARY_VALUE)){
				sim->advanceTimeOn();
				B[index] = varContext->getZpBoundaryValue(index);
				sim->advanceTimeOff();

			} else {
				assert(0);
			}	
		
		} else if (((mask & NEIGHBOR_XP_BOUNDARY) && feature->getXpBoundaryType() == BOUNDARY_PERIODIC)  // periodic and plus direction
				|| ((mask & NEIGHBOR_YP_BOUNDARY) && feature->getYpBoundaryType() == BOUNDARY_PERIODIC) 
				|| ((mask & NEIGHBOR_ZP_BOUNDARY) && feature->getZpBoundaryType() == BOUNDARY_PERIODIC)) {   
			B[index] = 0;
		
		} else { // no Dirichlet conditions (interior or neumann or minus periodic)
			double volumeScale = 1.0;
#ifdef USE_DIANA_SCALE
			double lambdaAreaX = AREAX;   
			double lambdaAreaY = AREAY;
			double lambdaAreaZ = AREAZ;

#else
			double lambdaAreaX = deltaTime/DELTAX;   
			double lambdaAreaY = deltaTime/DELTAY;
			double lambdaAreaZ = deltaTime/DELTAZ;
#endif
			if (mask & NEIGHBOR_BOUNDARY_MASK){   // boundary
				volumeScale /= (mask & VOLUME_MASK);

				if (mask & NEIGHBOR_X_BOUNDARY_MASK){
					lambdaAreaY /= 2.0;
					lambdaAreaZ /= 2.0;
				}
				if (mask & NEIGHBOR_Y_BOUNDARY_MASK){
					lambdaAreaX /= 2.0;
					lambdaAreaZ /= 2.0;
				}
				if (mask & NEIGHBOR_Z_BOUNDARY_MASK){
					lambdaAreaX /= 2.0;
					lambdaAreaY /= 2.0;
				}	
			}

			if (mask & NEIGHBOR_XM_BOUNDARY && feature->getXmBoundaryType() == BOUNDARY_PERIODIC) {
				volumeScale *= 2;
			} 
			if (mask & NEIGHBOR_YM_BOUNDARY && feature->getYmBoundaryType() == BOUNDARY_PERIODIC) {
				volumeScale *= 2;
			} 
			if (mask & NEIGHBOR_ZM_BOUNDARY && feature->getZmBoundaryType() == BOUNDARY_PERIODIC) {
				volumeScale *= 2;
			}

#ifdef USE_DIANA_SCALE
			B[index] = var->getOld(index) * volumeScale * VOLUME / deltaTime + varContext->getReactionRate(index) * volumeScale * VOLUME;
#else
			B[index] = var->getOld(index) * volumeScale + varContext->getReactionRate(index) * volumeScale * deltaTime;
#endif
			if ((mask & BOUNDARY_TYPE_MASK) == BOUNDARY_TYPE_NEUMANN) { // for corners, it might be both neuman and periodic, but periodic wins.
				if (mask & NEIGHBOR_XM_BOUNDARY && feature->getXmBoundaryType() == BOUNDARY_FLUX){
					sim->advanceTimeOn();
					B[index] += varContext->getXmBoundaryFlux(index) * lambdaAreaX;
					sim->advanceTimeOff();
				}
				if (mask & NEIGHBOR_XP_BOUNDARY && feature->getXpBoundaryType() == BOUNDARY_FLUX){
					sim->advanceTimeOn();
					B[index] += - varContext->getXpBoundaryFlux(index) * lambdaAreaX;
					sim->advanceTimeOff();
				}
				if (mask & NEIGHBOR_YM_BOUNDARY && feature->getYmBoundaryType() == BOUNDARY_FLUX){
					sim->advanceTimeOn();
					B[index] += varContext->getYmBoundaryFlux(index) * lambdaAreaY;
					sim->advanceTimeOff();
				}
				if (mask & NEIGHBOR_YP_BOUNDARY && feature->getYpBoundaryType() == BOUNDARY_FLUX){
					sim->advanceTimeOn();
					B[index] += - varContext->getYpBoundaryFlux(index) * lambdaAreaY;
					sim->advanceTimeOff();
				}
				if (mask & NEIGHBOR_ZM_BOUNDARY && feature->getZmBoundaryType() == BOUNDARY_FLUX){
					sim->advanceTimeOn();
					B[index] += varContext->getZmBoundaryFlux(index) * lambdaAreaZ;
					sim->advanceTimeOff();
				}
				if (mask & NEIGHBOR_ZP_BOUNDARY && feature->getZpBoundaryType() == BOUNDARY_FLUX){
					sim->advanceTimeOn();
					B[index] += - varContext->getZpBoundaryFlux(index) * lambdaAreaZ;
					sim->advanceTimeOff();
				}
			}
			if (mask & NEIGHBOR_MEMBRANE_MASK){ // if there is membrane
				int numAdjacentME = pVolumeElement[index].adjacentMembraneIndexes.size();
				for (int i = 0; i < numAdjacentME; i ++) {
					MembraneElement *me = pMembraneElement + pVolumeElement[index].adjacentMembraneIndexes[i];
					VolumeVarContext* anotherVarContext = me->feature->getVolumeVarContext((VolumeVariable*)var);

					double inFlux, outFlux;
					sim->advanceTimeOn();
					anotherVarContext->getFlux(me, &inFlux, &outFlux);
					sim->advanceTimeOff();		

					if (me->insideIndexNear == index) {
#ifdef USE_DIANA_SCALE
						B[index] += inFlux * me->area;
#else
						B[index] += inFlux * me->area * deltaTime / VOLUME;
#endif
					} else if (me->outsideIndexNear == index) {
#ifdef USE_DIANA_SCALE
						B[index] += outFlux * me->area;
#else
						B[index] += outFlux * me->area * deltaTime / VOLUME;
#endif
					}
				}
			} // end if (mask & NEIGHBOR_MEMBRANE_MASK)

#ifdef USE_DIANA_SCALE
			B[index] *= globalScale;
#endif			
			// for periodic boundary condition
			{
				int neighborIndex;
				if ((mask & NEIGHBOR_XM_BOUNDARY) && feature->getXmBoundaryType() == BOUNDARY_PERIODIC) {
					neighborIndex = index + (SIZEX - 2);
					double Aij = A->getValue(index, neighborIndex);
					B[index] += Aij * varContext->getXBoundaryPeriodicConstant();
				} 
				if (DIM > 1 && (mask & NEIGHBOR_YM_BOUNDARY) && feature->getYmBoundaryType() == BOUNDARY_PERIODIC) {
					neighborIndex = index + (SIZEY - 2) * SIZEX;
					double Aij = A->getValue(index, neighborIndex);
					B[index] += Aij * varContext->getYBoundaryPeriodicConstant();
				} 
				if ((DIM > 2 && mask & NEIGHBOR_ZM_BOUNDARY) && feature->getZmBoundaryType() == BOUNDARY_PERIODIC) {
					neighborIndex = index + (SIZEZ - 2) * SIZEXY;
					double Aij = A->getValue(index, neighborIndex);
					B[index] += Aij * varContext->getZBoundaryPeriodicConstant();
				}

				int neighborMask;
				Feature* neighborFeature;
				neighborIndex = index + 1;
				if (neighborIndex < N) {
					int neighborMask = pVolumeElement[neighborIndex].neighborMask;
					Feature* neighborFeature = pVolumeElement[neighborIndex].feature;				
					if (neighborMask & NEIGHBOR_XP_BOUNDARY && neighborFeature->getXpBoundaryType() == BOUNDARY_PERIODIC) {
						neighborIndex = index - (SIZEX - 2);
						double Aij = 0.0;
						if (bSymmetricStorage) {
							Aij = A->getValue(neighborIndex, index);
						} else {
							Aij = A->getValue(index, neighborIndex);
						}
						B[index] -= Aij * varContext->getXBoundaryPeriodicConstant();
					}
				}

				neighborIndex = index + SIZEX;
				if (DIM > 1 && neighborIndex < N) {
					neighborMask = pVolumeElement[neighborIndex].neighborMask;
					neighborFeature = pVolumeElement[neighborIndex].feature;				
					if (neighborMask & NEIGHBOR_YP_BOUNDARY && neighborFeature->getYpBoundaryType() == BOUNDARY_PERIODIC) {
						neighborIndex = index - (SIZEY - 2) * SIZEX;
						double Aij = 0.0;
						if (bSymmetricStorage) {
							Aij = A->getValue(neighborIndex, index);
						} else {
							Aij = A->getValue(index, neighborIndex);
						}
						B[index] -= Aij * varContext->getYBoundaryPeriodicConstant();
					}
				}

				neighborIndex = index + SIZEXY;
				if (DIM > 2 && neighborIndex < N) {
					neighborMask = pVolumeElement[neighborIndex].neighborMask;
					neighborFeature = pVolumeElement[neighborIndex].feature;				
					if (neighborMask & NEIGHBOR_ZP_BOUNDARY && neighborFeature->getZpBoundaryType() == BOUNDARY_PERIODIC) {
						neighborIndex = index - (SIZEZ - 2) * SIZEXY;
						double Aij = 0.0;
						if (bSymmetricStorage) {
							Aij = A->getValue(neighborIndex, index);
						} else {
							Aij = A->getValue(index, neighborIndex);
						}
						B[index] -= Aij * varContext->getZBoundaryPeriodicConstant();
					}
				}
			}			
			validateNumber(var->getName(), index, "RHS", B[index]);
		} // end if else (mask & BOUNDARY_TYPE_DIRICHLET)
	} // end index	

	// to make the matrix symmetric
	// for the points who have dirichlet neighbors
	// we have to change the right hand side
	for (int i = 0; i < (int)dirichletNeighbors.size(); i ++) {
		DirichletNeighbor* dn = dirichletNeighbors[i];
		B[dn->centerIndex] -= dn->coeff * B[dn->neighborIndex];		
	}	
	return TRUE;
}

void SparseVolumeEqnBuilder::initPeriodicPoints() {
	CartesianMesh* mesh = (CartesianMesh*)getMesh();
	int numX = mesh->getNumVolumeX();
	int numY = mesh->getNumVolumeY();
	int numZ = mesh->getNumVolumeZ();
	int numXY = numX * numY;
	VolumeElement* pVolumeElement = mesh->getVolumeElements();

	// X direction
	for (int k = 0; k < numZ; k ++){
		for (int j = 0; j < numY; j ++){
			int indexm = k * numXY + j * numX;
			int indexp = k * numXY + j * numX + numX - 1;
			if (numX == 1) {
				break;
			}
			int mask = pVolumeElement[indexm].neighborMask;
			VolumeVarContext* varContext = pVolumeElement[indexm].feature->getVolumeVarContext((VolumeVariable*)var);	
			if (mask & NEIGHBOR_XM_BOUNDARY && pVolumeElement[indexm].feature->getXmBoundaryType() == BOUNDARY_PERIODIC) {
				periodicCoupledPoints.push_back(new PeriodicCoupledPoints(indexm, indexp, X, varContext));					
			}
		}
	}
	// Y direction
	for (int k = 0; k < numZ; k ++){
		for (int i = 0; i < numX; i ++){		
			int indexm = k * numXY + i;
			int indexp = k * numXY + (numY - 1) * numX + i;
			if (numY == 1) {
				break;
			}
			int mask = pVolumeElement[indexm].neighborMask;
			VolumeVarContext* varContext = pVolumeElement[indexm].feature->getVolumeVarContext((VolumeVariable*)var);	
			if (mask & NEIGHBOR_YM_BOUNDARY && pVolumeElement[indexm].feature->getYmBoundaryType() == BOUNDARY_PERIODIC) {
				periodicCoupledPoints.push_back(new PeriodicCoupledPoints(indexm, indexp, Y, varContext));
			}
		}
	}

	// Z direction
	for (int j = 0; j < numY; j ++){
		for (int i = 0; i < numX; i ++){
			int indexm = j * numX + i;
			int indexp = (numZ - 1) * numXY + j * numX + i;
			if (numZ == 1) {
				break;
			}
			int mask = pVolumeElement[indexm].neighborMask;
			VolumeVarContext* varContext = pVolumeElement[indexm].feature->getVolumeVarContext((VolumeVariable*)var);	
			if (mask & NEIGHBOR_ZM_BOUNDARY && pVolumeElement[indexm].feature->getZmBoundaryType() == BOUNDARY_PERIODIC) {
				periodicCoupledPoints.push_back(new PeriodicCoupledPoints(indexm, indexp, Z, varContext));
			}
		}
	}		
	bPeriodicPointsInitialized = true;
}

void SparseVolumeEqnBuilder::updatePeriodicPoints(double* solution) {		
	for (int i = 0; i < (int)periodicCoupledPoints.size(); i ++){
		PeriodicCoupledPoints* pcp = periodicCoupledPoints[i];
		switch (pcp->xyz) {
			case X:			
				solution[pcp->indexp] = solution[pcp->indexm] + pcp->volVarContext->getXBoundaryPeriodicConstant();
				break;
			case Y:			
				solution[pcp->indexp] = solution[pcp->indexm] + pcp->volVarContext->getYBoundaryPeriodicConstant();
				break;
			case Z:			
				solution[pcp->indexp] = solution[pcp->indexm] + pcp->volVarContext->getZBoundaryPeriodicConstant();
				break;
		} 
	}
}