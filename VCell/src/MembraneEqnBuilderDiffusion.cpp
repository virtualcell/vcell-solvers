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
#include <VCELL/MembraneEqnBuilderDiffusion.h>
#include <VCELL/Simulation.h>


//---------------------------------------------------------------------
//
//     dC       d^2 C
//    ---- = D ------- 
//     dt       dx^2
//
//---------------------------------------------------------------------
MembraneEqnBuilderDiffusion::MembraneEqnBuilderDiffusion(MembraneVariable *Aspecies, Mesh *Amesh)
: SparseMatrixEqnBuilder(Aspecies, Amesh)
{
	//variable = Aspecies;
	//mesn = Amesh;
	SparseMatrixPCG* membraneElementCoupling = ((CartesianMesh*)mesh)->getMembraneCoupling();
	A = new SparseMatrixPCG(membraneElementCoupling);
	N = mesh->getNumMembraneElements();
	B = new double[N];
	memset(B, 0, N * sizeof(double)); 
}

MembraneEqnBuilderDiffusion::~MembraneEqnBuilderDiffusion() {
	delete A;
	delete[] B;
}

long MembraneEqnBuilderDiffusion::getN() {
	return N;
}

//------------------------------------------------------------------
//
// Initializes Matrix only
//
//------------------------------------------------------------------
boolean MembraneEqnBuilderDiffusion::initEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, 
				int membraneIndexStart, int membraneIndexSize)
{   	
	const double epsilon = 1.0E-8;

	VolumeElement *pVolumeElement = mesh->getVolumeElements();
	MembraneElement *pMembraneElement = mesh->getMembraneElements();

	ASSERTION(pVolumeElement);	
	
	SparseMatrixPCG* membraneElementCoupling = ((CartesianMesh*)mesh)->getMembraneCoupling();

	MembraneElement* membraneElement = pMembraneElement;
	for (long index = 0; index < N; index ++, membraneElement ++){		
		ASSERTION(membraneElement->feature);			
		MembraneVarContext *varContext = membraneElement->feature->getMembraneVarContext((MembraneVariable*)var);		
		int mask = mesh->getMembraneNeighborMask(membraneElement);
		INT32* columns;
		double* values;
		int numColumns = membraneElementCoupling->getColumns(index, columns, values);
		if (mask & NEIGHBOR_BOUNDARY_MASK && mask & BOUNDARY_TYPE_DIRICHLET){   // boundary and dirichlet
			A->setDiag(index, 1.0);

			// clear other non zeros
			for (long j = 0; j < numColumns; j ++) {
				INT32 neighborIndex = columns[j];
				A->setValue(index, neighborIndex, 0);
			}		
		} else {		
			double Di = varContext->getMembraneDiffusionRate(membraneElement);
			double volume = membraneElementCoupling->getValue(index, index);
			double Aii = volume/deltaTime;

			for (long j = 0; j < numColumns; j ++) {
				INT32 neighborIndex = columns[j];
				double Dj = varContext->getMembraneDiffusionRate(pMembraneElement + neighborIndex);
				double D = (Di + Dj < epsilon)?(0.0):(2 * Di * Dj/(Di + Dj));
				double Aij = D * values[j];
				A->setValue(index, neighborIndex, -Aij);
				Aii += Aij;
			}		
			
			A->setDiag(index, Aii);
		}

	} // end index

	//A->show();

	return TRUE;
}

//------------------------------------------------------------------
//
// updates B vector only with reaction rates and boundary conditions
//
//------------------------------------------------------------------
boolean MembraneEqnBuilderDiffusion::buildEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, 
				int membraneIndexStart, int membraneIndexSize)
{    
	VolumeElement *pVolumeElement = mesh->getVolumeElements();
	MembraneElement *pMembraneElement = mesh->getMembraneElements();
	ASSERTION(pMembraneElement);	

	Simulation *sim = theApplication->getSimulation();
	SparseMatrixPCG* membraneElementCoupling = ((CartesianMesh*)mesh)->getMembraneCoupling();

	MembraneElement* membraneElement = pMembraneElement;
	for (long index = 0;index < N; index ++, membraneElement ++){
		ASSERTION(membraneElement->feature);		
		Feature* feature = membraneElement->feature;
		MembraneVarContext *varContext = feature->getMembraneVarContext((MembraneVariable*)var);	

		double volume = membraneElementCoupling->getValue(index, index);
		double Ap0 = volume/deltaTime;

		int mask = mesh->getMembraneNeighborMask(membraneElement);
		if (mask & NEIGHBOR_BOUNDARY_MASK){ // boundary condition
			if (mask & BOUNDARY_TYPE_DIRICHLET){   // boundary and dirichlet
				if ((mask & NEIGHBOR_XM_BOUNDARY) && (feature->getXmBoundaryType() == BOUNDARY_VALUE)){
					sim->advanceTimeOn();
					B[index] = varContext->getXmBoundaryValue(membraneElement);
					sim->advanceTimeOff();

				}else if ((mask & NEIGHBOR_XP_BOUNDARY) && (feature->getXpBoundaryType() == BOUNDARY_VALUE)){

					sim->advanceTimeOn();
					B[index] = varContext->getXpBoundaryValue(membraneElement);
					sim->advanceTimeOff();

				}else if ((mask & NEIGHBOR_YM_BOUNDARY) && (feature->getYmBoundaryType() == BOUNDARY_VALUE)){

					sim->advanceTimeOn();
					B[index] = varContext->getYmBoundaryValue(membraneElement);
					sim->advanceTimeOff();

				}else if ((mask & NEIGHBOR_YP_BOUNDARY) && (feature->getYpBoundaryType() == BOUNDARY_VALUE)){

					sim->advanceTimeOn();
					B[index] = varContext->getYpBoundaryValue(membraneElement);
					sim->advanceTimeOff();

				}else if ((mask & NEIGHBOR_ZM_BOUNDARY) && (feature->getZmBoundaryType() == BOUNDARY_VALUE)){

					sim->advanceTimeOn();
					B[index] = varContext->getZmBoundaryValue(membraneElement);
					sim->advanceTimeOff();

				}else if ((mask & NEIGHBOR_ZP_BOUNDARY) && (feature->getZpBoundaryType() == BOUNDARY_VALUE)){

					sim->advanceTimeOn();
					B[index] = varContext->getZpBoundaryValue(membraneElement);
					sim->advanceTimeOff();

				}else{
					assert(0);
				}
			} else if (mask & BOUNDARY_TYPE_PERIODIC) {
				throw "periodic boundary conditon for membrane diffusion is not supported at the moment.";
			} else if (mask & BOUNDARY_TYPE_NEUMANN) { // boundary and neumann
				B[index] = Ap0 * var->getOld(index) + varContext->getMembraneReactionRate(membraneElement) * volume;

				if (mesh->getDimension() == 2) { // all the flux area is 1
					if (mask & NEIGHBOR_XM_BOUNDARY){
						sim->advanceTimeOn();
						B[index] += varContext->getXmBoundaryFlux(membraneElement);
						sim->advanceTimeOff();
					}
					if (mask & NEIGHBOR_XP_BOUNDARY){
						sim->advanceTimeOn();
						B[index] += - varContext->getXpBoundaryFlux(membraneElement);
						sim->advanceTimeOff();
					}
					if (mask & NEIGHBOR_YM_BOUNDARY){
						sim->advanceTimeOn();
						B[index] += varContext->getYmBoundaryFlux(membraneElement);
						sim->advanceTimeOff();
					}
					if (mask & NEIGHBOR_YP_BOUNDARY){
						sim->advanceTimeOn();
						B[index] += - varContext->getYpBoundaryFlux(membraneElement);
						sim->advanceTimeOff();
					}
				} else if (mesh->getDimension() == 3) {										
					double* boundaryFluxArea = mesh->getMembraneFluxArea(index);

					if (mask & NEIGHBOR_XM_BOUNDARY){
						sim->advanceTimeOn();
						B[index] += varContext->getXmBoundaryFlux(membraneElement)*boundaryFluxArea[BL_Xm];
						sim->advanceTimeOff();
					}
					if (mask & NEIGHBOR_XP_BOUNDARY){
						sim->advanceTimeOn();
						B[index] += - varContext->getXpBoundaryFlux(membraneElement)*boundaryFluxArea[BL_Xp];
						sim->advanceTimeOff();
					}
					if (mask & NEIGHBOR_YM_BOUNDARY){
						sim->advanceTimeOn();
						B[index] += varContext->getYmBoundaryFlux(membraneElement)*boundaryFluxArea[BL_Ym];
						sim->advanceTimeOff();
					}
					if (mask & NEIGHBOR_YP_BOUNDARY){
						sim->advanceTimeOn();
						B[index] += - varContext->getYpBoundaryFlux(membraneElement)*boundaryFluxArea[BL_Yp];
						sim->advanceTimeOff();
					}
					if (mask & NEIGHBOR_ZM_BOUNDARY){
						sim->advanceTimeOn();
						B[index] += varContext->getZmBoundaryFlux(membraneElement)*boundaryFluxArea[BL_Zm];
						sim->advanceTimeOff();
					}
					if (mask & NEIGHBOR_ZP_BOUNDARY){
						sim->advanceTimeOn();
						B[index] += - varContext->getZpBoundaryFlux(membraneElement)*boundaryFluxArea[BL_Zp];
						sim->advanceTimeOff();
					}					
				}
			}
		} else { // no boundary condition
			B[index] = Ap0 * var->getOld(index) + varContext->getMembraneReactionRate(membraneElement) * volume;
		}
	} // end index

	return TRUE;
}