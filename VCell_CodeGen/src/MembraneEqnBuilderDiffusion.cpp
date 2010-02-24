/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */

#include <VCELL/MembraneEqnBuilderDiffusion.h>
#include <VCELL/SimTypes.h>
#include <VCELL/Solver.h>
#include <VCELL/MembraneVariable.h>
#include <VCELL/Feature.h>
#include <VCELL/Element.h>
#include <VCELL/MembraneVarContext.h>
#include <VCELL/Simulation.h>
#include <VCELL/SimTool.h>
#include <VCELL/CartesianMesh.h>

MembraneEqnBuilderDiffusion::MembraneEqnBuilderDiffusion(MembraneVariable *Aspecies, Mesh *Amesh)
: SparseMatrixEqnBuilder(Aspecies, Amesh)
{
	SparseMatrixPCG* membraneElementCoupling = ((CartesianMesh*)mesh)->getMembraneCoupling();
	A = new SparseMatrixPCG(membraneElementCoupling);
	long size = mesh->getNumMembraneElements();
	B = new double[size];
	memset(B, 0, size * sizeof(double)); 
}

MembraneEqnBuilderDiffusion::~MembraneEqnBuilderDiffusion() {
	delete A;
	delete[] B;
}

//------------------------------------------------------------------
//
// Initializes Matrix only
//
//------------------------------------------------------------------
void MembraneEqnBuilderDiffusion::initEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, 
				int membraneIndexStart, int membraneIndexSize)
{   	
	const double epsilon = 1.0E-8;

	VolumeElement *pVolumeElement = mesh->getVolumeElements();
	MembraneElement *pMembraneElement = mesh->getMembraneElements();

	ASSERTION(pVolumeElement);	
	
	SparseMatrixPCG* membraneElementCoupling = ((CartesianMesh*)mesh)->getMembraneCoupling();

	MembraneElement* membraneElement = pMembraneElement;
	for (long index = membraneIndexStart; index < membraneIndexStart + membraneIndexSize; index ++, membraneElement ++){		
		ASSERTION(membraneElement->feature);			
		MembraneVarContext *varContext = membraneElement->feature->getMembraneVarContext((MembraneVariable*)var);		
		int mask = mesh->getMembraneNeighborMask(membraneElement);
		int32* columns;
		double* values;
		int numColumns = membraneElementCoupling->getColumns(index, columns, values);
		if (mask & NEIGHBOR_BOUNDARY_MASK && mask & BOUNDARY_TYPE_DIRICHLET){   // boundary and dirichlet
			A->setDiag(index, 1.0);

			// clear other non zeros
			for (long j = 0; j < numColumns; j ++) {
				int32 neighborIndex = columns[j];
				A->setValue(index, neighborIndex, 0);
			}		
		} else {		
			double Di = varContext->getMembraneDiffusionRate(membraneElement);
			double volume = membraneElementCoupling->getValue(index, index);
			double Aii = volume/deltaTime;

			for (long j = 0; j < numColumns; j ++) {
				int32 neighborIndex = columns[j];
				double Dj = varContext->getMembraneDiffusionRate(pMembraneElement + neighborIndex);
				double D = (Di + Dj < epsilon)?(0.0):(2 * Di * Dj/(Di + Dj));
				double Aij = D * values[j];
				A->setValue(index, neighborIndex, -Aij);
				Aii += Aij;
			}		
			
			A->setDiag(index, Aii);
		}

	} // end index
}

//------------------------------------------------------------------
//
// updates B vector only with reaction rates and boundary conditions
//
//------------------------------------------------------------------
void MembraneEqnBuilderDiffusion::buildEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, 
				int membraneIndexStart, int membraneIndexSize)
{    
	VolumeElement *pVolumeElement = mesh->getVolumeElements();
	MembraneElement *pMembraneElement = mesh->getMembraneElements();
	ASSERTION(pMembraneElement);	

	Simulation *sim = SimTool::getInstance()->getSimulation();
	SparseMatrixPCG* membraneElementCoupling = ((CartesianMesh*)mesh)->getMembraneCoupling();

	MembraneElement* membraneElement = pMembraneElement;
	for (long index = membraneIndexStart; index < membraneIndexStart + membraneIndexSize; index ++, membraneElement ++){
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
					throw "MembraneEqnBuilderDiffusion::buildEquation() : invalid boundary type";
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
}
