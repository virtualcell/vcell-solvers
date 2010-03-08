/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/Simulation.h>
#include <VCELL/ODESolver.h>
#include <VCELL/MembraneRegionVariable.h>
#include <VCELL/Membrane.h>
#include <VCELL/MembraneRegionVarContext.h>
#include <VCELL/MembraneRegion.h>
#include <VCELL/MembraneRegionEqnBuilder.h>
#include <VCELL/CartesianMesh.h>
#include <VCELL/Element.h>

MembraneRegionEqnBuilder::MembraneRegionEqnBuilder(MembraneRegionVariable *Avar, CartesianMesh *Amesh, ODESolver *Asolver)
: EqnBuilder(Avar,Amesh)
{
	odeSolver = Asolver;
}

void MembraneRegionEqnBuilder::buildEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize)
{
	int size = ((CartesianMesh*)mesh)->getNumMembraneRegions();
	ASSERTION(size==var->getSize());
	double *pRate = odeSolver->getRates();

	for(int i=0; i<size; i++) {
		*pRate = 0;            
		MembraneRegion *memRegion = ((CartesianMesh*)mesh)->getMembraneRegion(i);
		Membrane* membrane = memRegion->getMembrane();
		MembraneRegionVarContext * memRegionvarContext = membrane->getMembraneRegionVarContext((MembraneRegionVariable*)var);

		*pRate = memRegionvarContext->getUniformRate(memRegion);
		double surface = memRegion->getSize();
		ASSERTION(surface>0);
		long numElements = memRegion->getNumElements();
		double surfaceIntegral = 0.0;
		for(long j=0; j<numElements; j++) {
			long index = memRegion->getElementIndex(j); 
			MembraneElement *pElement = mesh->getMembraneElements() + index;
			surfaceIntegral += memRegionvarContext->getMembraneReactionRate(pElement) * pElement->area;
		}
		*pRate += surfaceIntegral/surface;
		pRate++;
	}
}
