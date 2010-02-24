/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/Simulation.h>
#include <VCELL/ODESolver.h>
#include <VCELL/MembraneRegionVariable.h>
#include <VCELL/Feature.h>
#include <VCELL/MembraneRegionVarContext.h>
#include <VCELL/MembraneRegion.h>
#include <VCELL/VolumeRegion.h>
#include <VCELL/MembraneRegionEqnBuilder.h>
#include <VCELL/CartesianMesh.h>

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
		Feature* feature = memRegion->getRegionInside()->getFeature();
		MembraneRegionVarContext * memRegionvarContext = feature->getMembraneRegionVarContext((MembraneRegionVariable*)var);

		if (memRegionvarContext != 0) {
			if(memRegion->getRegionInside()->isClosed()) {
				*pRate = memRegionvarContext->getUniformRate(memRegion);
				double surface = memRegion->getSurface();
				ASSERTION(surface>0);
				long numElements = memRegion->getNumElements();
				double surfaceIntegral = 0.0;
				for(long j=0; j<numElements; j++) {
					long index = memRegion->getIndex(j); 
					MembraneElement *pElement = mesh->getMembraneElements() + index;
					surfaceIntegral += memRegionvarContext->getMembraneReactionRate(pElement) * pElement->area;
				}
				*pRate += surfaceIntegral/surface;
			} else {
				throw "MembraneRegionEqnBuilder::buildEquation(), only implemented for closed surface, please check geometry";
			}
		}
		pRate++;
	}
}
