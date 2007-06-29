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

bool MembraneRegionEqnBuilder::buildEquation(double deltaTime, 
                            int volumeIndexStart, int volumeIndexSize, 
			    int membraneIndexStart, int membraneIndexSize)
{
	Feature *feature;
	MembraneRegionVarContext *varContext;
	MembraneRegion *region;
	int size = ((CartesianMesh*)mesh)->getNumMembraneRegions();
	ASSERTION(size==var->getSize());
	double *pRate = odeSolver->getRates();

	for(int i=0; i<size; i++){
		*pRate = 0;            
		region = ((CartesianMesh*)mesh)->getMembraneRegion(i);
		feature = (region->getRegionInside())->getFeature();
		varContext = feature->getMembraneRegionVarContext((MembraneRegionVariable*)var);
		if(((region->getRegionInside())->isClosed())||(varContext->hasZeroFluxBoundary())){
			*pRate = varContext->getUniformRate(region);
			double surface = region->getSurface();
			ASSERTION(surface>0);
			if(!((varContext->hasUniformRate())&&(varContext->hasUniformFlux()))){
				long numElements = region->getNumElements();
				double surfaceIntegral = 0.0;
				for(long j=0; j<numElements; j++){
					long index = region->getIndex(j); 
					double inFlux = 0.0;
					double outFlux = 0.0;
					MembraneElement *pElement = mesh->getMembraneElements() + index;
					varContext->getFlux(pElement, &inFlux, &outFlux);
					surfaceIntegral += (inFlux + varContext->getMembraneReactionRate(pElement)) * (pElement->area);
				}
				*pRate += surfaceIntegral/surface;
			} 
		}
		pRate++;
	}
	return true;
}
