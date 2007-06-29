/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VolumeRegionEqnBuilder.h>
#include <VCELL/VolumeRegionVariable.h>
#include <math.h>
//#include <VCELL/Simulation.h>
#include <VCELL/ODESolver.h>
#include <VCELL/Feature.h>
#include <VCELL/VolumeRegionVarContext.h>
#include <VCELL/VolumeRegion.h>
#include <VCELL/MembraneRegion.h>
#include <VCELL/CartesianMesh.h>

VolumeRegionEqnBuilder::VolumeRegionEqnBuilder(VolumeRegionVariable *Avar, CartesianMesh *Amesh, ODESolver *Asolver) : EqnBuilder(Avar,Amesh) {
	odeSolver = Asolver;
}

bool VolumeRegionEqnBuilder::buildEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize) {
	Feature *feature;
	VolumeRegionVarContext *varContext;
	VolumeRegion *region;
	int size = ((CartesianMesh*)mesh)->getNumVolumeRegions();
	ASSERTION(size==var->getSize());
	double *pRate = odeSolver->getRates();

	for(int i=0; i<size; i++){
		*pRate = 0;    // constant Dirichlet (shortening) is implied
		region = ((CartesianMesh*)mesh)->getVolumeRegion(i);
		feature = region->getFeature();
		varContext = feature->getVolumeRegionVarContext((VolumeRegionVariable*)var);
		long numElements, j;
		if((region->isClosed())||(varContext->hasZeroFluxBoundary())){
			*pRate = varContext->getUniformRate(region);
			double volume = region->getVolume();
			ASSERTION(volume>0);
			if(!(varContext->hasUniformRate())){
				numElements = region->getNumElements();
				double volumeIntegral = 0.0;
				for(j=0; j<numElements; j++){
					long index = region->getIndex(j); 
					volumeIntegral += (varContext->getReactionRate(index))*(mesh->getVolumeOfElement_cu(index));
				}
				*pRate += volumeIntegral/volume;
			}
			if(!(varContext->hasUniformFlux())){
				int numMembranes = region->getNumMembranes();
				double surfaceIntegral = 0.0;
				for(int k=0; k<numMembranes; k++){
					MembraneRegion *membrane = region->getMembrane(k);
					numElements = membrane->getNumElements();
					for(j=0; j<numElements; j++){
						double inFlux = 0.0;
						double outFlux = 0.0;
						MembraneElement *pElement = mesh->getMembraneElements() + membrane->getIndex(j);
						varContext->getFlux(pElement, &inFlux, &outFlux);
						surfaceIntegral += inFlux * (pElement->area);
					}
				}
				*pRate += surfaceIntegral/volume; 
			}
		}
		pRate++;
	}
	return true;
}
