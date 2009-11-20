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

void VolumeRegionEqnBuilder::buildEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize) {
	int size = ((CartesianMesh*)mesh)->getNumVolumeRegions();
	ASSERTION(size==var->getSize());
	double *pRate = odeSolver->getRates();
	for(int i=0; i<size; i++){
		*pRate = 0;    // constant Dirichlet (shortening) is implied
		VolumeRegion* volRegion = ((CartesianMesh*)mesh)->getVolumeRegion(i);
		Feature* feature = volRegion->getFeature();
		VolumeRegionVarContext* volRegionVarContext = feature->getVolumeRegionVarContext((VolumeRegionVariable*)var);

		if (volRegionVarContext != 0) {
			*pRate = volRegionVarContext->getUniformRate(volRegion);

			double volume = volRegion->getVolume();
			ASSERTION(volume>0);
			int numElements = volRegion->getNumElements();
			double volumeIntegral = 0.0;
			for(int j=0; j<numElements; j++){
				long index = volRegion->getIndex(j); 
				volumeIntegral += volRegionVarContext->getReactionRate(index) * mesh->getVolumeOfElement_cu(index);
			}
			*pRate += volumeIntegral/volume;

			int numMembraneRegions = volRegion->getNumMembraneRegions();
			double surfaceIntegral = 0.0;
			for(int k=0; k<numMembraneRegions; k++){
				MembraneRegion *membraneRegion = volRegion->getMembraneRegion(k);
				numElements = membraneRegion->getNumElements();
				for(int j=0; j<numElements; j++){
					double inFlux = 0.0;
					double outFlux = 0.0;
					MembraneElement *pElement = mesh->getMembraneElements() + membraneRegion->getIndex(j);
					volRegionVarContext->getFlux(pElement, &inFlux, &outFlux);
					surfaceIntegral += inFlux * pElement->area;
				}
			}
			*pRate += surfaceIntegral/volume; 
		}		
		pRate++;
	}
}
