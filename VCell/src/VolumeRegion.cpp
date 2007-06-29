/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VolumeRegion.h>
#include <VCELL/CartesianMesh.h>
#include <VCELL/Feature.h>

VolumeRegion::VolumeRegion(CartesianMesh *mesh) : Region(mesh)
{
	feature = NULL;
	closure = false;
	volume = 0;
	membranes.erase(membranes.begin(), membranes.end()); 
}

void VolumeRegion::addMembrane(MembraneRegion *membrane)
{
	membranes.push_back(membrane);
}

void VolumeRegion::recompute()
{
	volume = 0;
	for(long i=0; i < (long)index.size(); i++){
		volume += mesh->getVolumeOfElement_cu(getIndex(i));
	}
	closure = true;
	for(long i=0; i < (long)index.size(); i++){
		if(((mesh->getVolumeElements()+getIndex(i))->neighborMask)
												&(NEIGHBOR_BOUNDARY_MASK)){
			closure = false;
			break;
		}
	}
	feature = (mesh->getVolumeElements()+getIndex(0))->feature;	
}

bool VolumeRegion::isClosed()
{
	if(wasChanged){
		recompute();
		wasChanged = false;
	}
	return closure;     
}

double VolumeRegion::getVolume()
{
	if(volume == 0 && wasChanged){
		recompute();
		wasChanged = false;
	}
	return volume;     
}

void VolumeRegion::setVolume(double newVolume)
{
	volume = newVolume;
}

Feature* VolumeRegion::getFeature()
{
	if(feature == NULL && wasChanged){
		recompute();
		wasChanged = false;
	}
	return feature;     
}

void VolumeRegion::setFeature(Feature* newFeature)
{
	feature = newFeature;     
}
