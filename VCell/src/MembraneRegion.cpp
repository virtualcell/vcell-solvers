/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/MembraneRegion.h>
#include <VCELL/VolumeRegion.h>
#include <VCELL/CartesianMesh.h>

MembraneRegion::MembraneRegion(CartesianMesh *mesh) : Region(mesh)
{
	regionInside = NULL;
	regionOutside = NULL;
	surface = 0;
}

void MembraneRegion::setRegionInside(VolumeRegion *region)
{
	regionInside = region;
}

void MembraneRegion::setRegionOutside(VolumeRegion *region)
{
	regionOutside = region;
}

void MembraneRegion::recomputeSurface()
{
	surface = 0;
	for(long i = 0; i < (long)index.size(); i ++){
		surface += (mesh->getMembraneElements()+getIndex(i))->area;
	}
}

double MembraneRegion::getSurface()
{
	if(wasChanged){
		recomputeSurface();
		wasChanged = false;
	}
	return surface;     
}

void MembraneRegion::setSurface(double newSurface)
{
	surface = newSurface;     
}
