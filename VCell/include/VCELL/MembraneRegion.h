/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef MEMBRANEREGION_H
#define MEMBRANEREGION_H 

#include <VCELL/Region.h>

class Feature;
class Mesh;
class CartesianMesh;
class VolumeRegion;
class MembraneRegion;

class MembraneRegion : public Region
{
public:
	MembraneRegion(CartesianMesh *mesh);
	void setRegionInside(VolumeRegion *region);
	void setRegionOutside(VolumeRegion *region);
	VolumeRegion *getRegionInside() { return regionInside; }
	VolumeRegion *getRegionOutside() { return regionOutside; }
	double  getSurface();
	void setSurface(double newSurface);
	    
private:
	void recomputeSurface();
	VolumeRegion *regionInside;
	VolumeRegion *regionOutside;
	double surface;
};

#endif
