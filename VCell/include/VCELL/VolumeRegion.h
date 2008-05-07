/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef VOLUMEREGION_H
#define VOLUMEREGION_H 

#include <VCELL/Region.h>

class Feature;
class CartesianMesh;
class MembraneRegion;


class VolumeRegion : public Region
{
public:
	VolumeRegion(CartesianMesh *mesh);
	Feature *getFeature();
	void setFeature(Feature* newFeature);
	void addMembraneRegion(MembraneRegion *membraneRegion);
	MembraneRegion *getMembraneRegion(int i) { return membraneRegionList[i]; }
	int getNumMembraneRegions() { return (int)membraneRegionList.size(); }
	bool isClosed();
	double  getVolume();
	void setVolume(double newVolume);
    
	void setNotClosed() { closure = false; }
	void setBoundaryDirichlet() { bBoundaryDirichlet = true; }
	bool isBoundaryDirichlet() { return bBoundaryDirichlet; }
private:
	void recompute();
	Feature *feature;
	vector<MembraneRegion*> membraneRegionList;
	bool closure;     //denotes whether the region surface is closed
	double volume;
	bool bBoundaryDirichlet;
};

#endif
