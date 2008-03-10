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
	void addMembrane(MembraneRegion *membrane);
	MembraneRegion *getMembrane(int i) { return membranes[i]; }
	int getNumMembranes() { return (int)membranes.size(); }
	bool isClosed();
	double  getVolume();
	void setVolume(double newVolume);
    
	void setNotClosed() { closure = false; }
	void setBoundaryDirichlet() { bBoundaryDirichlet = true; }
	bool isBoundaryDirichlet() { return bBoundaryDirichlet; }
private:
	void recompute();
	Feature *feature;
	vector<MembraneRegion*> membranes;
	bool closure;     //denotes whether the region surface is closed
	double volume;
	bool bBoundaryDirichlet;
};

#endif
