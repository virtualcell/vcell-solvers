/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef VCELLMODEL_H
#define VCELLMODEL_H 

#include <VCELL/SimTypes.h>
#include <vector>
using namespace std;

class Feature;
class Contour;
class Simulation;

class VCellModel
{
public:
	VCellModel();
	~VCellModel();
	Contour *getContour(int type);

	int getNumContours();
	Feature *getFeature(FeatureHandle handle);
	Feature *getFeature(string&  name);
	Feature *getNextFeature(Feature *ptr=NULL);
	   
	bool resolveReferences();
	   
	void addFeature(Feature *feature);
	void addContour(Contour *contour);
   
private:
	Feature *featureList;
	vector<Contour*> pContours;
};

#endif
