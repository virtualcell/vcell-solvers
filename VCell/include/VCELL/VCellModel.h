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
	int getNumFeatures() {
		return (int)featureList.size();
	}
	Feature* getFeatureFromHandle(FeatureHandle handle);
	Feature* getFeatureFromName(string&  name);
	Feature* getFeatureFromIndex(int index);
	   
	void resolveReferences();
	   
	void addFeature(Feature *feature);
	void addContour(Contour *contour);
   
private:
	vector<Feature*> featureList;
	vector<Contour*> pContours;
};

#endif
