/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef FEATURE_H
#define FEATURE_H

#include <VCELL/Structure.h>
#include <vector>
using std::vector;

class VolumeVarContextExpression;
class VolumeRegionVarContextExpression;
class MembraneVarContextExpression;
class MembraneRegionVarContextExpression;
class FastSystem;
class Feature;
class SimulationExpression;
class VolumeVariable;
class MembraneVariable;
class VolumeRegionVariable;
class MembraneRegionVariable;

class Feature : public Structure
{
public:
	Feature(string& name, unsigned char findex, FeatureHandle handle);
	~Feature();

	void resolveReferences(SimulationExpression *sim);

	FeatureHandle   getHandle();
	unsigned char getIndex() {
		return index;
	}

	VolumeVarContextExpression *getVolumeVarContext(VolumeVariable *var);
	VolumeRegionVarContextExpression *getVolumeRegionVarContext(VolumeRegionVariable *var);
	   
	void addVolumeVarContext(VolumeVarContextExpression *vc);
	void addVolumeRegionVarContext(VolumeRegionVarContextExpression *vc);
	
	vector<int>& getMemVarIndexesInAdjacentMembranes() {
		return memVarIndexesInAdjacentMembranes;
	}
	
	void addMemVarIndexInAdjacentMembrane(int index) {
		memVarIndexesInAdjacentMembranes.push_back(index);
	}
	
protected:

	vector<VolumeVarContextExpression*> volumeVarContextList;
	vector<VolumeRegionVarContextExpression*> volumeRegionVarContextList;
	   
	FeatureHandle handle;
	unsigned char index;
	
	vector<int> memVarIndexesInAdjacentMembranes;
};  

#endif
