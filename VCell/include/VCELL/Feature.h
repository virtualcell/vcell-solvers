/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef FEATURE_H
#define FEATURE_H

#include <VCELL/Structure.h>
#include <vector>
using std::vector;

class VolumeVarContext;
class VolumeRegionVarContext;
//class VolumeParticleContext;
//class ContourParticleContext;
class MembraneVarContext;
class MembraneRegionVarContext;
class FastSystem;
class Feature;
class Simulation;
class VolumeVariable;
class MembraneVariable;
class VolumeRegionVariable;
class MembraneRegionVariable;

class Feature : public Structure
{
public:
	Feature(string& name, unsigned char findex, FeatureHandle handle);
	~Feature();

	void resolveReferences(Simulation *sim);
	virtual void initVolumeValues(long volumeIndex);
	virtual void initVolumeRegionValues(int volumeRegionIndex);

	FeatureHandle   getHandle();
	unsigned char getIndex() {
		return index;
	}

	VolumeVarContext *getVolumeVarContext(VolumeVariable *var);
	VolumeRegionVarContext *getVolumeRegionVarContext(VolumeRegionVariable *var);
	   
	void addVolumeVarContext(VolumeVarContext *vc);	
	void addVolumeRegionVarContext(VolumeRegionVarContext *vc);	
	   
	void reinitConstantValues();

	//VolumeParticleContext     *getVolumeParticleContext(){return vpc;}
	//MembraneParticleContext   *getMembraneParticleContext(){return mpc;}
	//ContourParticleContext    *getContourParticleContext(){return cpc;}
	//VolumeParticleContext     *setVolumeParticleContext(VolumeParticleContext *Avpc);
	//MembraneParticleContext   *setMembraneParticleContext(MembraneParticleContext *Ampc);
	//ContourParticleContext    *setContourParticleContext(ContourParticleContext *Acpc);	
   
protected:

	//VolumeParticleContext     *vpc;
	//MembraneParticleContext   *mpc;
	//ContourParticleContext    *cpc;
	
	vector<VolumeVarContext*> volumeVarContextList;
	vector<VolumeRegionVarContext*> volumeRegionVarContextList;
	   
	FeatureHandle handle;
	unsigned char index;
};  

#endif
