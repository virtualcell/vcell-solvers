/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VolumeVariable.h>
#include <VCELL/VolumeRegionVariable.h>
#include <VCELL/Feature.h>
#include <VCELL/VolumeVarContextExpression.h>
#include <VCELL/VolumeRegionVarContextExpression.h>

Feature::Feature(string& name, unsigned char findex, FeatureHandle Ahandle) : Structure(name)
{
	index = findex;
	handle = Ahandle;
}

Feature::~Feature()
{
}

void Feature::resolveReferences(SimulationExpression *sim)
{	
	for (int i = 0; i < (int)volumeVarContextList.size(); i ++) {
		VolumeVarContextExpression* volumeVarContext = volumeVarContextList[i];
		volumeVarContext->resolveReferences(sim);
	}
	
	for (int i = 0; i < (int)volumeRegionVarContextList.size(); i ++) {
		VolumeRegionVarContextExpression *volumeRegionVarContext = volumeRegionVarContextList[i];
		volumeRegionVarContext->resolveReferences(sim);
	}
}

void Feature::addVolumeVarContext(VolumeVarContextExpression *vvc)
{
	volumeVarContextList.push_back(vvc);
}

void Feature::addVolumeRegionVarContext(VolumeRegionVarContextExpression *vrvc)
{
	volumeRegionVarContextList.push_back(vrvc);
}

FeatureHandle Feature::getHandle()
{
	return handle;
}

VolumeVarContextExpression* Feature::getVolumeVarContext(VolumeVariable *volVar)
{
	for (int i = 0; i < (int)volumeVarContextList.size(); i ++) {
		if (volumeVarContextList[i]->getVar() == volVar) {
			return volumeVarContextList[i];
		}
	}
	return 0;
}

VolumeRegionVarContextExpression* Feature::getVolumeRegionVarContext(VolumeRegionVariable *volRegionVar)
{
	for (int i = 0; i < (int)volumeRegionVarContextList.size(); i ++) {
		if (volumeRegionVarContextList[i]->getVar() == volRegionVar) {
			return volumeRegionVarContextList[i];
		}
	}
	return 0;
}

