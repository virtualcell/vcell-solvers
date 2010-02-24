/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/SimTypes.h>
#include <VCELL/Element.h>
#include <VCELL/Solver.h>
#include <VCELL/VolumeVariable.h>
#include <VCELL/VolumeRegionVariable.h>
#include <VCELL/MembraneVariable.h>
#include <VCELL/MembraneRegionVariable.h>
#include <VCELL/Simulation.h>
#include <VCELL/Feature.h>
#include <VCELL/VolumeVarContext.h>
#include <VCELL/VolumeRegionVarContext.h>
#include <VCELL/MembraneVarContext.h>
#include <VCELL/MembraneRegionVarContext.h>
#include <VCELL/ParticleContext.h>
#include <VCELL/FastSystem.h>

//-----------------------------------------------------------------
//
//  class Feature
//
//-----------------------------------------------------------------
Feature::Feature(string& Aname, FeatureHandle Ahandle, int Priority)
{
	priority = Priority;
	handle = Ahandle;
	name = Aname;

	vpc = NULL;
	mpc = NULL;
	cpc = NULL;
	   
	fastSystem = NULL;
	membraneFastSystem = NULL;

	for (int i = 0; i < 6; i ++) {
		boundaryType[i] = BOUNDARY_VALUE;
	}
}

Feature::~Feature()
{
}

double Feature::getMaxIterationTime()
{
	return 0.0;
}

void Feature::resolveReferences(Simulation *sim)
{	
	for (int i = 0; i < (int)volumeVarContextList.size(); i ++) {
		VolumeVarContext* volumeVarContext = volumeVarContextList[i];
		volumeVarContext->resolveReferences(sim);
	}
	
	for (int i = 0; i < (int)membraneVarContextList.size(); i ++) {
		MembraneVarContext *membraneVarContext = membraneVarContextList[i];
		membraneVarContext->resolveReferences(sim);
	}
	
	for (int i = 0; i < (int)volumeRegionVarContextList.size(); i ++) {
		VolumeRegionVarContext *volumeRegionVarContext = volumeRegionVarContextList[i];
		volumeRegionVarContext->resolveReferences(sim);
	}    

	for (int i = 0; i < (int)membraneRegionVarContextList.size(); i ++) {
		MembraneRegionVarContext* membraneRegionVarContext = membraneRegionVarContextList[i];
		membraneRegionVarContext->resolveReferences(sim);
	}    

	if(vpc!=NULL){
		vpc->resolveReferences(sim);
	}
	if(mpc!=NULL){
		mpc->resolveReferences(sim);
	}
	if(cpc!=NULL){
		cpc->resolveReferences(sim);
	}
	if(fastSystem!=NULL){
		fastSystem->resolveReferences(sim);
	}
	if(membraneFastSystem!=NULL){
		membraneFastSystem->resolveReferences(sim);
	}
}

void Feature::initVolumeValues(long volumeIndex)
{
	for (int i = 0; i < (int)volumeVarContextList.size(); i ++) {
		VolumeVarContext *volumeVarContext = volumeVarContextList[i];
		double value = volumeVarContext->getInitialValue(volumeIndex);
		VolumeVariable* var = (VolumeVariable *)volumeVarContext->getVar();
		var->setOld(volumeIndex, value);
		var->setCurr(volumeIndex, value);
	}
}

void Feature::initMembraneValues(MembraneElement *membraneElement)
{
	for (int i = 0; i < (int)membraneVarContextList.size(); i ++) {
		MembraneVarContext *membraneVarContext = membraneVarContextList[i];
		double value = membraneVarContext->getInitialValue(membraneElement);
		MembraneVariable* var = (MembraneVariable *)membraneVarContext->getVar();
		var->setOld(membraneElement->index, value);
		var->setCurr(membraneElement->index, value);
	}
}

void Feature::initVolumeRegionValues(int volumeRegionIndex)
{
	for (int i = 0; i < (int)volumeRegionVarContextList.size(); i ++) {
		VolumeRegionVarContext *volumeRegionVarContext = volumeRegionVarContextList[i];
		double value = volumeRegionVarContext->getInitialValue(volumeRegionIndex);
		VolumeRegionVariable* var = (VolumeRegionVariable*)volumeRegionVarContext->getVar();
		var->setOld(volumeRegionIndex, value);
		var->setCurr(volumeRegionIndex, value);
	}
}

void Feature::initMembraneRegionValues(int membraneRegionIndex)
{
	for (int i = 0; i < (int)membraneRegionVarContextList.size(); i ++) {
		MembraneRegionVarContext *membraneRegionVarContext = membraneRegionVarContextList[i];
		double value = membraneRegionVarContext->getInitialValue(membraneRegionIndex);
		MembraneRegionVariable* var = (MembraneRegionVariable *)membraneRegionVarContext->getVar();
		var->setOld(membraneRegionIndex, value);
		var->setCurr(membraneRegionIndex, value);
	}
}

void Feature::addVolumeVarContext(VolumeVarContext *vvc)
{
	volumeVarContextList.push_back(vvc);
}

void Feature::addMembraneVarContext(MembraneVarContext *mvc)
{
	membraneVarContextList.push_back(mvc);
}

void Feature::addVolumeRegionVarContext(VolumeRegionVarContext *vrvc)
{
	volumeRegionVarContextList.push_back(vrvc);
}

void Feature::addMembraneRegionVarContext(MembraneRegionVarContext *mrvc)
{
	membraneRegionVarContextList.push_back(mrvc);
}

FeatureHandle Feature::getHandle()
{
	return handle;
}


VolumeVarContext* Feature::getVolumeVarContext(VolumeVariable *volVar)
{
	for (int i = 0; i < (int)volumeVarContextList.size(); i ++) {
		if (volumeVarContextList[i]->getVar() == volVar) {
			return volumeVarContextList[i];
		}
	}
	return 0;
}


VolumeVarContext* Feature::getVolumeVarContext(string& volumeVarName)
{
	for (int i = 0; i < (int)volumeVarContextList.size(); i ++) {
		if (volumeVarContextList[i]->getVarName() == volumeVarName) {
			return volumeVarContextList[i];
		}
	}
	return 0;
}


MembraneVarContext* Feature::getMembraneVarContext(MembraneVariable *memVar)
{
	for (int i = 0; i < (int)membraneVarContextList.size(); i ++) {
		if (membraneVarContextList[i]->getVar() == memVar) {
			return membraneVarContextList[i];
		}
	}
	return 0;
}


MembraneVarContext* Feature::getMembraneVarContext(string& membraneVarName)
{
	for (int i = 0; i < (int)membraneVarContextList.size(); i ++) {
		if (membraneVarContextList[i]->getVarName() == membraneVarName) {
			return membraneVarContextList[i];
		}
	}
	return 0;
}

VolumeRegionVarContext* Feature::getVolumeRegionVarContext(VolumeRegionVariable *volRegionVar)
{
	for (int i = 0; i < (int)volumeRegionVarContextList.size(); i ++) {
		if (volumeRegionVarContextList[i]->getVar() == volRegionVar) {
			return volumeRegionVarContextList[i];
		}
	}
	return 0;
}

MembraneRegionVarContext* Feature::getMembraneRegionVarContext(MembraneRegionVariable *memRegionVar)
{
	for (int i = 0; i < (int)membraneRegionVarContextList.size(); i ++) {
		if (membraneRegionVarContextList[i]->getVar() == memRegionVar) {
			return membraneRegionVarContextList[i];
		}
	}
	return 0;
}
