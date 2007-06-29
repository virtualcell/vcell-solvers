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
	next = NULL;
	name = Aname;
	   
	currVolumeVarContext = NULL;
	volumeVarContextList = NULL;
	currMembraneVarContext = NULL;
	membraneVarContextList = NULL;

	currVolumeRegionVarContext = NULL;
	volumeRegionVarContextList = NULL;
	currMembraneRegionVarContext = NULL;
	membraneRegionVarContextList = NULL;

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

bool Feature::resolveReferences(Simulation *sim)
{
	//
	// initialize VarContexts
	//
	VolumeVarContext *volumeVarContext=NULL;
	while (volumeVarContext = getNextVolumeVarContext(volumeVarContext)){
		if (!volumeVarContext->resolveReferences(sim)){
			printf("Feature::resolveReferences(), error \n");
			return false;
		}
	}    

	MembraneVarContext *membraneVarContext=NULL;
	while (membraneVarContext = getNextMembraneVarContext(membraneVarContext)){
		if (!membraneVarContext->resolveReferences(sim)){
			printf("Feature::resolveReferences(), error \n");
			return false;
		}
	}    

	VolumeRegionVarContext *volumeRegionVarContext=NULL;
	while (volumeRegionVarContext = getNextVolumeRegionVarContext(volumeRegionVarContext)){
		if (!volumeRegionVarContext->resolveReferences(sim)){
			printf("Feature::resolveReferences(), error \n");
			return false;
		}
	}    

	MembraneRegionVarContext *membraneRegionVarContext=NULL;
	while (membraneRegionVarContext = getNextMembraneRegionVarContext(membraneRegionVarContext)){
		if (!membraneRegionVarContext->resolveReferences(sim)){
			printf("Feature::resolveReferences(), error \n");
			return false;
		}
	}    

	//
	// initialize ParticleContexts
	//
	if(vpc!=NULL){
		if (!vpc->resolveReferences(sim)){
			printf("Feature::resolveReferences(), error \n");
			return false;
		}
	}
	if(mpc!=NULL){
		if (!mpc->resolveReferences(sim)){
			printf("Feature::resolveReferences(), error \n");
			return false;
		}
	}
	if(cpc!=NULL){
		if (!cpc->resolveReferences(sim)){
			printf("Feature::resolveReferences(), error \n");
			return false;
		}
	}
	//
	// initialize FastSystem
	//
	if(fastSystem!=NULL){
		if(!fastSystem->resolveReferences(sim)){
			printf("Feature::resolveReferences(), error resolving FastSystem\n");
			return false;
		}
	}
	//
	// initialize MembraneFastSystem
	//
	if(membraneFastSystem!=NULL){
		if(!membraneFastSystem->resolveReferences(sim)){
			printf("Feature::resolveReferences(), error resolving MembraneFastSystem\n");
			return false;
		}
	}
	    
	return true;
}

bool Feature::initVolumeValues(long volumeIndex)
{
	VolumeVariable *var;

	//
	// initialize VolumeVariables
	//
	VolumeVarContext *volumeVarContext=NULL;
	while (volumeVarContext = getNextVolumeVarContext(volumeVarContext)){
		double value = volumeVarContext->getInitialValue(volumeIndex);
		var = (VolumeVariable *)volumeVarContext->getVar();
		var->setOld(volumeIndex, value);
		var->setCurr(volumeIndex, value);
	}
	return true;
}

bool Feature::initMembraneValues(MembraneElement *membraneElement)
{
	MembraneVariable *var;
	long membraneIndex = membraneElement->index;
	//
	// initialize MembraneVariables
	//
	MembraneVarContext *membraneVarContext=NULL;
	while (membraneVarContext = getNextMembraneVarContext(membraneVarContext)){
		double value = membraneVarContext->getInitialValue(membraneElement);
		var = (MembraneVariable *)membraneVarContext->getVar();
		var->setOld(membraneIndex, value);
		var->setCurr(membraneIndex, value);
	}
	return true;
}

bool Feature::initVolumeRegionValues(int volumeRegionIndex)
{
	VolumeRegionVariable *var;

	//
	// initialize VolumeRegionVariables
	//
	VolumeRegionVarContext *volumeRegionVarContext=NULL;
	while (volumeRegionVarContext = getNextVolumeRegionVarContext(volumeRegionVarContext)){
		double value = volumeRegionVarContext->getInitialValue(volumeRegionIndex);
		var = (VolumeRegionVariable *)volumeRegionVarContext->getVar();
		var->setOld(volumeRegionIndex, value);
		var->setCurr(volumeRegionIndex, value);
	}
	return true;
}

bool Feature::initMembraneRegionValues(int membraneRegionIndex)
{
	MembraneRegionVariable *var;

	//
	// initialize MembraneRegionVariables
	//
	MembraneRegionVarContext *membraneRegionVarContext=NULL;
	while (membraneRegionVarContext = getNextMembraneRegionVarContext(membraneRegionVarContext)){
		double value = membraneRegionVarContext->getInitialValue(membraneRegionIndex);
		var = (MembraneRegionVariable *)membraneRegionVarContext->getVar();
		var->setOld(membraneRegionIndex, value);
		var->setCurr(membraneRegionIndex, value);
	}
	return true;
}

VolumeVarContext *Feature::getNextVolumeVarContext(VolumeVarContext *vc)
{
	if (vc==NULL){
		return volumeVarContextList;
	}else{
		return (VolumeVarContext *)(vc->getNext());
	}
}

MembraneVarContext *Feature::getNextMembraneVarContext(MembraneVarContext *vc)
{
	if (vc==NULL){
		return membraneVarContextList;
	}else{
		return (MembraneVarContext *)(vc->getNext());
	}
}

VolumeRegionVarContext *Feature::getNextVolumeRegionVarContext(VolumeRegionVarContext *vc)
{
	if (vc==NULL){
		return volumeRegionVarContextList;
	}else{
		return (VolumeRegionVarContext *)(vc->getNext());
	}
}

MembraneRegionVarContext *Feature::getNextMembraneRegionVarContext(MembraneRegionVarContext *vc)
{
	if (vc==NULL){
		return membraneRegionVarContextList;
	}else{
		return (MembraneRegionVarContext *)(vc->getNext());
	}
}

void Feature::addVolumeVarContext(VolumeVarContext *vc)
{
	//
	// add 'this' to front of linked list
	//
	currVolumeVarContext = vc;
	vc->next = volumeVarContextList;
	volumeVarContextList = vc;
}

void Feature::addMembraneVarContext(MembraneVarContext *vc)
{
	//
	// add 'this' to front of linked list
	//
	currMembraneVarContext = vc;
	vc->next = membraneVarContextList;
	membraneVarContextList = vc;
}

void Feature::addVolumeRegionVarContext(VolumeRegionVarContext *vc)
{
	//
	// add 'this' to front of linked list
	//
	currVolumeRegionVarContext = vc;
	vc->next = volumeRegionVarContextList;
	volumeRegionVarContextList = vc;
}

void Feature::addMembraneRegionVarContext(MembraneRegionVarContext *vc)
{
	//
	// add 'this' to front of linked list
	//
	currMembraneRegionVarContext = vc;
	vc->next = membraneRegionVarContextList;
	membraneRegionVarContextList = vc;
}

FeatureHandle Feature::getHandle()
{
	return handle;
}


VolumeVarContext *Feature::getVolumeVarContext(VolumeVariable *var)
{
	//
	// check if current varContext already
	//
	if (currVolumeVarContext != NULL){
		if (currVolumeVarContext->getVar()==var){
			return currVolumeVarContext;
		}
	}
	//
	// check if varContext has already been fully created
	//
	currVolumeVarContext = volumeVarContextList;
	while(currVolumeVarContext){
		if (currVolumeVarContext->getVar()==var) break;
		currVolumeVarContext = (VolumeVarContext *)(currVolumeVarContext->getNext());
	}
	ASSERTION(currVolumeVarContext);

	return currVolumeVarContext;
}


VolumeVarContext *Feature::getVolumeVarContext(string& volumeVarName)
{
	//
	// check if current varContext already
	//
	if (currVolumeVarContext != NULL){
		if (currVolumeVarContext->getVarName() == volumeVarName){
			return currVolumeVarContext;
		}
	}
	//
	// check if varContext has already been fully created
	//
	currVolumeVarContext = volumeVarContextList;
	while(currVolumeVarContext){
		if (currVolumeVarContext->getVarName() == volumeVarName) 
			break;
		currVolumeVarContext = (VolumeVarContext *)(currVolumeVarContext->getNext());
	}
	ASSERTION(currVolumeVarContext);

	return currVolumeVarContext;
}


MembraneVarContext *Feature::getMembraneVarContext(MembraneVariable *var)
{
	//
	// check if current varContext already
	//
	if (currMembraneVarContext != NULL){
		if (currMembraneVarContext->getVar()==var){
			return currMembraneVarContext;
		}
	}
	//
	// check if varContext has already been fully created
	//
	currMembraneVarContext = membraneVarContextList;
	while(currMembraneVarContext){
		if (currMembraneVarContext->getVar()==var) break;
		currMembraneVarContext = (MembraneVarContext *)(currMembraneVarContext->getNext());
	}
	ASSERTION(currMembraneVarContext);
	   
	return currMembraneVarContext;
}


MembraneVarContext *Feature::getMembraneVarContext(string& membraneVarName)
{
	//
	// check if current varContext already
	//
	if (currMembraneVarContext != NULL){
		if (currMembraneVarContext->getVarName() == membraneVarName){
			return currMembraneVarContext;
		}
	}
	//
	// check if varContext has already been fully created
	//
	currMembraneVarContext = membraneVarContextList;
	while(currMembraneVarContext){
		if (currMembraneVarContext->getVarName() == membraneVarName) 
			break;
		currMembraneVarContext = (MembraneVarContext *)(currMembraneVarContext->getNext());
	}
	ASSERTION(currMembraneVarContext);
	   
	return currMembraneVarContext;
}

VolumeRegionVarContext *Feature::getVolumeRegionVarContext(VolumeRegionVariable *var)
{
	//
	// check if current varContext already
	//
	if (currVolumeRegionVarContext != NULL){
		if (currVolumeRegionVarContext->getVar()==var){
			return currVolumeRegionVarContext;
		}
	}
	//
	// check if varContext has already been fully created
	//
	currVolumeRegionVarContext = volumeRegionVarContextList;
	while(currVolumeRegionVarContext){
		if (currVolumeRegionVarContext->getVar()==var) break;
		currVolumeRegionVarContext = (VolumeRegionVarContext *)(currVolumeRegionVarContext->getNext());
	}
	ASSERTION(currVolumeRegionVarContext);

	return currVolumeRegionVarContext;
}

MembraneRegionVarContext *Feature::getMembraneRegionVarContext(MembraneRegionVariable *var)
{
	//
	// check if current varContext already
	//
	if (currMembraneRegionVarContext != NULL){
		if (currMembraneRegionVarContext->getVar()==var){
			return currMembraneRegionVarContext;
		}
	}
	//
	// check if varContext has already been fully created
	//
	currMembraneRegionVarContext = membraneRegionVarContextList;
	while(currMembraneRegionVarContext){
		if (currMembraneRegionVarContext->getVar()==var) break;
		currMembraneRegionVarContext = (MembraneRegionVarContext *)(currMembraneRegionVarContext->getNext());
	}
	ASSERTION(currMembraneRegionVarContext);
	   
	return currMembraneRegionVarContext;
}
