/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/Contour.h>
#include <VCELL/ContourVarContext.h>
#include <VCELL/ParticleContext.h>
#include <VCELL/FastSystem.h>
#include <VCELL/ContourVariable.h>
#include <string>
using namespace std;

//-----------------------------------------------------------
//
//   class Contour
//
//------------------------------------------------------------
string *Contour::names = NULL;

Contour::Contour(int Atype)
{
	type = Atype;

	currContourVarContext = NULL;
	contourVarContextList = NULL;
	fastSystem = NULL;
}

Contour::~Contour()
{
}

bool Contour::resolveReferences(Simulation *sim)
{
	//
	// initialize ContourVarContexts
	//
	ContourVarContext *contourVarContext=NULL;
	while (contourVarContext = getNextContourVarContext(contourVarContext)){
		if (!contourVarContext->resolveReferences(sim)){
			printf("Contour::resolveReferences(), error \n");
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
	    
	return true;
}

bool Contour::initElementValues(long elementIndex)
{
	ContourVariable *var;

	//
	// initialize ContourVariables
	//
	ContourVarContext *contourVarContext=NULL;
	while (contourVarContext = getNextContourVarContext(contourVarContext)){
		double value = contourVarContext->getInitialValue(elementIndex);
		var = (ContourVariable *)contourVarContext->getVar();
		var->setOld(elementIndex, value);
		var->setCurr(elementIndex, value);
	}
	return true;
}

ContourVarContext *Contour::getContourVarContext(string& contourVarName)
{
	//
	// check if current varContext already
	//
	if (currContourVarContext->getVarName() == contourVarName){
		return currContourVarContext;
	}
	//
	// check if varContext has already been fully created
	//
	currContourVarContext = contourVarContextList;
	while(currContourVarContext){
		if (currContourVarContext->getVarName() == contourVarName) 
			break;
		currContourVarContext = (ContourVarContext *)(currContourVarContext->getNext());
	}
	ASSERTION(currContourVarContext);

	return currContourVarContext;
}

ContourVarContext *Contour::getContourVarContext(ContourVariable *var)
{
	//
	// check if current varContext already
	//
	if (currContourVarContext->getVar()==var){
		return currContourVarContext;
	}
	//
	// check if varContext has already been fully created
	//
	currContourVarContext = contourVarContextList;
	while(currContourVarContext){
		if (currContourVarContext->getVar()==var) break;
		currContourVarContext = (ContourVarContext *)(currContourVarContext->getNext());
	}
	ASSERTION(currContourVarContext);

	return currContourVarContext;
}

ContourVarContext *Contour::getNextContourVarContext(ContourVarContext *vc)
{
	if (vc==NULL){
		return contourVarContextList;
	}else{
		return (ContourVarContext *)(vc->getNext());
	}
}

void Contour::addContourVarContext(ContourVarContext *vc)
{
	//
	// add 'this' to front of linked list
	//
	currContourVarContext = vc;
	vc->next = contourVarContextList;
	contourVarContextList = vc;
}

void Contour::setContourTypes(string *nameArray)
{
	names = nameArray; 
}

//-----------------------------------------------------------
//
//   class ContourElement
//
//------------------------------------------------------------
ContourElement:: ContourElement()
{
	contour = NULL;

	wc_begin.x = 0;
	wc_begin.y = 0;
	wc_begin.z = 0;
	wc_end.x = 0;
	wc_end.y = 0;
	wc_end.z = 0;

	border = CONTOUR_BEGIN;

	elementIndex = 0;
	volumeIndex = 0;
	elementLength = 0;
}

WorldCoord ContourElement::getCoord(double u)
{
	ASSERTION((u>=0)&&(u<=elementLength));
	WorldCoord wc;

	wc.x = wc_begin.x + (wc_end.x - wc_begin.x)*u/elementLength;
	wc.y = wc_begin.y + (wc_end.y - wc_begin.y)*u/elementLength;
	wc.z = wc_begin.z + (wc_end.z - wc_begin.z)*u/elementLength;

	return wc;  
}

double ContourElement::getDistancetoPoint(WorldCoord wc)
{
	double u;

	ASSERTION(!((wc_begin.x==wc_end.x)
			&&(wc_begin.y==wc_end.y)
			&&(wc_begin.z==wc_end.z)));

	u = ((wc_end.x - wc_begin.x)*(wc.x - wc_begin.x)
			+ (wc_end.y - wc_begin.y)*(wc.y - wc_begin.y)
			+ (wc_end.z - wc_begin.z)*(wc.z - wc_begin.z))
			/((wc_end.x - wc_begin.x)*(wc_end.x - wc_begin.x)
			+(wc_end.y - wc_begin.y)*(wc_end.y - wc_begin.y) 
			+(wc_end.z - wc_begin.z)*(wc_end.z - wc_begin.z));
	if(u<0){
		return sqrt((wc_begin.x - wc.x)*(wc_begin.x - wc.x)
				+(wc_begin.y - wc.y)*(wc_begin.y - wc.y)
				+(wc_begin.z - wc.z)*(wc_begin.z - wc.z));
	}else if(u>1){
		return sqrt((wc_end.x - wc.x)*(wc_end.x - wc.x)
				+(wc_end.y - wc.y)*(wc_end.y - wc.y)
				+(wc_end.z - wc.z)*(wc_end.z - wc.z));
	}else{
		u*=getElementLength();
		return  sqrt((getCoord(u).x - wc.x)*(getCoord(u).x - wc.x)
				+(getCoord(u).y - wc.y)*(getCoord(u).y - wc.y)
				+(getCoord(u).z - wc.z)*(getCoord(u).z - wc.z));
	} 
}
