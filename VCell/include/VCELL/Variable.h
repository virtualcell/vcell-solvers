/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
//--------------------------------------------
// Variable.h
//--------------------------------------------
#ifndef VARIABLE_H
#define VARIABLE_H

#include <stdio.h>
#include <VCELL/SimTypes.h>
#include <string>
using namespace std;

class Variable 
{
protected:
	Variable(long size, string& name, string& units);

public:
	virtual ~Variable();

	virtual bool isVolumeVar() { return false; }
	virtual bool isMembraneVar() { return false; }
	virtual bool isContourVar() { return false; }
	virtual bool isVolumeRegionVar() { return false; }
	virtual bool isMembraneRegionVar() { return false; }
	virtual bool isContourRegionVar() { return false; }

	virtual void show(ofstream&);

	double *getOld()  { return old; }
	double *getCurr() { return curr; }
	long    getSize() { return size; }

	double  getOld(long index);           // if doesn't exist, return 0.0
	double  getCurr(long index);          // if doesn't exist, return 0.0

	void    setOld(long index, double value);
	void    setCurr(long index, double value);

	string	getName() { return name; }
	virtual VariableType	getVarType() {return VAR_UNKNOWN;}
	string getUnits() { return units; }

	double   getMaxDifference();
	void     update();
	void     revert();
	void     clear();

	Variable *next; 
   
protected:
	string name;
	string units;
	    
	long    size;
	double *old;
	double *curr;
};

#endif
