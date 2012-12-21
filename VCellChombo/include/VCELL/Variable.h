/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
//--------------------------------------------
// Variable.h
//--------------------------------------------
#ifndef VARIABLE_H
#define VARIABLE_H

#include <string>
using std::string;
using std::ofstream;

#define ERROR_VAR_SUFFIX "__error"

class Structure;
enum VariableType {
	VAR_UNKNOWN =			0,
	VAR_VOLUME =			1,
	VAR_MEMBRANE =			2,
	VAR_CONTOUR =			3,
	VAR_VOLUME_REGION =		4,
	VAR_MEMBRANE_REGION=	5,
	VAR_CONTOUR_REGION =	6
} ;

class VarContext;
class Variable 
{
protected:
	Variable(string& nameStr, Structure* structure, long Asize, bool diff=false);

public:
	virtual ~Variable();

//	double *getOld()  { return old; }
	double *getCurr() { return curr; }
	long getSize() { return size; }
	const string& getName() { return name; }
	string getQualifiedName();
	virtual VariableType getVarType() {	return VAR_UNKNOWN; }
	bool isDiffusing() { return bDiffusing; }
	
	Structure* getStructure()
	{
		return structure;
	}

	void setVarContext(VarContext* vc)
	{
		varContext = vc;
	}
	VarContext* getVarContext()
	{
		return varContext;
	}
	Variable* getExactErrorVariable()
	{
		return exactErrorVar;
	}

	virtual Variable* createExactErrorVariable()=0;

	void addL2Error(double d);
	void addL2Exact(double d);
	void addMean(double d);
	void addVolFrac(double d);
	void computeFinalL2Error();
	void computeFinalMean();
	double getL2Error()
	{
		return l2Error;
	}
	double getMean()
	{
		return mean;
	}
	double getSumVolFrac()
	{
		return sumVolFrac;
	}
	void updateMaxError(double d);
	double getMaxError()
	{
		return maxError;
	}
	void reset();

protected:
	string name;
	Structure* structure;
	    
	long size;
//	double *old;
	double *curr;

	bool bDiffusing;
	VarContext* varContext;
	Variable* exactErrorVar;
	double l2Error;
	double maxError;
	double l2Exact;
	double mean;
	double sumVolFrac;
};

#endif
