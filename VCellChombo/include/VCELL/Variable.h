/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
//--------------------------------------------
// Variable.h
//--------------------------------------------
#ifndef VARIABLE_H
#define VARIABLE_H

#include <VCELL/Structure.h>
#include <string>
using std::string;
using std::ofstream;

#define ERROR_VAR_SUFFIX "__error"
#define RELATIVE_ERROR_VAR_SUFFIX "__relative_error"

enum VariableType {
	VAR_UNKNOWN =			0,
	VAR_VOLUME =			1,
	VAR_MEMBRANE =			2,
	VAR_CONTOUR =			3,
	VAR_VOLUME_REGION =		4,
	VAR_MEMBRANE_REGION=	5,
	VAR_CONTOUR_REGION =	6
} ;
class Membrane;

class VarContext;
class Variable 
{
public:
	virtual ~Variable();

	double *getCurr();
	long getSize() { return size; }
	const string& getName() { return name; }
	string getQualifiedName();
	virtual VariableType getVarType() {	return VAR_UNKNOWN; }
	bool isDiffusing()
	{
		return bDiffusing;
	}
	bool isElliptic()
	{
		return bElliptic;
	}
	void setDiffusing()
	{
		 bDiffusing = true;
	}
	void setElliptic()
	{
		bElliptic = true;
	}
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

#ifndef CH_MPI
	Variable* getExactErrorVariable()
	{
		return exactErrorVar;
	}

	Variable* getRelativeErrorVariable()
	{
		return relativeErrorVar;
	}

	virtual void createErrorVariables()=0;
	
	void addL2Error(double d);
	void addL2Exact(double d);
	void addTotalVCell(double d);
	
	double getL2Error()
	{
		return l2Error;
	}
	void updateMaxError(double d);
	double getMaxError()
	{
		return maxError;
	}
#endif
	
	double getTotal()
	{
		return total;
	}
	double getMean()
	{
		return mean;
	}

	void reset(bool bSaveVCellOutput);
	void addTotal(double d);
	void computeFinalStatistics();
	
protected:
	Variable(string& nameStr, Structure* structure, long Asize);

	string name;
	Structure* structure;
	    
	double *curr;
	long size;

	bool bDiffusing;
	bool bElliptic;
	VarContext* varContext;

#ifndef CH_MPI
	Variable* exactErrorVar;
	Variable* relativeErrorVar;
	double l2Error;
	double maxError;
	double l2Exact;
	double meanVCell;
	double totalVCell;
#endif
	
	double mean;
	double total;
};

#endif
