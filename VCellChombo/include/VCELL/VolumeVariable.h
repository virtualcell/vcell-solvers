/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef VOLUMEVARIABLE_H
#define VOLUMEVARIABLE_H

#include <VCELL/Variable.h>

class VolumeRegion;
class Feature;

class VolumeVariable : public Variable
{
public:
	VolumeVariable(string& nameStr, Feature* feature, long size);

	virtual VariableType	getVarType() {return VAR_VOLUME;}

	bool isAdvecting()
	{
		return bAdvecting;
	}

	void createErrorVariables();
	
private:
	VolumeVariable* clone(string& varName);
	bool bAdvecting;
};

#endif
