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
	VolumeVariable(string& nameStr, Feature* feature, long sizeX, long sizeY, long sizeZ, bool diff=true, bool bAdvecting=false);

	virtual VariableType	getVarType() {return VAR_VOLUME;}

	long getSizeX() { return sizeX;} 
	long getSizeY() { return sizeY;} 
	long getSizeZ() { return sizeZ;}

	bool isAdvecting()
	{
		return bAdvecting;
	}

	void createErrorVariables();
	
protected:
	long    sizeX;
	long    sizeY;
	long    sizeZ;
	bool bAdvecting;
};

#endif
