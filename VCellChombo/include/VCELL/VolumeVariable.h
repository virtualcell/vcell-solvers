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
	VolumeVariable(string& nameStr, Feature* feature, long sizeX, long sizeY, long sizeZ, bool diff=true, bool advect=false);

	virtual VariableType	getVarType() {return VAR_VOLUME;}

	virtual void show(ofstream& fp);

	long getSizeX() { return sizeX;} 
	long getSizeY() { return sizeY;} 
	long getSizeZ() { return sizeZ;} 
	bool isAdvecting() { return bAdvecting; }

	Variable* createExactErrorVariable();
	
protected:
	long    sizeX;
	long    sizeY;
	long    sizeZ;
	
	bool bAdvecting;
	bool bHasGradient;
};

#endif
