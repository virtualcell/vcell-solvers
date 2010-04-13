/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef VOLUMEVARIABLE_H
#define VOLUMEVARIABLE_H

#include <VCELL/Variable.h>

class VolumeRegion;

class VolumeVariable : public Variable
{
public:
	VolumeVariable(long sizeX, long sizeY, long sizeZ, string& name, bool diff=true, bool advect=false);

	virtual VariableType	getVarType() {return VAR_VOLUME;}

	virtual void show(ofstream& fp);

	long getSizeX() { return sizeX;} 
	long getSizeY() { return sizeY;} 
	long getSizeZ() { return sizeZ;} 
	bool isAdvecting() { return bAdvecting; }

protected:
	long    sizeX;
	long    sizeY;
	long    sizeZ;
	
	bool bAdvecting;
};

#endif
