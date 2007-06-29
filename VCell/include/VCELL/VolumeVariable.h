/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef VOLUMEVARIABLE_H
#define VOLUMEVARIABLE_H

#include <VCELL/Variable.h>

class VolumeVariable : public Variable
{
public:
	VolumeVariable(long sizeX, long sizeY, long sizeZ, string& name, string& units);

	virtual bool isVolumeVar() { return true; }
	virtual VariableType	getVarType() {return VAR_VOLUME;}

	virtual void show(ofstream& fp);

	long getSizeX() { return sizeX;} 
	long getSizeY() { return sizeY;} 
	long getSizeZ() { return sizeZ;} 
	bool getLineX(int y, int z, double *vect, int length); 
	bool getLineY(int x, int z, double *vect, int length); 
	bool getLineZ(int x, int y, double *vect, int length); 
	bool setLineX(int y, int z, double *vect, int length); 
	bool setLineY(int x, int z, double *vect, int length); 
	bool setLineZ(int x, int y, double *vect, int length); 

protected:
	long    sizeX;
	long    sizeY;
	long    sizeZ;
};

#endif
