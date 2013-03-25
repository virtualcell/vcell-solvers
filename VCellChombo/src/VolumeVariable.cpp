/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VolumeVariable.h>
#include <VCELL/Feature.h>
#include <fstream>
using std::ofstream;
using std::endl;

VolumeVariable::VolumeVariable(string& nameStr, Feature* feature, long numX, long numY, long numZ, bool diff, bool advect)
: Variable(nameStr, feature, numX*numY*numZ, diff)
{
	sizeX = numX;
	sizeY = numY;
	sizeZ = numZ;
	bAdvecting = advect;
}

void VolumeVariable::createErrorVariables()
{
	if (exactErrorVar == NULL)
	{
		string errorVarName = name + ERROR_VAR_SUFFIX;
		exactErrorVar = new VolumeVariable(errorVarName, (Feature*)structure, sizeX, sizeY, sizeZ, bDiffusing, bAdvecting);
		errorVarName = name + RELATIVE_ERROR_VAR_SUFFIX;
		relativeErrorVar = new VolumeVariable(errorVarName, (Feature*)structure, sizeX, sizeY, sizeZ, bDiffusing, bAdvecting);
	}
}