/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VolumeVariable.h>
#include <VCELL/Feature.h>
#include <fstream>
using std::ofstream;
using std::endl;

VolumeVariable::VolumeVariable(string& nameStr, Feature* feature, long size)
: Variable(nameStr, feature, size)
{
	bAdvecting = false;
}

VolumeVariable* VolumeVariable::clone(string& varName)
{
	VolumeVariable* newVar = new VolumeVariable(varName, (Feature*)structure, size);
	newVar->bDiffusing = bDiffusing;
	newVar->bElliptic = bElliptic;
	newVar->bAdvecting = bAdvecting;
	return newVar;
}

void VolumeVariable::createErrorVariables()
{
	if (exactErrorVar == NULL)
	{
		string errorVarName = name + ERROR_VAR_SUFFIX;
		exactErrorVar = clone(errorVarName);
		errorVarName = name + RELATIVE_ERROR_VAR_SUFFIX;
		relativeErrorVar = clone(errorVarName);
	}
}