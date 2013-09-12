/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */

#include <VCELL/MembraneVariable.h>
#include <VCELL/Membrane.h>
#include <string.h>
#include <REAL.H>

MembraneVariable::MembraneVariable(string& nameStr, Membrane* membrane, long size)
: Variable(nameStr, membrane, size)
{
	old = new double[size];
	for (int i = 0; i < size; ++ i)
	{
		old[i] = BASEFAB_REAL_SETVAL;
	}
}

MembraneVariable::~MembraneVariable()
{
	delete[] old;
}

void MembraneVariable::update()
{
	memcpy(old, curr, sizeof(double)*size);
}

MembraneVariable* MembraneVariable::clone(string& varName)
{
	MembraneVariable* newVar = new MembraneVariable(varName, (Membrane*)structure, size);
	newVar->bDiffusing = bDiffusing;
	newVar->bElliptic = bElliptic;
	return newVar;
}

void MembraneVariable::createErrorVariables()
{
	if (exactErrorVar == NULL)
	{
		string errorVarName = name + ERROR_VAR_SUFFIX;
		exactErrorVar = clone(errorVarName);
		errorVarName = name + RELATIVE_ERROR_VAR_SUFFIX;
		relativeErrorVar = clone(errorVarName);
	}
}