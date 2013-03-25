/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */

#include <VCELL/MembraneVariable.h>
#include <VCELL/Membrane.h>

MembraneVariable::MembraneVariable(string& nameStr, Membrane* membrane, long size, bool diff)
: Variable(nameStr, membrane, size, diff)
{
}

void MembraneVariable::createErrorVariables()
{
	if (exactErrorVar == NULL)
	{
		string errorVarName = name + ERROR_VAR_SUFFIX;
		exactErrorVar = new MembraneVariable(errorVarName, (Membrane*)structure, size, bDiffusing);
		errorVarName = name + RELATIVE_ERROR_VAR_SUFFIX;
		relativeErrorVar = new MembraneVariable(errorVarName, (Membrane*)structure, size, bDiffusing);
	}
}