/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef MEMBRANEREGIONVARIABLE_H
#define MEMBRANEREGIONVARIABLE_H

#include <VCELL/Variable.h>

class MembraneRegionVariable : public Variable
{
public:
	MembraneRegionVariable(int size, string& nameStr, string& Aunits);

	virtual bool isMembraneRegionVar() { return true; }
	virtual VariableType	getVarType() {return VAR_MEMBRANE_REGION;}
};

#endif
