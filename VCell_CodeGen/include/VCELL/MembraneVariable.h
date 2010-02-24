/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef MEMBRANEVARIABLE_H
#define MEMBRANEVARIABLE_H

#include <VCELL/Variable.h>

class MembraneVariable : public Variable
{
public:
	MembraneVariable(long size, string& nameStr, string& Aunits, bool pde=false);

	virtual bool isMembraneVar() { return true; }
	virtual VariableType	getVarType() {return VAR_MEMBRANE;}
};

#endif
