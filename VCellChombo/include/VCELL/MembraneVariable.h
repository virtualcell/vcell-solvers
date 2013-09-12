/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef MEMBRANEVARIABLE_H
#define MEMBRANEVARIABLE_H

#include <VCELL/Variable.h>

class Membrane;

class MembraneVariable : public Variable
{
public:
	virtual ~MembraneVariable();
	MembraneVariable(string& nameStr, Membrane* membrane, long size);

	VariableType getVarType() { return VAR_MEMBRANE; }

	void createErrorVariables();
	double* getOld()
	{
		return old;
	}
	void update();

protected:
	MembraneVariable* clone(string& varName);

private:
	double* old;
};

#endif
