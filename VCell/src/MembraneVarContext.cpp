/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/MembraneVarContext.h>
#include <VCELL/Element.h>
#include <VCELL/Membrane.h>
#include <VCELL/MembraneVariable.h>

MembraneVarContext::MembraneVarContext(Membrane *membrane, MembraneVariable* var)
: VarContext(membrane, var)
{
}

double MembraneVarContext::getInitialValue(MembraneElement *membraneElement)
{
	if (initialValue){
		return *initialValue;
	}
	throw "Application Error: neither initialValue nor getInitialValue() specified for MembraneVarContext";
}
