/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <math.h>
#include <VCELL/MembraneVarContext.h>
#include <VCELL/Element.h>
#include <VCELL/Solver.h>
#include <VCELL/Variable.h>
#include <VCELL/Feature.h>
#include <VCELL/EqnBuilder.h>
#include <VCELL/Simulation.h>
#include <VCELL/VarContext.h>
#include <VCELL/Region.h>
#include <VCELL/CartesianMesh.h>

MembraneVarContext::MembraneVarContext(Feature *Afeature, string& AspeciesName)
: VarContext(Afeature, AspeciesName)
{
}

double MembraneVarContext::getInitialValue(MembraneElement *membraneElement)
{
	if (initialValue){
		return *initialValue;
	}
	throw "Application Error: neither initialValue nor getInitialValue() specified for MembraneVarContext";
}
