/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/MembraneRegionVarContextExpression.h>
#include <VCELL/Simulation.h>
#include <VCELL/Mesh.h>
#include <VCELL/MembraneRegion.h>
#include <Expression.h>

MembraneRegionVarContextExpression:: MembraneRegionVarContextExpression(Feature *feature, string& speciesName) 
: MembraneRegionVarContext(feature, speciesName)
{
}

double MembraneRegionVarContextExpression::getMembraneReactionRate(MembraneElement* element) {
	return getValue(element, REACT_RATE_EXP);
}

double MembraneRegionVarContextExpression::getUniformRate(MembraneRegion *region){
	return getRegionValue(region, UNIFORM_RATE_EXP);
}

void MembraneRegionVarContextExpression::getFlux(MembraneElement *element, double *inFlux, double *outFlux){
	*inFlux = getValue(element, IN_FLUX_EXP);
	*outFlux = getValue(element, OUT_FLUX_EXP);
}

double MembraneRegionVarContextExpression::getRegionValue(MembraneRegion *region, long expIndex)
{
	int* indices = sim->getIndices();
	indices[VAR_MEMBRANE_REGION_INDEX] = region->getId();
	return expressions[expIndex]->evaluateProxy();	
}
