/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/MembraneRegionVarContextExpression.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/Mesh.h>
#include <VCELL/MembraneRegion.h>
#include <Expression.h>

MembraneRegionVarContextExpression:: MembraneRegionVarContextExpression(Feature *feature, string& speciesName) 
: MembraneRegionVarContext(feature, speciesName)
{
}

double MembraneRegionVarContextExpression::getMembraneReactionRate(MembraneElement* element) {
	return getExpressionValue(element, REACT_RATE_EXP);
}

bool MembraneRegionVarContextExpression::resolveReferences(Simulation* sim) {
	bool bResolved = VarContext::resolveReferences(sim);
	if (bResolved) {
		bindAll(((SimulationExpression*)sim)->getOldSymbolTable());
	}
	return bResolved;
}

double MembraneRegionVarContextExpression::getInitialValue(long regionIndex) {
	return getIndexValue(regionIndex, INITIAL_VALUE_EXP);
}

double MembraneRegionVarContextExpression::getUniformRate(MembraneRegion *region){
	return getRegionValue(region, UNIFORM_RATE_EXP);
}

void MembraneRegionVarContextExpression::getFlux(MembraneElement *element, double *inFlux, double *outFlux){
	*inFlux = getExpressionValue(element, IN_FLUX_EXP);
	*outFlux = getExpressionValue(element, OUT_FLUX_EXP);
}

double MembraneRegionVarContextExpression::getRegionValue(MembraneRegion *region, long expIndex)
{
	return getIndexValue(region->getId(), expIndex);
}

double MembraneRegionVarContextExpression::getIndexValue(long regionIndex, long expIndex)
{
	int* indices = ((SimulationExpression*)sim)->getIndices();
	indices[VAR_MEMBRANE_REGION_INDEX] = regionIndex;
	return expressions[expIndex]->evaluateProxy();	
}

bool MembraneRegionVarContextExpression::isNullExpressionOK(int expIndex) {
	if (expIndex == INITIAL_VALUE_EXP || expIndex == REACT_RATE_EXP || expIndex == UNIFORM_RATE_EXP) {
		return false;
	}
	return true;
}