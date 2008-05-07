/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VolumeRegionVarContextExpression.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/VolumeRegion.h>
#include <Expression.h>

VolumeRegionVarContextExpression:: VolumeRegionVarContextExpression(Feature *feature, string& speciesName) 
: VolumeRegionVarContext(feature, speciesName)
{
}

void VolumeRegionVarContextExpression::resolveReferences(Simulation* sim) {
	VarContext::resolveReferences(sim);
	bindAll(((SimulationExpression*)sim)->getOldSymbolTable());
}

double VolumeRegionVarContextExpression::getInitialValue(long regionIndex) {
	return getIndexValue(regionIndex, INITIAL_VALUE_EXP);
}

double VolumeRegionVarContextExpression::getReactionRate(long volumeIndex) {
	return getExpressionValue(volumeIndex, REACT_RATE_EXP);
}

double VolumeRegionVarContextExpression::getUniformRate(VolumeRegion *region){
	return getRegionValue(region, UNIFORM_RATE_EXP);
}

void VolumeRegionVarContextExpression::getFlux(MembraneElement *element, double *inFlux, double *outFlux){
	*inFlux = getExpressionValue(element, IN_FLUX_EXP);
	*outFlux = getExpressionValue(element, OUT_FLUX_EXP);
}


double VolumeRegionVarContextExpression::getRegionValue(VolumeRegion *region, long expIndex)
{
	return getIndexValue(region->getId(), expIndex);
}

double VolumeRegionVarContextExpression::getIndexValue(long regionIndex, long expIndex)
{
	int* indices = ((SimulationExpression*)sim)->getIndices();
	indices[VAR_VOLUME_REGION_INDEX] = regionIndex;
	return expressions[expIndex]->evaluateProxy();	
}

bool VolumeRegionVarContextExpression::isNullExpressionOK(int expIndex) {
	if (expIndex == INITIAL_VALUE_EXP || expIndex == REACT_RATE_EXP || expIndex == UNIFORM_RATE_EXP || expIndex == IN_FLUX_EXP || expIndex == OUT_FLUX_EXP) {
		return false;
	}
	return true;
}
