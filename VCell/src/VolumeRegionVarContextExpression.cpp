/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VolumeRegionVarContextExpression.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/VolumeRegion.h>
#include <Expression.h>
#include <SimpleSymbolTable.h>

VolumeRegionVarContextExpression:: VolumeRegionVarContextExpression(Feature *feature, VolumeRegionVariable* var) 
: VolumeRegionVarContext(feature, var)
{
}

void VolumeRegionVarContextExpression::resolveReferences(Simulation* sim) {
	VarContext::resolveReferences(sim);
	bindAll((SimulationExpression*)sim);
}

double VolumeRegionVarContextExpression::getInitialValue(long regionIndex) {
	return evaluateVolumeRegionExpression(regionIndex, INITIAL_VALUE_EXP);
}

double VolumeRegionVarContextExpression::getReactionRate(long volumeIndex) {
	return evaluateExpression(volumeIndex, REACT_RATE_EXP);
}

double VolumeRegionVarContextExpression::getUniformRate(VolumeRegion *region){
	return evaluateVolumeRegionExpression(region->getIndex(), UNIFORM_RATE_EXP);
}

double VolumeRegionVarContextExpression::getFlux(MembraneElement *element){
	return evaluateJumpCondition(element);
}

bool VolumeRegionVarContextExpression::isNullExpressionOK(int expIndex) {
	if (expIndex == INITIAL_VALUE_EXP || expIndex == REACT_RATE_EXP || expIndex == UNIFORM_RATE_EXP || expIndex == FLUX_EXP) {
		return false;
	}
	return true;
}
