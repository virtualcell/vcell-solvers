/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VolumeRegionVarContextExpression.h>
#include <VCELL/Simulation.h>
#include <VCELL/VolumeRegion.h>
#include <Expression.h>

VolumeRegionVarContextExpression:: VolumeRegionVarContextExpression(Feature *feature, string& speciesName) 
: VolumeRegionVarContext(feature, speciesName)
{
}

double VolumeRegionVarContextExpression::getReactionRate(long volumeIndex) {
	return getValue(volumeIndex, REACT_RATE_EXP);
}

double VolumeRegionVarContextExpression::getUniformRate(VolumeRegion *region){
	return getRegionValue(region, UNIFORM_RATE_EXP);
}

void VolumeRegionVarContextExpression::getFlux(MembraneElement *element, double *inFlux, double *outFlux){
	*inFlux = getValue(element, IN_FLUX_EXP);
	*outFlux = getValue(element, OUT_FLUX_EXP);
}


double VolumeRegionVarContextExpression::getRegionValue(VolumeRegion *region, long expIndex)
{
	int* indices = sim->getIndices();
	indices[VAR_VOLUME_REGION_INDEX] = region->getId();
	return expressions[expIndex]->evaluateProxy();	
}
