/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef VOLUMEREGIONVARCONTEXTEXPRESSION_H
#define VOLUMEREGIONVARCONTEXTEXPRESSION_H

#include <VCELL/VolumeRegionVarContext.h>

class VolumeRegionVarContextExpression : public VolumeRegionVarContext
{
public:
	void resolveReferences(Simulation *sim);

	double getInitialValue(long index);
    double getReactionRate(long volumeIndex);
    double getUniformRate(VolumeRegion *region);
    void  getFlux(MembraneElement *element, double *inFlux, double *outFlux);

    VolumeRegionVarContextExpression(Feature *feature, string& speciesName);

private:
	double getRegionValue(VolumeRegion *region, long expIndex);	
	double getIndexValue(long regionIndex, long expIndex);	

protected:
	bool isNullExpressionOK(int expIndex);
};

#endif
