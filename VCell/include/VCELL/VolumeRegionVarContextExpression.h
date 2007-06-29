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
    virtual double  getReactionRate(long volumeIndex);
    virtual double  getUniformRate(VolumeRegion *region);
    virtual void    getFlux(MembraneElement *element, double *inFlux, double *outFlux);

    VolumeRegionVarContextExpression(Feature *feature, string& speciesName);

private:
	double getRegionValue(VolumeRegion *region, long expIndex);	
};

#endif
