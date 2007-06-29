/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef MEMBRANEREGIONVARCONTEXTEXPRESSION_H
#define MEMBRANEREGIONVARCONTEXTEXPRESSION_H

#include <VCELL/MembraneRegionVarContext.h>

class MembraneRegionVarContextExpression : public MembraneRegionVarContext
{
public:
    virtual double  getMembraneReactionRate(MembraneElement *element);
    virtual double  getUniformRate(MembraneRegion *region);
    virtual void    getFlux(MembraneElement *element, double *inFlux, double *outFlux);

    MembraneRegionVarContextExpression(Feature *feature, string& speciesName);

private:
	double getRegionValue(MembraneRegion *region, long expIndex);	
};

#endif
