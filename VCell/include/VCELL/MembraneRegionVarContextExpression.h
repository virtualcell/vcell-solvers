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
	double  getInitialValue(long index);
    double  getMembraneReactionRate(MembraneElement *element);
    double  getUniformRate(MembraneRegion *region);
    void    getFlux(MembraneElement *element, double *inFlux, double *outFlux);

    MembraneRegionVarContextExpression(Feature *feature, string& speciesName);
	bool resolveReferences(Simulation *sim);

private:
	double getRegionValue(MembraneRegion *region, long expIndex);
	double getIndexValue(long regionIndex, long expIndex);

protected:
	bool isNullExpressionOK(int expIndex);
};

#endif
