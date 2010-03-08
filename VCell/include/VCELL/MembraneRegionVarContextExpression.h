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

    MembraneRegionVarContextExpression(Membrane *membrane, MembraneRegionVariable* var);
	void resolveReferences(Simulation *sim);

private:
	double evaluateRegionExpression(MembraneRegion *region, long expIndex);
	double evaluateRegionExpression(long regionIndex, long expIndex);

protected:
	bool isNullExpressionOK(int expIndex);
};

#endif
