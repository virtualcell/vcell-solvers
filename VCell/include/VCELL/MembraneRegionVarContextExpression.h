/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef MEMBRANEREGIONVARCONTEXTEXPRESSION_H
#define MEMBRANEREGIONVARCONTEXTEXPRESSION_H

#include <VCELL/VarContext.h>

class Membrane;
class MembraneRegionVariable;
class MembraneRegion;
struct MembraneElement;

class MembraneRegionVarContextExpression : public VarContext
{
public:
	double  getInitialValue(long index);
    double  getMembraneReactionRate(MembraneElement *element);
    double  getUniformRate(MembraneRegion *region);

    MembraneRegionVarContextExpression(Membrane *membrane, MembraneRegionVariable* var);
	void resolveReferences(Simulation *sim);

protected:
	bool isNullExpressionOK(int expIndex);
};

#endif
