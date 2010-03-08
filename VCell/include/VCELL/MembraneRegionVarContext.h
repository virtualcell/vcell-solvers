/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef MEMBRANEREGIONVARCONTEXT_H
#define MEMBRANEREGIONVARCONTEXT_H

#include <VCELL/VarContext.h>

class Membrane;
class MembraneRegion;
class MembraneRegionVariable;

class MembraneRegionVarContext : public VarContext
{
public:
    virtual double  getMembraneReactionRate(MembraneElement *element)=0;
    virtual double  getUniformRate(MembraneRegion *region)=0;

protected:
    MembraneRegionVarContext(Membrane *membrane, MembraneRegionVariable* var);
};

#endif
