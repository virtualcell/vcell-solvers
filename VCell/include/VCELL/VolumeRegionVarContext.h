/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef VOLUMEREGIONVARCONTEXT_H
#define VOLUMEREGIONVARCONTEXT_H

#include <VCELL/VarContext.h>

class VolumeRegion;
class VolumeRegionVariable;
class Feature;

class VolumeRegionVarContext : public VarContext
{
public:
    virtual double  getReactionRate(long volumeIndex)=0;
    virtual double  getUniformRate(VolumeRegion *region)=0;
    virtual double  getFlux(MembraneElement *element)=0;

protected:
    VolumeRegionVarContext(Feature *feature, VolumeRegionVariable* var);
};

#endif
