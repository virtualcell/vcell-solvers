/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef VOLUMEREGIONVARCONTEXT_H
#define VOLUMEREGIONVARCONTEXT_H

#include <VCELL/VarContext.h>

class VolumeRegionVarContext : public VarContext
{
public:
    virtual double  getReactionRate(long volumeIndex)=0;
    virtual double  getUniformRate(VolumeRegion *region)=0;
    virtual void    getFlux(MembraneElement *element, 
                            double *inFlux, double *outFlux)=0;
    bool hasUniformRate() {return uniformRate; }			     
    bool hasUniformFlux() {return uniformFlux; }			     
    bool hasZeroFluxBoundary() {return uniformFlux; }	

protected:
    VolumeRegionVarContext(Feature *feature, string& speciesName);
    bool uniformFlux;
    bool uniformRate;
    bool zeroFluxBoundary;
};

#endif
