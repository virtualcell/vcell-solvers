/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef MEMBRANEREGIONVARCONTEXT_H
#define MEMBRANEREGIONVARCONTEXT_H

#include <VCELL/VarContext.h>

class MembraneRegionVarContext : public VarContext
{
public:
    virtual double  getMembraneReactionRate(MembraneElement *element)=0;
    virtual double  getUniformRate(MembraneRegion *region)=0;
    virtual void    getFlux(MembraneElement *element, double *inFlux, double *outFlux)=0;
    bool hasUniformRate() {return uniformRate; }			     
    bool hasUniformFlux() {return uniformFlux; }			     
    bool hasZeroFluxBoundary() {return uniformFlux; }	

protected:
    MembraneRegionVarContext(Feature *feature, string& speciesName);
    bool uniformFlux;
    bool uniformRate;
    bool zeroFluxBoundary;     
};

#endif
