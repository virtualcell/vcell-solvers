/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef MEMBRANEVARCONTEXT_H
#define MEMBRANEVARCONTEXT_H

#include <VCELL/VarContext.h>

class MembraneVarContext : public VarContext
{
public:
    virtual double  getInitialValue(MembraneElement *element);
    virtual double  getMembraneReactionRate(MembraneElement *element)=0;
    virtual double  getMembraneDiffusionRate(MembraneElement *element)=0;

    virtual double getXmBoundaryValue(MembraneElement *element) { return getInitialValue(element); }
    virtual double getXpBoundaryValue(MembraneElement *element) { return getInitialValue(element); }
    virtual double getYmBoundaryValue(MembraneElement *element) { return getInitialValue(element); }
    virtual double getYpBoundaryValue(MembraneElement *element) { return getInitialValue(element); }
    virtual double getZmBoundaryValue(MembraneElement *element) { return getInitialValue(element); }
    virtual double getZpBoundaryValue(MembraneElement *element) { return getInitialValue(element); }

    virtual double getXmBoundaryFlux(MembraneElement *element) { return 0.0; }
    virtual double getXpBoundaryFlux(MembraneElement *element) { return 0.0; }
    virtual double getYmBoundaryFlux(MembraneElement *element) { return 0.0; }
    virtual double getYpBoundaryFlux(MembraneElement *element) { return 0.0; }
    virtual double getZmBoundaryFlux(MembraneElement *element) { return 0.0; }
    virtual double getZpBoundaryFlux(MembraneElement *element) { return 0.0; }

protected:
    MembraneVarContext(Feature *feature, string& speciesName);
};

#endif
