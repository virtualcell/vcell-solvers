/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef VOLUMEVARCONTEXT_H
#define VOLUMEVARCONTEXT_H

#include <VCELL/VarContext.h>

class VolumeVarContext : public VarContext
{
public:
    virtual double  getReactionRate(long volumeIndex)=0;
    virtual void    getFlux(MembraneElement *element, 
                            double *inFlux, double *outFlux)=0;

    virtual double getXmBoundaryValue(long index) { return getInitialValue(index); }
    virtual double getXpBoundaryValue(long index) { return getInitialValue(index); }
    virtual double getYmBoundaryValue(long index) { return getInitialValue(index); }
    virtual double getYpBoundaryValue(long index) { return getInitialValue(index); }
    virtual double getZmBoundaryValue(long index) { return getInitialValue(index); }
    virtual double getZpBoundaryValue(long index) { return getInitialValue(index); }

    virtual double getXmBoundaryFlux(long index) { return 0.0; }
    virtual double getXpBoundaryFlux(long index) { return 0.0; }
    virtual double getYmBoundaryFlux(long index) { return 0.0; }
    virtual double getYpBoundaryFlux(long index) { return 0.0; }
    virtual double getZmBoundaryFlux(long index) { return 0.0; }
    virtual double getZpBoundaryFlux(long index) { return 0.0; }

	virtual double getXBoundaryPeriodicConstant() { return 0.0; }
    virtual double getYBoundaryPeriodicConstant() { return 0.0; }
    virtual double getZBoundaryPeriodicConstant() { return 0.0; }	

    virtual double getDiffusionRate(long index);
    double getMobilityConstant();     // this returns mobility * valence
    virtual double getConvectionVelocity_X(long index) {return convectionVelocity.x;}
    virtual double getConvectionVelocity_Y(long index) {return convectionVelocity.y;}
    virtual double getConvectionVelocity_Z(long index) {return convectionVelocity.z;}
    
protected:
    VolumeVarContext(Feature *feature, string& speciesName);

    double         *diffusionRate;
    double         *mobility;
    DoubleVector3   convectionVelocity;
};

#endif
