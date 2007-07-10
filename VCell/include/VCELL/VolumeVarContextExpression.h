/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef VOLUMEVARCONTEXTEXPRESSION_H
#define VOLUMEVARCONTEXTEXPRESSION_H

#include <VCELL/VolumeVarContext.h>

class VolumeVarContextExpression : public VolumeVarContext
{
public:	
	VolumeVarContextExpression(Feature *feature, string& varName);
	
	bool resolveReferences(Simulation *sim);

	double getInitialValue(long index);
	double getDiffusionRate(long index);
	double  getReactionRate(long volumeIndex);
    void    getFlux(MembraneElement *element, double *inFlux, double *outFlux);

    double getXmBoundaryValue(long index);
    double getXpBoundaryValue(long index);
    double getYmBoundaryValue(long index);
    double getYpBoundaryValue(long index);
    double getZmBoundaryValue(long index);
    double getZpBoundaryValue(long index);

    double getXmBoundaryFlux(long index);
    double getXpBoundaryFlux(long index);
    double getYmBoundaryFlux(long index);
    double getYpBoundaryFlux(long index);
    double getZmBoundaryFlux(long index);
    double getZpBoundaryFlux(long index);    
    
	double getXBoundaryPeriodicConstant();
    double getYBoundaryPeriodicConstant();
    double getZBoundaryPeriodicConstant();

    double getConvectionVelocity_X(long index);
    double getConvectionVelocity_Y(long index);
    double getConvectionVelocity_Z(long index);

protected:
	bool isNullExpressionOK(int expIndex);
};

#endif
