/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef MEMBRANEVARCONTEXTEXPRESSION_H
#define MEMBRANEVARCONTEXTEXPRESSION_H

#include <VCELL/MembraneVarContext.h>
class Feature;

class MembraneVarContextExpression : public MembraneVarContext
{
public:
	MembraneVarContextExpression(Feature *feature, string& varName);

	bool resolveReferences(Simulation *sim);

    double  getInitialValue(MembraneElement *element);
    double  getMembraneReactionRate(MembraneElement *element);
    double  getMembraneDiffusionRate(MembraneElement *element);

    double getXmBoundaryValue(MembraneElement *element);
    double getXpBoundaryValue(MembraneElement *element);
    double getYmBoundaryValue(MembraneElement *element);
    double getYpBoundaryValue(MembraneElement *element);
    double getZmBoundaryValue(MembraneElement *element);
    double getZpBoundaryValue(MembraneElement *element);

    double getXmBoundaryFlux(MembraneElement *element);
    double getXpBoundaryFlux(MembraneElement *element);
    double getYmBoundaryFlux(MembraneElement *element);
    double getYpBoundaryFlux(MembraneElement *element);
    double getZmBoundaryFlux(MembraneElement *element);
    double getZpBoundaryFlux(MembraneElement *element);

protected:
	bool isNullExpressionOK(int expIndex);
};

#endif
