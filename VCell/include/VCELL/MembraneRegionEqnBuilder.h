/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef MEMBRANEREGIONEQNBUILDER_H
#define MEMBRANEREGIONEQNBUILDER_H

#include <VCELL/EqnBuilder.h>

class Mesh;
class ODESolver;
class VolumeRegionVariable;
class MembraneRegionVariable;

class MembraneRegionEqnBuilder : public EqnBuilder
{
public:
    MembraneRegionEqnBuilder(MembraneRegionVariable *var, CartesianMesh *mesh, ODESolver *solver);
    virtual bool initEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize) 
		{ return true; }
    virtual bool buildEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize);          

private:
	ODESolver* odeSolver;
};

#endif
