/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef MEMBRANEEQNBUILDERFORWARD_H
#define MEMBRANEEQNBUILDERFORWARD_H

#include <VCELL/EqnBuilder.h>

class Mesh;
class ODESolver;
class MembraneVariable;
class ExactVolumeVarContextRemainder;

class MembraneEqnBuilderForward : public EqnBuilder
{
public:
	MembraneEqnBuilderForward(MembraneVariable *species, 
							Mesh *mesh,  
							ODESolver *solver);

	void initEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize) 
	{}
	void buildEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize);

private:
	ODESolver* odeSolver;
};    

#endif
