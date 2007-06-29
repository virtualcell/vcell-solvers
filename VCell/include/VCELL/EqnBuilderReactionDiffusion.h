/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef EQNBUILDERREACTIONDIFFUSION_H
#define EQNBUILDERREACTIONDIFFUSION_H

#include <VCELL/EqnBuilder.h>

class CartesianMesh;
class PdeSolverDiana;
class VolumeVariable;
class ExactVolumeVarContextRemainder;

class EqnBuilderReactionDiffusion : public EqnBuilder
{
public:
	EqnBuilderReactionDiffusion(VolumeVariable *species, 
							CartesianMesh *mesh,  
							PdeSolverDiana *solver);

	virtual bool initEquation(double deltaTime, 
					int volumeIndexStart, int volumeIndexSize, 
					int membraneIndexStart, int membraneIndexSize);
	virtual bool buildEquation(double deltaTime, 
					int volumeIndexStart, int volumeIndexSize, 
					int membraneIndexStart, int membraneIndexSize);

private:
	PdeSolverDiana* pdeSolverDiana;

};    

#endif
