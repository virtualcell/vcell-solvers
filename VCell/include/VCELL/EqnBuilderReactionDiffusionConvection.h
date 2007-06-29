/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef EQNBUILDERREACTIONDIFFUSIONCONVECTION_H
#define EQNBUILDERREACTIONDIFFUSIONCONVECTION_H

#include <VCELL/EqnBuilder.h>

class CartesianMesh;
class PdeSolverDiana;
class VolumeVariable;
class ExactVolumeVarContextRemainder;

class EqnBuilderReactionDiffusionConvection : public EqnBuilder
{
public:
	EqnBuilderReactionDiffusionConvection(VolumeVariable *species, 
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
