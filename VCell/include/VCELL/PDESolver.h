/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef PDESOLVER_H
#define PDESOLVER_H

#include <VCELL/Solver.h>

class Variable;
class VolumeVariable;
class MembraneVariable;
class EqnBuilder;
class Mesh;
class CartesianMesh;

class PDESolver : public Solver
{
public:
	PDESolver(Variable *var, bool bTimeDependent);
	virtual ~PDESolver();

	virtual bool initEqn(double deltaTime, 
							int volumeIndexStart, int volumeIndexSize, 
							int membraneIndexStart, int membraneIndexSize, bool bFirstTime);
	virtual bool isPDESolver() { return true; }
	bool isTimeDependent() { return bTimeDependent; }
private:
	bool  bTimeDependent;
};

#endif
