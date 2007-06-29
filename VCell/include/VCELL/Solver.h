/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef SOLVER_H
#define SOLVER_H

class Variable;
class VolumeVariable;
class MembraneVariable;
class EqnBuilder;
class Mesh;
class CartesianMesh;

class Solver
{ 
public:
	Solver(Variable *variable);

	virtual bool initEqn(double deltaTime, 
							int volumeIndexStart, int volumeIndexSize, 
							int membraneIndexStart, int membraneIndexSize, bool bFirstTime);
	virtual bool buildEqn(double deltaTime, 
							int volumeIndexStart, int volumeIndexSize, 
							int membraneIndexStart, int membraneIndexSize, bool bFirstTime);
	virtual bool solveEqn(double deltaTime, 
							int volumeIndexStart, int volumeIndexSize, 
							int membraneIndexStart, int membraneIndexSize, bool bFirstTime)=0;

	Variable *getVar() { return var; }
	void setEqnBuilder(EqnBuilder *builder) { eqnBuilder = builder; }
	EqnBuilder *getEqnBuilder() { return eqnBuilder; }
	virtual bool isPDESolver() { return false; }

	Solver       *next;

protected:
	Variable     *var;
	EqnBuilder   *eqnBuilder;

};

#endif
