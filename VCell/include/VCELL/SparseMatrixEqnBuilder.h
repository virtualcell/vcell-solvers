/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef SPARSEMATRIXEQNBUILDER_H
#define SPARSEMATRIXEQNBUILDER_H

#include <VCELL/EqnBuilder.h>

class Mesh;
class Variable;
class SparseMatrixPCG;
class EqnBuilder;

class SparseMatrixEqnBuilder : public EqnBuilder
{
public:
	SparseMatrixEqnBuilder(Variable *var, Mesh *mesh);

	virtual boolean initEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, 
				int membraneIndexStart, int membraneIndexSize)=0;
	virtual boolean buildEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, 
				int membraneIndexStart, int membraneIndexSize)=0;

	SparseMatrixPCG* getA();
	double* getB();
	long getSize();
	int getSymmetricFlag();

	virtual double* getX(); // X is both initial guess and final solution to the linear system
	virtual void postProcess() {}

protected:
	SparseMatrixPCG* A;
	double* B;
	double* X;
};    

#endif
