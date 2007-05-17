/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifdef WIN32 
#include <Windows.h>
#else
#include <UnixDefs.h>
#endif

#include <VCELL/SparseMatrixEqnBuilder.h>
#include <VCELL/SparseMatrixPCG.h>
#include <VCELL/Variable.h>

SparseMatrixEqnBuilder::SparseMatrixEqnBuilder(Variable *var, Mesh *mesh) : EqnBuilder(var, mesh)
{
	A = 0;
	B = 0;
	X = var->getCurr();
}

long SparseMatrixEqnBuilder::getSize() {
	assert(A);
	return A->getN();
}

double* SparseMatrixEqnBuilder::getX() {
	return X;
}

SparseMatrixPCG* SparseMatrixEqnBuilder::getA() {
	return A;
}

double* SparseMatrixEqnBuilder::getB(){
	return B;
}

int SparseMatrixEqnBuilder::getSymmetricFlag() {
	assert(A);
	return A->getSymmetricFlag();
}
