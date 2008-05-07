#ifndef SPARSELINEARSOLVER_H
#define SPARSELINEARSOLVER_H

#include <VCELL/PDESolver.h>

class SparseMatrixEqnBuilder;
class Variable;

class SparseLinearSolver : public PDESolver
{
public:
    SparseLinearSolver(Variable *Var,  SparseMatrixEqnBuilder* eqnbuilder,  bool AbTimeDependent);
    ~SparseLinearSolver(); 

	virtual void solveEqn(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize, bool bFirstTime);	

private:
	bool enableRetry;
	void initPCGWorkspace(long additional=0);

protected:
	int* PCGSolve(bool bRecomputeIncompleteFactorization);
	SparseMatrixEqnBuilder* smEqnBuilder;    
	double* pcg_workspace;	
	long nWork;
	
};
#endif
