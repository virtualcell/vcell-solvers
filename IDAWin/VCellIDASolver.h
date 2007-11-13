#ifndef VCELLIDASOLVER_H
#define VCELLIDASOLVER_H

#include <VCellSundialsSolver.h>

class VCellIDASolver : public VCellSundialsSolver {
public:
	VCellIDASolver();	
	~VCellIDASolver();

	void readInput(istream& inputstream);
	void solve(double* paramValues=0, bool bPrintProgress=false, FILE* outputFile=0, void (*checkStopRequested)(double, long)=0);	
	SymbolTable* getSymbolTable() { return rhsSymbolTable;}	

protected:
	void writeData(double currTime, N_Vector y, FILE* outputFile);

private:
	Expression** rhsExpressions;  // can be rate expression in ODE case or RHS expression in DAE case
	SymbolTable* rhsSymbolTable;
	double **transformMatrix;
	double **inverseTransformMatrix;
	int numDifferential;
	int numAlgebraic;

	N_Vector yp;
	N_Vector id;  // 1 for differential variable, 0 for algebraic variable (used in IDACalcIC()).

	int Residual(realtype t, N_Vector y, N_Vector yp, N_Vector residual);	
	static int Residual_callback(realtype t, N_Vector y, N_Vector yp, N_Vector residual, void *rdata);
	void* initIDA(double* paramValues);
	void idaSolve(void* ida_mem, bool bPrintProgress, FILE* outputFile, void (*checkStopRequested)(double, long));
};

#endif
