#ifndef VCELLIDASOLVER_H
#define VCELLIDASOLVER_H

#include <VCellSundialsSolver.h>

class VCellIDASolver : public VCellSundialsSolver {
public:
	VCellIDASolver(istream& inputstream, bool bPrintProgress=false);	
	~VCellIDASolver();

	void readInput(istream& inputstream);
	void solve(double* paramValues=0, FILE* outputFile=0, void (*checkStopRequested)(double, long)=0);
	SymbolTable* getSymbolTable() { return rateSymbolTable;}	

private:
	Expression** rateExpressions; 
	SymbolTable* rateSymbolTable;

	N_Vector yp;
	int Residual(realtype t, N_Vector y, N_Vector yp, N_Vector r);	
	static int Residual_callback(realtype t, N_Vector y, N_Vector yp, N_Vector r, void *rdata);	
};

#endif
