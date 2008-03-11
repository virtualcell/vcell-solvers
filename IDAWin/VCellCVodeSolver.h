#ifndef VCELLCVODESOLVER_H
#define VCELLCVODESOLVER_H

#include <VCellSundialsSolver.h>


class VCellCVodeSolver : public VCellSundialsSolver {
public:
	VCellCVodeSolver();	
	~VCellCVodeSolver();

	void solve(double* paramValues=0, bool bPrintProgress=false, FILE* outputFile=0, void (*checkStopRequested)(double, long)=0);
	SymbolTable* getSymbolTable() { return rateSymbolTable;}	
	double RHS(double* allValues, int equationIndex);	
	void readInput(istream& inputstream);

private:
	Expression** rateExpressions; 
	SymbolTable* rateSymbolTable;
	
	int RHS(realtype t, N_Vector y, N_Vector yp);
	static int RHS_callback(realtype t, N_Vector y, N_Vector r, void *fdata);	
	char* getCVodeErrorMessage(int returnCode);
	void checkCVodeFlag(int flag);
};

#endif
