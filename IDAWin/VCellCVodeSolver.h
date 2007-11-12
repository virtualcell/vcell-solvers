#ifndef VCELLCVODESOLVER_H
#define VCELLCVODESOLVER_H

#include <VCellSundialsSolver.h>


class VCellCVodeSolver : public VCellSundialsSolver {
public:
	VCellCVodeSolver(istream& inputstream, bool bPrintProgress=false);	
	~VCellCVodeSolver();

	void solve(double* paramValues=0, FILE* outputFile=0, void (*checkStopRequested)(double, long)=0);
	SymbolTable* getSymbolTable() { return rateSymbolTable;}	
	double RHS(double* allValues, int equationIndex);	

protected:	
	virtual void readInput(istream& inputstream);

private:
	Expression** rateExpressions; 
	SymbolTable* rateSymbolTable;
	
	int RHS(realtype t, N_Vector y, N_Vector yp);
	static int RHS_callback(realtype t, N_Vector y, N_Vector r, void *fdata);	
};

#endif
