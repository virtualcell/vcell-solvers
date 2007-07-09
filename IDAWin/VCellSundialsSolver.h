#ifndef VCELLSUNDIALSSOLVER_H
#define VCELLSUNDIALSSOLVER_H

#include <nvector/nvector_serial.h>
#include <sundials/sundials_types.h>

#include <iostream>
#include <string>
#include <vector>
using namespace std;

class Expression;
class SymbolTable;
class OdeResultSet;

#define MAX_EXPRESSION_LENGTH 40000
#define bytesPerSample 25
#define MaxFileSizeBytes 1000000000 /* 1 gigabyte */	

class VCellSundialsSolver {
public:
	VCellSundialsSolver(istream& inputstream, bool arg_bPrintProgress);	
	~VCellSundialsSolver();

	virtual void readInput(istream& inputstream) = 0;
	virtual void solve(double* paramValues=0, FILE* outputFile=0, void (*checkStopRequested)(double, long)=0) = 0;
	OdeResultSet* getResultSet() { return odeResultSet; }
	int getNumEquations() { return NEQ; }
	Expression** getInitialConditionExpressions() { return initialConditionExpressions; }
	void setStartingTime(realtype newStartingTime) { STARTING_TIME = newStartingTime; }
	void setEndingTime(realtype newEndingTime) { ENDING_TIME = newEndingTime; }
	void setOutputTimes(int count, double* newOutputTimes);

protected:
	bool bPrintProgress;
	
	// 0 : t
	// 1 ~ N : variable values
	// N+1 ~ N+NPARAM : parameter values 
	realtype* values; 
	// 0 ~ N-1 : equations
	Expression** initialConditionExpressions; 
	SymbolTable* initialConditionSymbolTable;
	OdeResultSet* odeResultSet;

	int NEQ;
	int NPARAM;
	realtype STARTING_TIME;
	realtype ENDING_TIME;
	realtype RelativeTolerance;
	realtype AbsoluteTolerance;
	long keepEvery;
	double maxTimeStep;		
	vector<double> outputTimes;
	double* tempRowData;
	
	// 0 ~ NAPRAM-1 : parameter names;
	vector<string> paramNames;

	N_Vector y;	
	void writeData(double currTime, N_Vector y, FILE* outputFile);
	void printProgress(double currTime, double& percentile, double increment);
};

char* trim(char* str);
int check_flag(void *flagvalue, char *funcname, int opt);

#endif
