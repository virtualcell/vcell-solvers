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
#define BAD_EXPRESSION_MSG " is not terminated by ';', it is either an invalid expression or longer than MAX_EXPRESSION_LENGTH (40000)"

class VCellSundialsSolver {
public:
	VCellSundialsSolver();	
	~VCellSundialsSolver();

	virtual void readInput(istream& inputstream) = 0;
	virtual void solve(double* paramValues=0, bool bPrintProgress=false, FILE* outputFile=0, void (*checkStopRequested)(double, long)=0) = 0;
	OdeResultSet* getResultSet() { return odeResultSet; }
	int getNumEquations() { return NEQ; }
	Expression** getInitialConditionExpressions() { return initialConditionExpressions; }
	void setStartingTime(realtype newStartingTime) { STARTING_TIME = newStartingTime; }
	void setEndingTime(realtype newEndingTime) { ENDING_TIME = newEndingTime; }
	void setOutputTimes(int count, double* newOutputTimes);

protected:	
	// 0 : t
	// 1 ~ N : variable values
	// N+1 ~ N+NPARAM : parameter values 
	realtype* values; 
	// 0 ~ N-1 : equations
	Expression** initialConditionExpressions; 
	SymbolTable* initialConditionSymbolTable;
	OdeResultSet* odeResultSet;		// mainly for parameter optimization use but it also stores column names

	void* solver;	// the memory for solver
	string recoverableErrMsg;

	int NEQ;
	int NPARAM;
	realtype STARTING_TIME;
	realtype ENDING_TIME;
	realtype RelativeTolerance;
	realtype AbsoluteTolerance;
	long keepEvery;
	double maxTimeStep;		
	vector<double> outputTimes;
	double* tempRowData; // data for current time to be written to output file and to be added to odeResultSet
	
	// 0 ~ NAPRAM-1 : parameter names;
	vector<string> paramNames;

	N_Vector y;	
	void writeData(double currTime, FILE* outputFile);
	virtual void updateTempRowData(double currTime);
	void writeFileData(FILE* outputFile);
	void writeFileHeader(FILE* outputFile);
	void printProgress(double currTime, double& percentile, double increment);
};

char* trim(char* str);

#endif
