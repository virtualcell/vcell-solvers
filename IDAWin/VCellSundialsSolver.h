#ifndef VCELLSUNDIALSSOLVER_H
#define VCELLSUNDIALSSOLVER_H

#include <iostream>
#include <string>
#include <vector>
#include <list>
using std::vector;
using std::list;
using std::stringstream;

#include <Expression.h>
using VCell::Expression;

#include <nvector/nvector_serial.h>
#include <sundials/sundials_types.h>


class SymbolTable;
class OdeResultSet;

#define bytesPerSample 25
#define MaxFileSizeBytes 1000000000 /* 1 gigabyte */	
#define BAD_EXPRESSION_MSG " is not terminated by ';'"
#define MAX_NUM_EVENTS_DISCONTINUITIES_EVAL 50

struct EventAssignment {
	int varIndex;
	Expression* assignmentExpression;

	~EventAssignment() {
		delete assignmentExpression;
	}
	void bind(SymbolTable* symbolTable) {
		assignmentExpression->bindExpression(symbolTable);
	}
};

struct Event {
	string name;
	Expression* triggerExpression;
	bool bUseValuesAtTriggerTime;
	Expression* delayDurationExpression;
	int numEventAssignments;
	EventAssignment** eventAssignments;
	bool triggerValue;	

	Event() {
		bUseValuesAtTriggerTime = false;
		triggerExpression = 0;
		triggerValue = false;
		delayDurationExpression = 0;
		eventAssignments = 0;		
	}
	~Event() {
		delete triggerExpression;
		delete delayDurationExpression;
		for (int i = 0; i < numEventAssignments; i ++) {
			delete eventAssignments[i];
		}
		delete[] eventAssignments;
	}

	bool hasDelay() {
		return delayDurationExpression != 0;
	}

	void bind(SymbolTable* symbolTable) {
		triggerExpression->bindExpression(symbolTable);
		if (delayDurationExpression != 0) {
			delayDurationExpression->bindExpression(symbolTable);
		}
		for (int i = 0; i < numEventAssignments; i ++) {
			eventAssignments[i]->bind(symbolTable);
		}
	}
};

struct EventExecution {
	realtype exeTime;
	Event* event0;
	double* targetValues;

	EventExecution(Event* e) {
		event0 = e;
		targetValues = 0;
	}
	~EventExecution() {
		delete targetValues;
	}
};

struct OdeDiscontinuity {
	string discontinuitySymbol;
	Expression* discontinuityExpression;
	Expression* rootFindingExpression;	

	~OdeDiscontinuity() {
		delete discontinuityExpression;
		delete rootFindingExpression;
	}
};

class VCellSundialsSolver {
public:
	VCellSundialsSolver();	
	virtual ~VCellSundialsSolver();

	void readInput(istream& inputstream);
	virtual void solve(double* paramValues=0, bool bPrintProgress=false, FILE* outputFile=0, void (*checkStopRequested)(double, long)=0) = 0;
	OdeResultSet* getResultSet() { return odeResultSet; }
	int getNumEquations() { return NEQ; }
	Expression** getInitialConditionExpressions() { return initialConditionExpressions; }
	void setStartingTime(realtype newStartingTime) { STARTING_TIME = newStartingTime; }
	void setEndingTime(realtype newEndingTime) { ENDING_TIME = newEndingTime; }
	void setOutputTimes(int count, double* newOutputTimes);
	SymbolTable* getSymbolTable() { return defaultSymbolTable;}	

	static void checkStopRequested(double, long);

protected:	
	// 0 : t
	// 1 ~ N : variable values
	// N+1 ~ N+NPARAM : parameter values 
	// N+NPARAM+1 ~ N+NPARAM+numDiscontinuites : discontinuity values
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

	int numDiscontinuities;
	OdeDiscontinuity** odeDiscontinuities;
	SymbolTable* discontinuitySymbolTable;
	double* discontinuityValues;
	int* rootsFound;
	
	string* paramNames;
	string* variableNames; // variables
	string* allSymbols;
	int numAllSymbols;
	SymbolTable* defaultSymbolTable;

	N_Vector y;	
	void writeData(double currTime, FILE* outputFile);
	virtual void updateTempRowData(double currTime);
	void writeFileData(FILE* outputFile);
	void writeFileHeader(FILE* outputFile);
	void printProgress(double currTime, double& percentile, double increment, FILE* outputFile);

	void readDiscontinuities(istream& inputstream);
	virtual void readEquations(istream& inputstream) = 0;
	virtual void initialize();

	void initDiscontinuities();
	bool updateDiscontinuities(realtype t, bool bOnRootReturn);
	void checkDiscontinuityConsistency();

	void solveInitialDiscontinuities(double t);
	virtual bool fixInitialDiscontinuities(double t)=0;

	void printVariableValues(realtype t);
	void printDiscontinuityValues();
	virtual void updateTandVariableValues(realtype t, N_Vector y)=0;

	int RootFn(realtype t, N_Vector y, realtype *gout);
	virtual string getSolverName()=0;

	Expression* readExpression(istream& inputstream);
	bool executeEvents(realtype Time);
	double getNextEventTime();

private:
	Event** events;
	int numEvents;

	void readEvents(istream& inputstream);
	void testEventTriggers(realtype Time);
	list<EventExecution*> eventExeList;
};

#endif
