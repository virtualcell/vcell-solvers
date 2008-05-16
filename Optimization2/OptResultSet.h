#ifndef OPTRESULTSET_H
#define OPTRESULTSET_H

#include <vector>
#include <string>
using namespace std;

typedef enum {
	unknown = -1,
	normalTermination = 0,
	nonfeasibleLinear = 1,
	nonfeasibleNonlinear = 2,
	noSolutionIterations = 3,
	noSolutionMachinePrecision = 4,
	failedConstructingD0 = 5,
	failedConstructingD1 = 6,
	failedInconsistentInput = 7,
	failedIteratesStalled = 8,
	failedPenaltyTooLarge = 9,
	failed = 10,
	stoppedByUser = 11
} OptSolverStatus;

class OptResultSet {
public:
	OptResultSet();
	~OptResultSet();

	void show();

	OptSolverStatus status;
	vector<string> paramNames;
	vector<double> paramValues;
	double objectiveFunctionValue;
	int numObjFuncEvals;
	string statusMessage;
};

#endif
