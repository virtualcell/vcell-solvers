#ifndef OPTRESULTSET_H
#define OPTRESULTSET_H

#include <vector>
#include <string>
using std::vector;
using std::string;

enum OptSolverStatus {
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
};

struct OptRunResultSet {
	OptSolverStatus status;
	vector<double> paramValues;
	double objectiveFunctionValue;
	int numObjFuncEvals;
	string statusMessage;
};

struct ProfileDistribution {
	int fixedIndex;
	string fixedParamName;
	vector<OptRunResultSet> optRunResultSets;
};

class OptResultSet {
public:
	OptResultSet();
	~OptResultSet();

	vector<string> paramNames;
	void show();
	OptRunResultSet bestRunResultSet;
	vector<ProfileDistribution> vectProfileDistribution;
};

#endif
