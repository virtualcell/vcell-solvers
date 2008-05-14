#ifndef OPTSOLVER2_H
#define OPTSOLVER2_H

#include <vector>
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

class OptProblemDescription;
class OptSolver;
class OptSolver2Listener;
class OptSolverEvent;
class OptResultSet;

class OptSolver2 {
public:
	OptSolver2(OptProblemDescription *optProblemDescription);
	~OptSolver2();

	virtual OptResultSet* solve()=0;  // pure virtual 
	virtual void stop()=0;
	void addListener(OptSolver2Listener *listener);
	void removeListener(OptSolver2Listener *listener);
	OptProblemDescription* getOptProblemDescription();

protected:
	void fireObjectiveFunctionEvalEvent(int numParameters, double* paramValues, double objValue);
private:
	vector<OptSolver2Listener*> optSolverListeners;
	OptProblemDescription *optProblemDescription;
};

#endif
