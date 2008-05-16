#ifndef OPTSOLVER2_H
#define OPTSOLVER2_H

#include <vector>
using namespace std;

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
