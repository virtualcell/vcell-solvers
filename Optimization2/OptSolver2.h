#ifndef OPTSOLVER2_H
#define OPTSOLVER2_H

#include <vector>
using namespace std;

class OptProblemDescription;
class OptSolver;
class OptSolver2Listener;
class OptSolverEvent;
class OptResultSet;

struct FixParameterSpec {
	int paramIndex;
	double paramValue;
	bool bFixed;

	FixParameterSpec() {
		bFixed = false;
	}
};

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
	void fixParameter(int paramIndex, double paramValue) {
		fixedParameterSpec.paramIndex = paramIndex;
		fixedParameterSpec.paramValue = paramValue;
		fixedParameterSpec.bFixed = true;
	}
	void clearFixedParameter() {
		fixedParameterSpec.bFixed = false;
	}
	bool isFixedParameter() {
		return fixedParameterSpec.bFixed;
	}
	int getFixedParamterIndex() {
		return fixedParameterSpec.paramIndex;
	}
	double getFixedParamterValue() {
		return fixedParameterSpec.paramValue;
	}
	virtual void computeProfileDistributions(OptResultSet* optResultSet)=0;

private:
	vector<OptSolver2Listener*> optSolverListeners;
	OptProblemDescription *optProblemDescription;
	FixParameterSpec fixedParameterSpec;
};

#endif
