#ifndef OPTSOLVER_H
#define OPTSOLVER_H

#include <vector>
#include <string>
using namespace std;

class OptJob;
class OptSolver;
class OptSolverListener;
class OptSolverEvent;
class OptSolverResultSet;

class OptJob {
};

class OptSolver {
public:
	OptSolver(OptJob *optJob);
	~OptSolver();

	virtual OptSolverResultSet* solve()=0;  // pure virtual 
	virtual void stop()=0;
	void addListener(OptSolverListener *listener);
	void removeListener(OptSolverListener *listener);
	OptJob* getOptJob();

protected:
	void fireObjectiveFunctionEvalEvent(int numParameters, double* paramValues, double objValue);
private:
	vector<OptSolverListener*> optSolverListeners;
	OptJob *optJob;
};


class OptSolverListener {
public:
	virtual void handleObjectiveFunctionEvalEvent(int numParameters, double* paramValues, double objValue)=0;
};

class OptSolverResultSet {
public:
	OptSolverResultSet();
	~OptSolverResultSet();

	void show();

	int status;
	double* params;
	int nParams;
	double objectiveFunctionValue;
	int numObjFuncEvals;
	string statusMessage;
};

#endif
