#ifndef OPTSOLVER2LISTENER_H
#define OPTSOLVER2LISTENER_H

class OptSolver2Listener {
public:
	virtual void handleObjectiveFunctionEvalEvent(int numParameters, double* paramValues, double objValue)=0;
};

#endif
