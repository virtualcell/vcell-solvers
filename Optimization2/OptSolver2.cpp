// OptSolver2Library.cpp : Defines the entry point for the application.
//
#include <iostream>
#include <stdio.h>
using namespace std;
#include "OptSolver2.h"

//================================================
//
// class OptSolver2
//
//================================================
OptSolver2::OptSolver2(OptProblemDescription *arg_optProblemDescription){
	optProblemDescription = arg_optProblemDescription;
}

OptSolver2::~OptSolver2(){
	optSolverListeners.clear();
}

void OptSolver2::addListener(OptSolver2Listener *listener){
	optSolverListeners.push_back(listener);
}

void OptSolver2::removeListener(OptSolver2Listener *listener){
	for (vector<OptSolver2Listener*>::iterator iter = optSolverListeners.begin(); iter < optSolverListeners.end(); iter ++) {
		if (*iter == listener) {
			optSolverListeners.erase(iter);
			break;
		}
	}
}

void OptSolver2::fireObjectiveFunctionEvalEvent(int numParameters, double* paramValues, double objValue){
	for (vector<OptSolver2Listener*>::iterator iter = optSolverListeners.begin(); iter < optSolverListeners.end(); iter ++) {
		(*iter)->handleObjectiveFunctionEvalEvent(numParameters, paramValues, objValue);
	}
}

OptProblemDescription* OptSolver2::getOptProblemDescription(){
	return optProblemDescription;
}

