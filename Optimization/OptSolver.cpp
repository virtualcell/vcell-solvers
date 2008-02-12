// OptSolverLibrary.cpp : Defines the entry point for the application.
//
#include <iostream>
#include <stdio.h>
using namespace std;
#include "OptSolver.h"

//================================================
//
// class OptSolver
//
//================================================
OptSolver::OptSolver(OptJob *argOptJob){
	optJob = argOptJob;
}

OptSolver::~OptSolver(){
	optSolverListeners.clear();
}

void OptSolver::addListener(OptSolverListener *listener){
	optSolverListeners.push_back(listener);
}

void OptSolver::removeListener(OptSolverListener *listener){
	for (vector<OptSolverListener*>::iterator iter = optSolverListeners.begin(); iter < optSolverListeners.end(); iter ++) {
		if (*iter == listener) {
			optSolverListeners.erase(iter);
			break;
		}
	}
}

void OptSolver::fireObjectiveFunctionEvalEvent(int numParameters, double* paramValues, double objValue){
	for (vector<OptSolverListener*>::iterator iter = optSolverListeners.begin(); iter < optSolverListeners.end(); iter ++) {
		(*iter)->handleObjectiveFunctionEvalEvent(numParameters, paramValues, objValue);
	}
}

OptJob* OptSolver::getOptJob(){
	return optJob;
}


//================================================
//
// class OptSolverResultSet
//
//================================================
OptSolverResultSet::OptSolverResultSet() {
	params = 0;
}

OptSolverResultSet::~OptSolverResultSet() {
	delete[] params;
}

void OptSolverResultSet::show(){
	for (int i=0;i<nParams;i++){
		cout << "p[" << i << "] = " << params[i] << endl;
	}
	cout << "objective function value = " << objectiveFunctionValue << endl;
	cout << "num of objective function evaluations = " << numObjFuncEvals << endl;	
	cout << "status message: " << statusMessage << endl;	
	cout.flush();
}
