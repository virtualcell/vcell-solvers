// OptSolverLibrary.cpp : Defines the entry point for the application.
//
#include <iostream>
#include <stdio.h>
using namespace std;
#include "OptSolver.h"
#include "OptResultSet.h"

//================================================
//
// class OptResultSet
//
//================================================
OptResultSet::OptResultSet() {
}

OptResultSet::~OptResultSet() {
}

void OptResultSet::show(){
	for (int i=0;i<paramValues.size();i++){
		cout << "p[" << i << "] = " << paramValues[i] << endl;
	}
	cout << "objective function value = " << objectiveFunctionValue << endl;
	cout << "num of objective function evaluations = " << numObjFuncEvals << endl;	
	cout << "status message: " << statusMessage << endl;	
	cout.flush();
}
