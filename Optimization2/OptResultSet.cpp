// OptSolverLibrary.cpp : Defines the entry point for the application.
//
#include <iostream>
using namespace std;


#include "OptResultSet.h"

OptResultSet::OptResultSet() {
}

OptResultSet::~OptResultSet() {
}

void OptResultSet::show(){
	for (int i = 0; i < (int)paramValues.size(); i ++){
		cout << "p[" << i << "] = " << paramValues[i] << endl;
	}
	cout << "objective function value = " << objectiveFunctionValue << endl;
	cout << "num of objective function evaluations = " << numObjFuncEvals << endl;	
	cout << "status message: " << statusMessage << endl;	
	cout.flush();
}
