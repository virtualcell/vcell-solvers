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
	for (int i = 0; i < (int)bestRunResultSet.paramValues.size(); i ++){
		cout << paramNames[i] << " = " << bestRunResultSet.paramValues[i] << endl;
	}
	cout << "objective function value = " << bestRunResultSet.objectiveFunctionValue << endl;
	cout << "num of objective function evaluations = " << bestRunResultSet.numObjFuncEvals << endl;	
	cout << "status message: " << bestRunResultSet.statusMessage << endl;	

	for (int pd = 0; pd < (int)vectProfileDistribution.size(); pd ++){
		cout << "-----------------------------------------" << endl;
		ProfileDistribution& profileDistribution = vectProfileDistribution[pd];
		cout << "fixedIndex = " << profileDistribution.fixedIndex << endl;
		cout << "fixedParameterName = " << profileDistribution.fixedParamName << endl;

		for (int r = 0; r < (int)profileDistribution.optRunResultSets.size(); r ++){
			OptRunResultSet& optRunResultSet = profileDistribution.optRunResultSets[r];
			for (int i = 0; i < (int)optRunResultSet.paramValues.size(); i ++){
				cout << paramNames[i] << " = " << optRunResultSet.paramValues[i] << endl;
			}
			cout << "objective function value = " << optRunResultSet.objectiveFunctionValue << endl;
			cout << "num of objective function evaluations = " << optRunResultSet.numObjFuncEvals << endl;	
			cout << "status message: " << optRunResultSet.statusMessage << endl;	
		}
	}

	cout.flush();
}
