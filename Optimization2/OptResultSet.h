#ifndef OPTRESULTSET_H
#define OPTRESULTSET_H

#include <vector>
#include <string>
#include "OptSolver2.h"
using namespace std;


class OptResultSet {
public:
	OptResultSet();
	~OptResultSet();

	void show();

	OptSolverStatus status;
	vector<string> paramNames;
	vector<double> paramValues;
	double objectiveFunctionValue;
	int numObjFuncEvals;
	string statusMessage;
};

#endif
