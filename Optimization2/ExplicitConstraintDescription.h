#ifndef EXPLICITCONSTRAINTDESCRIPTION_H
#define EXPLICITCONSTRAINTDESCRIPTION_H

#include <string>
#include <vector>
using namespace std;

#include "ConstraintDescription.h"

class SymbolTable;
class Constraint;

class ExplicitConstraintDescription : public ConstraintDescription {
public:
	ExplicitConstraintDescription(vector<Constraint*> constraints, SymbolTable* symbolTable);
	~ExplicitConstraintDescription();

	virtual int getNumNonlinearInequality();
	virtual int getNumLinearInequality();
	virtual int getNumNonlinearEquality();
	virtual int getNumLinearEquality();
	
	virtual void constraints(int nparams, int j, double* x, double* gj);
	
	virtual int getNumConstraintEvals() { return numConstraintEvals; }
	
private:
	int numNonLinearInequality, numLinearInequality;
    int numNonLinearEquality, numLinearEquality;

	int numConstraintEvals;

	vector<Constraint*> constraintList;
};

#endif
