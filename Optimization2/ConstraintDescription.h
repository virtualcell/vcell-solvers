#ifndef ConstraintDescription_H
#define ConstraintDescription_H

#include <string>
#include <vector>
using namespace std;

class SymbolTable;
class Constraint;

class ConstraintDescription {
public:
	virtual int getNumNonlinearInequality() = 0;
	virtual int getNumLinearInequality() = 0;
	virtual int getNumNonlinearEquality() = 0;
	virtual int getNumLinearEquality() = 0;
	
	virtual void constraints(int nparams, int j, double* x, double* gj)=0;

	virtual void gradConstraint(int nparam, int j, double *x, double *gradgj, void (* dummy)(int,int,double*,double*,void*), void *cd){}
	virtual bool hasGradConstraint() { return false; }
	
	virtual int getNumConstraintEvals() = 0;
};

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
