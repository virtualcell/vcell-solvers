#ifndef CONSTRAINTDESCRIPTION_H
#define CONSTRAINTDESCRIPTION_H

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

#endif
