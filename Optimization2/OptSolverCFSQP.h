#ifndef OPTSOLVERCFSQP_H
#define OPTSOLVERCFSQP_H

#include "OptSolver2.h"

class OptSolverCFSQP;
class OptResultSet;
class OptProblemDescription;

class OptSolverCFSQP : public OptSolver2 {
public:
	OptSolverCFSQP(OptProblemDescription *optProblemDescription);
	~OptSolverCFSQP();

	OptResultSet* solve();
	void stop();
	void setPrintMode(int newIprint) { iprint = newIprint; }


protected:
	void obj(int nparam, double *x, double *pobj);
	void constr(int nparam, int j, double *x, double *pconstr);
	void gradob(int nparam, int j, double *x, double *gradfj, void (* dummy)(int,int,double*,double*,void*), void *cd);
	void gradcn(int nparam, int j, double *x, double *gradgj, void (* dummy)(int,int,double*,double*,void*), void *cd);

private:
	int iprint;

	static void obj_callback(int nparam, int j, double *x, double *fj, void *solverPointer);
	static void constr_callback(int nparam, int j, double *x, double *gj, void *solverPointer);
	static void gradob_callback(int nparam, int j, double *x, double *gradfj, void (* dummy)(int,int,double*,double*,void*), void *solverPointer);
	static void gradcn_callback(int nparam, int j, double *x, double *gradgj, void (* dummy)(int,int,double*,double*,void*), void *solverPointer);
};

#endif
