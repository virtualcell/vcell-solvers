#include "CFSQPSolver.h"
#include "OdeOptJob.h"
#include "OdeMultiShootingOptJob.h"
#include "OdeResultSet.h"
#include "math.h"
#include <float.h>

#include <iomanip>
#include <iostream>
using namespace std;

/*
class Sample1 : public CFSQPOptJob {
public:
	Sample1() : CFSQPOptJob(){
	}

	int getNumParameters() { return 3; }
	int getNumNonlinearInequality() { return 1; }
	int getNumLinearInequality() { return 1; }
	int getNumNonlinearEquality() { return 0; }
	int getNumLinearEquality() { return 1; }
	void getLimits(double* bl, double* bu) {
		bl[0] = 0.0; bl[1]=0.0; bl[2]=0.0;
		bu[0] = 1e10; bu[1]=1e10; bu[2]=1e10;
	}
	void getInitialGuess(double* x) {
		x[0]=0.1e0;
		x[1]=0.7e0;
		x[2]=0.2e0;
	}

	void objective(int nparams, double* x, double* f) {
		*f = pow((x[0]+3.e0*x[1]+x[2]),2.e0)+4.e0*pow((x[0]-x[1]),2.e0);
	}
	void constraints(int nparams, int j, double* x, double* gj) {
		switch (j) {
			case 1:
			*gj=pow(x[0],3.e0)-6.e0*x[1]-4.e0*x[2]+3.e0;
			break;
			case 2:
			*gj=1.e0-x[0]-x[1]-x[2];
			break;
		}
		return;
	}
	void gradObjective(int nparam, double* x, double *gradfj){
		double fa=2.e0*(x[0]+3.e0*x[1]+x[2]);
		double fb=8.e0*(x[0]-x[1]);
		gradfj[0]=fa+fb;
		gradfj[1]=fa*3.e0-fb;
		gradfj[2]=fa;
		return;
	}
	void gradConstraint(int nparam,int j,double* x,double* gradgj){
		switch (j) {
			case 1:
				gradgj[0]=3.e0*x[0]*x[0];
				gradgj[1]=-6.e0;
				gradgj[2]=-4.e0;
			break;
			case 2:
				gradgj[0]=gradgj[1]=gradgj[2]=-1.e0;
			break;
		}
		return;
	}   
};


class Sample2 : public CFSQPOptJob {
public:
	Sample2() : CFSQPOptJob(){
	}

	int getNumParameters() { return 2; }
	int getNumNonlinearInequality() { return 0; }
	int getNumLinearInequality() { return 0; }
	int getNumNonlinearEquality() { return 0; }
	int getNumLinearEquality() { return 0; }

	void getLimits(double* bl, double* bu) {
		bl[0] = 0.0;		bl[1]=0.0;
		bu[0] = 1e10;		bu[1]=1e10;
	}
	void getInitialGuess(double* x) {
		x[0]=0.0e0;
		x[1]=0.0e0;
	}

	void objective(int nparams, double* x, double* f) {
		*f = (x[0]-5.0)*(x[0]-5.0) + (x[1]-2.0)*(x[1]-2.0);
	}

	void constraints(int nparams, int j, double* x, double* gj) {
	}

	void gradObjective(int nparam, double* x, double *gradfj){
		gradfj[0]=2*(x[0]-5);
		gradfj[1]=2*(x[1]-2);
		return;
	}
	void gradConstraint(int nparam,int j,double* x,double* gradgj){
	}   
};
*/

#define NUM_PARAMS 4
#define NUM_REF_COLUMNS 4
#define NUM_REF_ROWS 11

int main() {
	/*
	CFSQPOptJob* cfsqpOptJob = new Sample2();
	CFSQPOptSolver* cfsqpOptSolver = new CFSQPOptSolver(cfsqpOptJob);
	OptSolverResultSet* optSolverResultSet = cfsqpOptSolver->solve();
	optSolverResultSet->show();
	optSolverResultSet->show();
	*/

	cout << setprecision(30) << endl;

	char* paramNames[20] = {"Vmax"};
	double LB[NUM_PARAMS] = {20.0};
	double UB[NUM_PARAMS] = {DBL_MAX};
	double initialGuess[NUM_PARAMS] = {20.0};
	double scaleFactors[NUM_PARAMS] = {20.0};
	double weights[NUM_REF_COLUMNS] = {1.0, 5.075729614413505E-4, 0.0031398209428204773, 0.021289489638613643};
	OdeResultSet* refData = new OdeResultSet();
	char* refNames[20] = {"t", "BS_nucleus", "rB_nucleus", "r_nucleus"};
	for (int i = 0; i < NUM_REF_COLUMNS; i ++) {
        refData->addColumn(refNames[i]);
	}
	double refRows[NUM_REF_ROWS][NUM_REF_COLUMNS] = {
							{0.0, 16.18033988749905, 3.819660112500951, 1.180339887499049},
					                 {2.0, 14.233628939985033, 3.700193287600382, 1.2998067123996204},
					                  {4.0, 13.444291438073678, 3.6445670692237253, 1.3554329307762791},
					                  {6.0, 13.098551382928354, 3.618673977212261, 1.381326022787741},
					                  {8.0, 12.941981757625658, 3.606619918706895, 1.3933800812931074},
					                  {10.0, 12.870006870163259, 3.6010078126079903, 1.398992187392012},
					                  {12.0, 12.810093544564559, 6.897400765603068, 2.692174238075611},
					                  {14.0, 12.80890204139876, 6.8972206459011325, 2.6923543577775444},
					                  {16.0, 12.808319999477469, 6.8971326497788965, 2.69244235389978},
					                  {18.0, 12.808035662528477, 6.897089660150106, 2.6924853435285723},
					                  {20.0, 12.807896749788219, 6.8970686570791795, 2.692506346599499}
					};
	for (int i = 0; i < NUM_REF_ROWS; i ++) {
        refData->addRow(refRows[i]);
	}

	string input = string("STARTING_TIME 0.0\n") + 
			"ENDING_TIME 20.0\n" + 
			"RELATIVE_TOLERANCE 1.0E-9\n" + 
			"ABSOLUTE_TOLERANCE 1.0E-9\n" + 
			"MAX_TIME_STEP 1.0\n" + 
			"KEEP_EVERY 1\n" + 
			"NUM_PARAMETERS 1\n" + 
			"Vmax\n" + 
			"NUM_EQUATIONS 3\n" + 
			"VAR rf_nucleus INIT (5.0);\n" + 
			"VAR BS_nucleus INIT (20.0);\n" + 
			"VAR rfB_nucleus INIT (0.0);\n" + 
			"TRANSFORM\n" + 
			"0.2 0.0 0.0 \n" + 
			"0.0 0.0 0.2 \n" + 
			"0.0 1.0 0.0 \n" + 
			"INVERSETRANSFORM\n" + 
			"5.0 0.0 0.0 \n" + 
			"0.0 0.0 1.0 \n" + 
			"0.0 5.0 0.0 \n" + 
			"RHS DIFFERENTIAL 2 ALGEBRAIC 1\n" + 
			"(0.2 * ( - (0.10000000000000002 * Vmax * rf_nucleus * ((t > 10.0) && (t < 10.5))) - ((0.02 * BS_nucleus * rf_nucleus) - (0.1 * rfB_nucleus))));\n" + 
			"(0.2 * ( - (5.000000000000001 * rfB_nucleus * ((t > 10.0) && (t < 10.5))) + (0.02 * BS_nucleus * rf_nucleus) - (0.1 * rfB_nucleus)));\n" + 
			"- ((0.1 * BS_nucleus * (-2.0 - (0.2 * rf_nucleus) + (0.2 * BS_nucleus))) - (0.5 * (4.0 - (0.2 * rfB_nucleus) - (0.2 * BS_nucleus))));\n";

	char* columnExpressions[200] = {"BS_nucleus", "(5.0 * (4.0 - (0.2 * rfB_nucleus) - (0.2 * BS_nucleus)))", "(5.0 * (-2.0 - (0.2 * rf_nucleus) + (0.2 * BS_nucleus)))"};
	
	OdeOptJob* odeOptJob = new OdeOptJob(NUM_PARAMS, paramNames, LB, UB, initialGuess, scaleFactors, 0, 0, 0, 0, 0, refData, columnExpressions, (char*)input.c_str(), 0);
	CFSQPOptSolver* cfsqpOptSolver = new CFSQPOptSolver(odeOptJob);
	OptSolverResultSet* optSolverResultSet = cfsqpOptSolver->solve();
	optSolverResultSet->show();
	delete odeOptJob;
	delete cfsqpOptSolver;
	

	//OdeMultiShootingOptJob* odeMultiShootingOptJob = new OdeMultiShootingOptJob(2, paramNames, LB, UB, initialGuess, 0, 0, 0, 0, 
	//	0, refData, columnExpressions, (char*)input.data(), 0.02, 0);
	//cfsqpOptSolver = new CFSQPOptSolver(odeMultiShootingOptJob);
	//optSolverResultSet = cfsqpOptSolver->solve();
	//optSolverResultSet->show();
	//delete odeMultiShootingOptJob;
	//delete cfsqpOptSolver;
	
}