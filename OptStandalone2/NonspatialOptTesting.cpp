#include "OptSolver2.h"
#include "OptProblemDescription.h"
#include "ObjectiveFunction.h"
#include "ExplicitObjectiveFunction.h"
#include "OdeObjectiveFunction.h"
#include "ConstraintDescription.h"
#include "Constraint.h"
#include "ParameterDescription.h"
#include "Expression.h"
#include "OdeResultSet.h"
#include "OptSolverCFSQP.h"
#include "OptResultSet.h"
#include "math.h"
#include <VCELL/FVSolver.h>
#include <float.h>
#include "NonspatialOptTesting.h"

#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
using namespace std;


void checkStopRequested(double objectiveFunc, long iter){
//	std::cout << "checkStopRequested " << objectiveFunc << " iter " << iter << std::endl;
}

Expression* readExpression(istream& ifsInput, string prefix) {	
	string expStr = "";
	getline(ifsInput, expStr);
	string newstr = prefix + expStr;
	expStr = trim(newstr);
	if (expStr[expStr.size()-1] != ';') {
		throw "Expression not terminated by ';'";
	}
	return new Expression(expStr);
}

OptProblemDescription* readOptProblem(istream& instream){
	//
	// read parameters
	//
	string token;
	instream >> token;  // read OptDescription
	if (token != "OptDescription"){
		throw "unknown token, expected OptDescription";
	}
	instream >> token;
	ParameterDescription* paramDesc = null;
	if (token == "Parameters"){
		int numParameters=0;
		instream >> numParameters;
		double* low = new double[numParameters];
		double* init = new double[numParameters];
		double* high = new double[numParameters];
		double* scale = new double[numParameters];
		vector<string> names;
		int paramIndex = 0;
		instream >> token;
		while (token != "EndParameters"){
			names.push_back(string(token));
			instream >> low[paramIndex] >> high[paramIndex] >> scale[paramIndex] >> init[paramIndex];
			paramIndex++;
			if (paramIndex>numParameters){
				throw "too many parameters";
			}
			instream >> token;
		}
		paramDesc = new SimpleParameterDescription(numParameters,names,low,high,init,scale);
	}
	instream >> token;
	vector<Constraint*> constraints;
	SymbolTable* symbolTable = paramDesc->getSymbolTable();
	ConstraintDescription* constraintDesc = null;
	if (token == "Constraints"){
		int numConstraints=0;
		instream >> numConstraints;
		instream >> token;
		int constraintIndex = 0;
		while (token != "EndConstraints"){
			ConstraintType type;
			if (token == "LinearEquality"){
				type = EQUALITY_LINEAR;
			}else if (token == "LinearInequality"){
				type = INEQUALITY_LINEAR;
			}else if (token == "NonlinearEquality"){
				type = EQUALITY_NONLINEAR;
			}else if (token == "NonlinearInequality"){
				type = INEQUALITY_NONLINEAR;
			}else{
				throw "unknown constraint type";
			}
			Expression* exp = readExpression(instream,"");
			Constraint* constraint = new Constraint(type,exp->infix().c_str());
			constraints.push_back(constraint);
			instream >> token;
			constraintIndex++;
			if (constraintIndex>numConstraints){
				throw "too many constraints";
			}
		}
		constraintDesc = new ExplicitConstraintDescription(constraints,symbolTable);
	}
	instream >> token;
	ObjectiveFunction* objFunction = null;
	if (token == "ObjectiveFunction"){
		string objFunctionType;
		instream >> objFunctionType;
		if (objFunctionType == "Explicit"){
			instream >> token;
			Expression* exp = readExpression(instream,token);
			objFunction = new ExplicitObjectiveFunction(exp,paramDesc,paramDesc->getSymbolTable(),checkStopRequested);
		}else{
			throw "unknown objective function type";
		}
	}
	OptProblemDescription* optProb = new OptProblemDescription(paramDesc,constraintDesc,objFunction);
	return optProb;
}

OptProblemDescription* createOdeSimple() {

	const int numParams = 2;
	const int NUM_REF_COLUMNS = 2;
	const int NUM_REF_ROWS = 11;

	cout << setprecision(30) << endl;
	double SCALE_INIT_REAL		= 1e-2;
	double SCALE_INIT_CHOSEN	= 0.5;
	double SCALE_KOFF_REAL		= 1e-5;
	double SCALE_KOFF_CHOSEN	= 1;
	double tau = 2.333;
	double A = 12.22;
	vector<string> paramNames;
	paramNames.push_back("init");
	paramNames.push_back("koff");
	double* lb = new double[numParams];
	lb[0] = 0.0;
	lb[1] = 0.0;
	double* ub = new double[numParams];
	ub[0] = 1000;
	ub[1] = 1000;
	double* init = new double[numParams];
	init[0] = 0;
	init[1] = 0;
	double* scales = new double[numParams];
	scales[0] = SCALE_INIT_CHOSEN;
	scales[1] = SCALE_KOFF_CHOSEN;
	double* weights = new double[NUM_REF_COLUMNS];
	weights[0] = 1.0;
	weights[1] = 1.0;
	OdeResultSet* refData = new OdeResultSet();
	char* refNames[20] = {"t", "c"};
	for (int i = 0; i < NUM_REF_COLUMNS; i ++) {
        refData->addColumn(refNames[i]);
	}
	refData->setColumnWeights(weights);
	double refRows[NUM_REF_ROWS][NUM_REF_COLUMNS] = {
							{0.0, A*(exp(-(0.0)/tau))},
							{1.0, A*(exp(-(1.0)/tau))},
							{2.0, A*(exp(-(2.0)/tau))},
							{3.0, A*(exp(-(3.0)/tau))},
							{4.0, A*(exp(-(4.0)/tau))},
							{5.0, A*(exp(-(5.0)/tau))},
							{6.0, A*(exp(-(6.0)/tau))},
							{7.0, A*(exp(-(7.0)/tau))},
							{8.0, A*(exp(-(8.0)/tau))},
							{9.0, A*(exp(-(9.0)/tau))},
							{10.0, A*(exp(-(10.0)/tau))}};
	for (int i = 0; i < NUM_REF_ROWS; i ++) {
        refData->addRow(refRows[i]);
	}
	stringstream ssKoff;
	ssKoff << SCALE_KOFF_REAL;
	stringstream ssInit;
	ssInit << SCALE_KOFF_REAL;

	string inputIDA = string("STARTING_TIME 0.0\n") + 
			"ENDING_TIME 10.0\n" + 
			"RELATIVE_TOLERANCE 1.0E-9\n" + 
			"ABSOLUTE_TOLERANCE 1.0E-9\n" + 
			"MAX_TIME_STEP 1.0\n" + 
			"KEEP_EVERY 1\n" + 
			"NUM_PARAMETERS 2\n" + 
			"init\n" + 
			"koff\n" + 
			"NUM_EQUATIONS 1\n" + 
			"VAR calcium INIT (init/"+ssInit.str()+");\n" + 
			"TRANSFORM\n" + 
			"1.0\n" + 
			"INVERSETRANSFORM\n" + 
			"1.0\n" + 
			"RHS DIFFERENTIAL 1 ALGEBRAIC 0\n" + 
			"-koff/"+ssKoff.str()+"*calcium;\n";

	string inputCVODE = string("STARTING_TIME 0.0\n") + 
			"ENDING_TIME 10.0\n" + 
			"RELATIVE_TOLERANCE 1.0E-9\n" + 
			"ABSOLUTE_TOLERANCE 1.0E-9\n" + 
			"MAX_TIME_STEP 1.0\n" + 
			"KEEP_EVERY 1\n" + 
			"NUM_PARAMETERS 2\n" + 
			"init\n" + 
			"koff\n" + 
			"NUM_EQUATIONS 1\n" + 
			"ODE calcium INIT (init/"+ssInit.str()+");\n"+
			"RATE -koff/"+ssKoff.str()+"*calcium;\n";
	
	string input;
	input = inputCVODE;
	//input = inputIDA;

	vector<string> columnExpressions;
	columnExpressions.push_back("calcium");
	ParameterDescription* paramDesc = new SimpleParameterDescription(numParams, paramNames, lb, ub, init, scales);
	OdeObjectiveFunction* odeObjFunc = new OdeObjectiveFunction(paramDesc,refData,columnExpressions,(char*)input.c_str(),checkStopRequested);
	ConstraintDescription* constraintDesc = new ExplicitConstraintDescription(std::vector<Constraint*>(),paramDesc->getSymbolTable());
	OptProblemDescription* optProb = new OptProblemDescription(paramDesc,constraintDesc,odeObjFunc);

	return optProb;
}
	
OptProblemDescription* createOde() {

	const int numParams = 1;
	const int NUM_REF_COLUMNS = 4;
	const int NUM_REF_ROWS = 11;

	cout << setprecision(30) << endl;

	vector<string> paramNames;
	paramNames.push_back("Vmax");
	double* lb = new double[numParams];
	lb[0] = 0.0;
	double* ub = new double[numParams];
	ub[0] = 1000;
	double* init = new double[numParams];
	init[0] = 0;
	double* scales = new double[numParams];
	scales[0] = 1.0;
	double* weights = new double[NUM_REF_COLUMNS];
	weights[0] = 1.0;
	weights[1] = 5.075729614413505E-4;
	weights[2] = 0.0031398209428204773;
	weights[3] = 0.021289489638613643;
	OdeResultSet* refData = new OdeResultSet();
	char* refNames[20] = {"t", "BS_nucleus", "rB_nucleus", "r_nucleus"};
	for (int i = 0; i < NUM_REF_COLUMNS; i ++) {
        refData->addColumn(refNames[i]);
	}
	refData->setColumnWeights(weights);
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
			"MAX_TIME_STEP 10.0\n" + 
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

	vector<string> columnExpressions;
	columnExpressions.push_back("BS_nucleus");
	columnExpressions.push_back("(5.0 * (4.0 - (0.2 * rfB_nucleus) - (0.2 * BS_nucleus)))");
	columnExpressions.push_back("(5.0 * (-2.0 - (0.2 * rf_nucleus) + (0.2 * BS_nucleus)))");
	ParameterDescription* paramDesc = new SimpleParameterDescription(numParams, paramNames, lb, ub, init, scales);
	OdeObjectiveFunction* odeObjFunc = new OdeObjectiveFunction(paramDesc,refData,columnExpressions,(char*)input.c_str(),checkStopRequested);
	ConstraintDescription* constraintDesc = new ExplicitConstraintDescription(std::vector<Constraint*>(),paramDesc->getSymbolTable());
	OptProblemDescription* optProb = new OptProblemDescription(paramDesc,constraintDesc,odeObjFunc);

	return optProb;
}
	

OptProblemDescription* create1() {
	int numParams = 3;
	vector<string> paramNames;
	paramNames.push_back("x0");
	paramNames.push_back("x1");
	paramNames.push_back("x2");
	double* lb = new double[numParams];
	lb[0] = 0.0;
	lb[1] = 0.0;
	lb[2] = 0.0;
	double* ub = new double[numParams];
	ub[0] = 100;
	ub[1] = 100;
	ub[2] = 100;
	double* init = new double[numParams];
	init[0] = 0.1;
	init[1] = 0.7;
	init[2] = 0.2;
	double* scales = new double[numParams];
	scales[0] = 1;
	scales[1] = 1;
	scales[2] = 1;

	ParameterDescription* parameterDescription = new SimpleParameterDescription(numParams,paramNames,lb,ub,init,scales);

	vector<Constraint*> constraints;
	constraints.push_back(new Constraint(INEQUALITY_NONLINEAR,"pow(x0,3)-6*x1-4*x2+3"));
	constraints.push_back(new Constraint(INEQUALITY_LINEAR,"10-x0-x1-x2"));
	ConstraintDescription* constraintDescription = new ExplicitConstraintDescription(constraints,parameterDescription->getSymbolTable());

	ObjectiveFunction* objectiveFunction = new ExplicitObjectiveFunction(
		new Expression(string("pow(x0-1,2)+pow(x1-1,2)+pow(x2-1,2)")),
		parameterDescription,parameterDescription->getSymbolTable(),checkStopRequested);

	OptProblemDescription* optProb = new OptProblemDescription(parameterDescription, constraintDescription, objectiveFunction);

	return optProb;
};


void runExplitOptTest1(){
	OptProblemDescription* optProb = create1();
	OptSolverCFSQP* cfsqpOptSolver = new OptSolverCFSQP(optProb);
	OptResultSet* optResultSet = cfsqpOptSolver->solve();
	optResultSet->show();
	optResultSet->show();
}

string inputSimple = 
"OptDescription\n"
"Parameters 2\n"
"calcium 0.001 0.6 1.0 0.5\n"
"ip3 0.01 100.0 1.0 1.0\n"
"EndParameters\n"
"Constraints 1\n"
"LinearEquality	(calcium - ip3);\n"
"EndConstraints\n"
"ObjectiveFunction Explicit\n"
"(calcium + ip3);\n"
"EndObjectiveFunction\n"
"EndOptDescription";

string inputCVODE = 
"OptDescription\n"
"Parameters 2\n"
"calcium 0.001 0.6 1.0 0.5\n"
"ip3 0.01 100.0 1.0 1.0\n"
"EndParameters\n"
"Constraints 1\n"
"LinearEquality	(calcium - ip3);\n"
"EndConstraints\n"
"ObjectiveFunction ModelData\n"
"Model CVODE\n"
"STARTING_TIME 0.0\n" 
"ENDING_TIME 10.0\n" 
"RELATIVE_TOLERANCE 1.0E-9\n"
"ABSOLUTE_TOLERANCE 1.0E-9\n"
"MAX_TIME_STEP 1.0\n"
"KEEP_EVERY 1\n"
"NUM_PARAMETERS 2\n"
"init\n"
"koff\n"
"NUM_EQUATIONS 1\n"
"ODE calcium INIT init;\n"
"RATE -koff*calcium;\n"
"EndModel\n"
"Data Simple\n"
"Independent t 1\n"
"Dependent calcium 1\n"
"Rows 10\n"
"EndData\n"
"EndObjectiveFunction\n"
"EndOptDescription";


void runOdeOptTesting(){
	stringstream instream(inputSimple);
	OptProblemDescription* optProb = readOptProblem(instream);
//	OptProblemDescription* optProb = createOdeSimple();
	OptSolverCFSQP* cfsqpOptSolver = new OptSolverCFSQP(optProb);
	OptResultSet* optResultSet = cfsqpOptSolver->solve();
	optResultSet->show();
	optResultSet->show();
	delete optProb;
	delete cfsqpOptSolver;
}
