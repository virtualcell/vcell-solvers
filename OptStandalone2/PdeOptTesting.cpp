#include "OptSolverCFSQP.h"
#include "PdeObjectiveFunction.h"
#include "VFRAPObjectiveFunction.h"
#include "SimpleParameterDescription.h"
#include "SpatialReferenceData.h"
#include "ExplicitConstraintDescription.h"
#include "OptProblemDescription.h"
#include "OdeResultSet.h"
#include "OptResultSet.h"
#include "math.h"
#include <VCELL/FVSolver.h>
#include <float.h>
#include "PdeOptTesting.h"

#include <iomanip>
#include <iostream>
#include <sstream>

using namespace std;


void checkStopRequested_PDE(double objectiveFunc, long iter){
	std::cout << "checkStopRequested " << objectiveFunc << " iter " << iter << std::endl;
}


void printVars(FVSolver *fv, int arrayID){
	int numVars = fv->getNumVariables();
	for (int i=0;i<numVars;i++){
		std::string& varName = fv->getVariableName(i);
		std::cout << varName << "[" << arrayID << "]" << endl;
		int length = fv->getVariableLength(varName);
		double* data = fv->getValue(varName, arrayID);
		for (int j=0;j<length;j++){
			std::cout << data[j] << " ";
			if (j%10==0){
				std::cout << std::endl;
			}
		}
	}
	std::cout << std::endl;
	std::cout << "done" << std::endl;
	std::cout.flush();
}

OptProblemDescription* createExactPdeDiff() {

	const int numParams = 3;
	const int NUM_REF_VARIABLES = 1;
	const int NUM_REF_TIMEPOINTS = 11;

	cout << setprecision(30) << endl;
	double SCALE_INIT_REAL		= 1;
	double SCALE_INIT_CHOSEN	= 1;
	double SCALE_KOFF_REAL		= 1;
	double SCALE_KOFF_CHOSEN	= 1;
	vector<string> paramNames;
	paramNames.push_back("D");
	paramNames.push_back("U0");
	paramNames.push_back("U1");
	double* lb = new double[numParams];
	lb[0] = 0.1;
	lb[1] = .1;
	lb[2] = .1;
//	lb[3] = .1;
	double* ub = new double[numParams];
	ub[0] = 20;
	ub[1] = 10;
	ub[2] = 10;
//	ub[3] = 10;
	double* init = new double[numParams];
	init[0] = 5;
	init[1] = 5;
	init[2] = 5;
//	init[3] = 5;
	double* scales = new double[numParams];
	scales[0] = SCALE_INIT_CHOSEN;
	scales[1] = SCALE_INIT_CHOSEN;
	scales[2] = SCALE_INIT_CHOSEN;
//	scales[3] = SCALE_INIT_CHOSEN;
	double* weights = new double[NUM_REF_VARIABLES];
	weights[0] = 1.0;
	int varIndex = 0;
	int numX = 10;
	int numY = 1;
	SpatialReferenceData* refData = new SpatialReferenceData(numX*numY);
	char* refNames[20] = {"t", "calcium"};
//	refData->setColumnWeights(weights);
	for (int i = 0; i < NUM_REF_VARIABLES; i ++) {
        refData->addVariable(string(refNames[i+1])); // +1 to skip time
	}
	double endTime = 1;
	int N = 10;
	double startX = 0;
	double endX = 10;
	double D = 1;
	double U0 = 2;
	double U1 = 1;
	double V = 1;
	for (int i=0;i<N;i++){
		double time = i*endTime/N;
		refData->addTimePoint(time);
		double* data = new double[numX*numY];
		for (int j=0;j<numX;j++){
			double x = startX + ((endX-startX)*j)/(numX-1);
			for (int k=0;k<numY;k++){
				// traveling wave solution
				data[j+k*numX] = U0+U1*exp(-V/D*(x-V*time));
			}
		}
		refData->addVariableData(varIndex,data);
	}
refData->show();
	stringstream ssKoff;
	ssKoff << SCALE_KOFF_REAL;
	stringstream ssInit;
	ssInit << SCALE_KOFF_REAL;

	string input = "# Simulation Parameters\n"
"SIMULATION_PARAM_BEGIN\n"
"BASE_FILE_NAME wave100x3\n"
"ENDING_TIME 10\n"
"TIME_STEP 0.2\n"
"KEEP_EVERY 1\n"
"STORE_ENABLE 1\n"
"SIMULATION_PARAM_END\n"
"\n"
"# Model description: FEATURE name handle priority boundary_conditions\n"
"MODEL_BEGIN\n"
"FEATURE cytosol 0 200 value value flux flux \n"
"MODEL_END\n"
"\n"
"# Mesh file\n"
"MESH_BEGIN\n"
"VCG_FILE c:\\Vcell\\users\\fgao\\wave200x200.vcg\n"
"MESH_END\n"
"\n"
"# Variables : type name unit time_dependent_flag advection_flag solve_regions\n"
"VARIABLE_BEGIN\n"
"VOLUME_PDE calcium uM false false\n"
"VARIABLE_END\n"
"\n"
"PARAMETER_BEGIN 3\n"
"D\n"
"U0\n"
"U1\n"
//"V\n"
"PARAMETER_END\n"
"\n"
"COMPARTMENT_BEGIN cytosol\n"
"\n"
"BOUNDARY_CONDITIONS value value flux flux \n"
"\n"
"EQUATION_BEGIN calcium\n"
"INITIAL U0+U1*exp(-(1/D)*(x-1*t));\n"
"RATE 0;\n"
"DIFFUSION D;\n"
"VELOCITY_X 0.0;\n"
"VELOCITY_Y 0.0;\n"
"BOUNDARY_XM U0+U1*exp(-(1/D)*(x-1*t));\n"
"BOUNDARY_XP U0+U1*exp(-(1/D)*(x-1*t));\n"
"BOUNDARY_YM 0.0;\n"
"BOUNDARY_YP 0.0;\n"
"EQUATION_END\n"
"\n"
"COMPARTMENT_END\n"
"\n"
"\n";

	char* columnExpressions[200] = {"calcium"};
	ParameterDescription* paramDesc = new SimpleParameterDescription(numParams, paramNames, lb, ub, init, scales);
	PdeObjectiveFunction* pdeObjFunc = new PdeObjectiveFunction(paramDesc,refData,columnExpressions,(char*)input.c_str(),checkStopRequested_PDE);
	ConstraintDescription* constraintDesc = new ExplicitConstraintDescription(std::vector<Constraint*>(),paramDesc->getSymbolTable());
	OptProblemDescription* optProb = new OptProblemDescription(paramDesc,constraintDesc,pdeObjFunc);

	return optProb;
}



OptProblemDescription* createExactPdeNoDiffusion() {

	const int numParams = 2;
	const int NUM_REF_VARIABLES = 1;
	const int NUM_REF_TIMEPOINTS = 11;

	cout << setprecision(30) << endl;
	double SCALE_INIT_REAL		= 1;
	double SCALE_INIT_CHOSEN	= 1;
	double SCALE_KOFF_REAL		= 1;
	double SCALE_KOFF_CHOSEN	= 1;
	vector<string> paramNames;
	paramNames.push_back("init");
	paramNames.push_back("koff");
	double* lb = new double[numParams];
	lb[0] = .04;
	lb[1] = .04;
	double* ub = new double[numParams];
	ub[0] = 15;
	ub[1] = 15;
	double* init = new double[numParams];
	init[0] = 10;
	init[1] = 0.1;
	double* scales = new double[numParams];
	scales[0] = SCALE_INIT_CHOSEN;
	scales[1] = SCALE_KOFF_CHOSEN;
	double* weights = new double[NUM_REF_VARIABLES];
	weights[0] = 1.0;
	int varIndex = 0;
	int numX = 6;
	int numY = 6;
	SpatialReferenceData* refData = new SpatialReferenceData(numX*numY);
	char* refNames[20] = {"t", "calcium"};
//	refData->setColumnWeights(weights);
	for (int i = 0; i < NUM_REF_VARIABLES; i ++) {
        refData->addVariable(string(refNames[i+1])); // +1 to skip time
	}
	double endTime = 10;
	int N = 10;
	double INIT = 5;
	double KOFF = 0.5;
	for (int i=0;i<N;i++){
		double time = i*endTime/N;
		refData->addTimePoint(time);
		double* data = new double[numX*numY];
		double tau = 1.0/KOFF;
		for (int j=0;j<(numX*numY);j++){
			data[j] = INIT*exp(-time/tau);
			//data[j] = INIT*(1-KOFF*time);
		}
		refData->addVariableData(varIndex,data);
	}
	stringstream ssKoff;
	ssKoff << SCALE_KOFF_REAL;
	stringstream ssInit;
	ssInit << SCALE_KOFF_REAL;

	string input = "# Simulation Parameters\n"
"SIMULATION_PARAM_BEGIN\n"
"BASE_FILE_NAME SimID_26794493_0_\n"
"ENDING_TIME 10\n"
"TIME_STEP 0.1\n"
"KEEP_EVERY 1\n"
"STORE_ENABLE 0\n"
"SIMULATION_PARAM_END\n"
"\n"
"# Model description: FEATURE name handle priority boundary_conditions\n"
"MODEL_BEGIN\n"
"FEATURE cytosol 0 200 flux flux flux flux \n"
"FEATURE extracellular 1 101 flux flux flux flux\n" 
"MODEL_END\n"
"\n"
"# Mesh file\n"
"MESH_BEGIN\n"
"VCG_FILE c:\\Vcell\\users\\fgao\\SimID_26794493_0_.vcg\n"
"MESH_END\n"
"\n"
"# Variables : type name unit time_dependent_flag advection_flag solve_regions\n"
"VARIABLE_BEGIN\n"
"VOLUME_PDE calcium uM true false\n"
"VARIABLE_END\n"
"\n"
"PARAMETER_BEGIN 2\n"
"init\n"
"koff\n"
"PARAMETER_END\n"
"\n"
"COMPARTMENT_BEGIN cytosol\n"
"\n"
"BOUNDARY_CONDITIONS flux flux flux flux \n"
"\n"
"EQUATION_BEGIN calcium\n"
"INITIAL init;\n"
"RATE -koff*calcium;\n"
"DIFFUSION 1;\n"
"VELOCITY_X 0.0;\n"
"VELOCITY_Y 0.0;\n"
"BOUNDARY_XM 0.0;\n"
"BOUNDARY_XP 0.0;\n"
"BOUNDARY_YM 0.0;\n"
"BOUNDARY_YP 0.0;\n"
"EQUATION_END\n"
"\n"
"COMPARTMENT_END\n"
"\n"
"COMPARTMENT_BEGIN extracellular\n"
"\n"
"BOUNDARY_CONDITIONS flux flux flux flux \n"
"\n"
"EQUATION_BEGIN calcium\n"
"INITIAL init;\n"
"RATE -koff*calcium;\n"
"DIFFUSION 1;\n"
"VELOCITY_X 0.0;\n"
"VELOCITY_Y 0.0;\n"
"BOUNDARY_XM 0.0;\n"
"BOUNDARY_XP 0.0;\n"
"BOUNDARY_YM 0.0;\n"
"BOUNDARY_YP 0.0;\n"
"EQUATION_END\n"
"\n"
"COMPARTMENT_END\n"
"\n"
"\n"
"MEMBRANE_BEGIN cytosol_extracellular_membrane cytosol extracellular\n"
"\n"
"BOUNDARY_CONDITIONS flux flux flux flux \n"
"\n"
"JUMP_CONDITION_BEGIN calcium\n"
"INFLUX 0.0;\n"
"OUTFLUX 0.0;\n"
"JUMP_CONDITION_END\n"
"\n"
"MEMBRANE_END\n"
"\n";

	char* columnExpressions[200] = {"calcium"};
	ParameterDescription* paramDesc = new SimpleParameterDescription(numParams, paramNames, lb, ub, init, scales);
	PdeObjectiveFunction* pdeObjFunc = new PdeObjectiveFunction(paramDesc,refData,columnExpressions,(char*)input.c_str(),checkStopRequested_PDE);
	ConstraintDescription* constraintDesc = new ExplicitConstraintDescription(std::vector<Constraint*>(),paramDesc->getSymbolTable());
	OptProblemDescription* optProb = new OptProblemDescription(paramDesc,constraintDesc,pdeObjFunc);

	return optProb;
}

void runPdeOptTesting() {

//	OptProblemDescription* optProb = createExactPdeNoDiffusion();
	OptProblemDescription* optProb = createExactPdeDiff();
	OptSolverCFSQP* cfsqpOptSolver = new OptSolverCFSQP(optProb);
	OptResultSet* optResultSet = cfsqpOptSolver->solve();
	optResultSet->show();
	optResultSet->show();
}

OptProblemDescription* createExactVFRAPProblem() {

	const int numParams = 4;

	cout << setprecision(30) << endl;
	vector<string> paramNames;
	paramNames.push_back("tau");
	paramNames.push_back("alpha");
	paramNames.push_back("scale1");
	paramNames.push_back("separation");
	double* lb = new double[numParams];
	lb[0] = 100;
	lb[1] = 1;
	lb[2] = .1;
	lb[3] = 1;
	double* ub = new double[numParams];
	ub[0] = 100;
	ub[1] = 1;
	ub[2] = 10;
	ub[3] = 1;
	double* init = new double[numParams];
	init[0] = 100;
	init[1] = 1;
	init[2] = 1;
	init[3] = 1;
	double* scales = new double[numParams];
	scales[0] = 1;
	scales[1] = 1;
	scales[2] = 1;
	scales[3] = 1;
	int varIndex = 0;
	int numX = 10;
	int numY = 1;
	SpatialReferenceData* refData = new SpatialReferenceData(numX*numY);
	refData->addVariable(string("calcium"));
	double endTime = 1;
	int N = 10;
	double startX = 0;
	double endX = 10;
	double D1 = 1;
	double D2 = 3;
	double alpha = 1;
	double tau = 100;
	double t0 = 0.1;
	for (int i=0;i<N;i++){
		double time = i*endTime/N;
		double t = t0+time;
		refData->addTimePoint(time);
		double* data = new double[numX*numY];
		for (int j=0;j<numX;j++){
			double x = startX + ((endX-startX)*j)/(numX-1);
			for (int k=0;k<numY;k++){
				if (time==0){
					data[j+k*numX] = exp(-time/tau)*exp(-(x*x)/(4*D1*t));
				}else{
					data[j+k*numX] = exp(-time/tau)*sqrt(t0/time)*(alpha*exp(-(x*x)/(4*D1*t)) + (1-alpha)*exp(-(x*x)/(4*D2*t)));
				}
			}
		}
		refData->addVariableData(varIndex,data);
	}

	//
	// create spatial dataset similar to diffusion with a single diffusion rate
	//
	//
	SpatialReferenceData* simData = new SpatialReferenceData(numX*numY);
	simData->addVariable(string("calcium"));
	double simD = 1;
	endTime = 10;
	N = 50;
	for (int i=0;i<N;i++){
		double time = i*endTime/N;
		double t = t0+time;
		simData->addTimePoint(time);
		double* data = new double[numX*numY];
		for (int j=0;j<numX;j++){
			double x = startX + ((endX-startX)*j)/(numX-1);
			for (int k=0;k<numY;k++){
				if (time==0){
					data[j+k*numX] = exp(-(x*x)/(4*simD*t));
				}else{
					data[j+k*numX] = sqrt(t0/time)*exp(-(x*x)/(4*simD*t));
				}
			}
		}
		simData->addVariableData(varIndex,data);
	}
	std::cout << "refData" << std::endl;
	refData->show();
	std::cout << std::endl << std::endl << "simData" << std::endl;
	simData->show();
	std::cout << std::endl;


	ParameterDescription* paramDesc = new SimpleParameterDescription(numParams, paramNames, lb, ub, init, scales);
	VFRAPObjectiveFunction* vfrapObjFunc = new VFRAPObjectiveFunction(paramDesc,refData,simData,checkStopRequested_PDE);
	ConstraintDescription* constraintDesc = new ExplicitConstraintDescription(std::vector<Constraint*>(),paramDesc->getSymbolTable());
	OptProblemDescription* optProb = new OptProblemDescription(paramDesc,constraintDesc,vfrapObjFunc);

	return optProb;
}
/**
OptProblemDescription* createExactVFRAPTriangles() {

	const int numParams = 3;

	//cout << setprecision(30) << endl;
	char* paramNames[20] = {"alpha", "scale1", "separation" };
	double* lb = new double[numParams];
	lb[0] = 1;
	lb[1] = .1;
	lb[2] = .1;
	double* ub = new double[numParams];
	ub[0] = 1000;
	ub[1] = 1;
	ub[2] = 100;
	double* init = new double[numParams];
	init[0] = 100;
	init[1] = 0.7;
	init[2] = 5;
	double* scales = new double[numParams];
	scales[0] = 1;
	scales[1] = 1;
	scales[2] = 1;
	int numX = 11;
	int numY = 1;
	SpatialReferenceData* refData = new SpatialReferenceData(numX*numY);
	refData->addVariable(string("calcium"));
	double endTime = 0;
	int N = 1;
	double startX = 0;
	double endX = 1;
	double alpha = 0.5;
	double scale = 1;
	double separationScale = 2;
	int varIndex = 0;
	for (int i=0;i<N;i++){
		double time = i*endTime/N;
		refData->addTimePoint(time);
		double* data = new double[numX*numY];
		for (int j=0;j<numX;j++){
			double x = startX + ((endX-startX)*j)/(numX-1);
			for (int k=0;k<numY;k++){
				double triangle1 = alpha*(1.0-scale*x);
				if (triangle1<0){
					triangle1 = 0;
				}
				double triangle2 = (1-alpha)*(1.0-scale*separationScale*x);
				if (triangle2<0){
					triangle2 = 0;
				}
				data[j+k*numX] = triangle1 + triangle2;
			}
		}
		refData->addVariableData(varIndex,data);
	}

	//
	// create spatial dataset similar to diffusion with a single diffusion rate
	//
	//
	SpatialReferenceData* simData = new SpatialReferenceData(numX*numY);
	simData->addVariable(string("calcium"));
	endTime = 0;
	N = 1;
	for (int i=0;i<N;i++){
		double time = i*endTime/N;
		simData->addTimePoint(time);
		double* data = new double[numX*numY];
		for (int j=0;j<numX;j++){
			double x = startX + ((endX-startX)*j)/(numX-1);
			for (int k=0;k<numY;k++){
				double unitTriangle = 1.0-x;
				if (unitTriangle<0){
					unitTriangle = 0;
				}
				data[j+k*numX] = unitTriangle;
			}
		}
		simData->addVariableData(varIndex,data);
	}
	std::cout << "refData" << std::endl;
	refData->show();
	std::cout << std::endl << std::endl << "simData" << std::endl;
	simData->show();
	std::cout << std::endl;


	ParameterDescription* paramDesc = new SimpleParameterDescription(numParams, paramNames, lb, ub, init, scales);
	VFRAPObjectiveFunction* vfrapObjFunc = new VFRAPObjectiveFunction(paramDesc,refData,simData,checkStopRequested_PDE);
	ConstraintDescription* constraintDesc = new ExplicitConstraintDescription(std::vector<Constraint*>(),paramDesc->getSymbolTable());
	OptProblemDescription* optProb = new OptProblemDescription(paramDesc,constraintDesc,vfrapObjFunc);

	return optProb;
}
**/

void runVFRAPOptTesting() {
//	OptProblemDescription* optProb = createExactVFRAPTriangles();
	OptProblemDescription* optProb = createExactVFRAPProblem();
	OptSolverCFSQP* cfsqpOptSolver = new OptSolverCFSQP(optProb);
	OptResultSet* optResultSet = cfsqpOptSolver->solve();
	optResultSet->show();
	optResultSet->show();
}
