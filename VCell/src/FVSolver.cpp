#include <sys/stat.h>
#include <VCELL/FVSolver.h>
#include <VCELL/VolumeVariable.h>
#include <VCELL/VolumeRegionVariable.h>
#include <VCELL/MembraneVariable.h>
#include <VCELL/MembraneRegionVariable.h>
#include <VCELL/VCellModel.h>
#include <VCELL/Feature.h>
#include <VCELL/Mesh.h>
#include <VCELL/SimTool.h>
#include <VCELL/ODESolver.h>
#include <VCELL/EqnBuilderReactionForward.h>
#include <VCELL/MembraneEqnBuilderForward.h>
#include <VCELL/Element.h>
#include <VCELL/VolumeRegion.h>
#include <VCELL/MembraneRegion.h>
#include <VCELL/MembraneRegionEqnBuilder.h>
#include <VCELL/VolumeRegionEqnBuilder.h>
#include <VCELL/SparseMatrixEqnBuilder.h>
#include <VCELL/SparseVolumeEqnBuilder.h>
#include <VCELL/EllipticVolumeEqnBuilder.h>
#include <VCELL/MembraneEqnBuilderDiffusion.h>
#include <VCELL/SparseLinearSolver.h>
#include <VCELL/VarContext.h>
#include <VCELL/FastSystemExpression.h>
#include <VCELL/VolumeVarContextExpression.h>
#include <VCELL/VolumeRegionVarContextExpression.h>
#include <VCELL/MembraneVarContextExpression.h>
#include <VCELL/MembraneRegionVarContextExpression.h>
#include <VCELL/SimulationMessaging.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/CartesianMesh.h>
#include <VCELL/FieldData.h>

#include <Exception.h>
#include <Expression.h>

#include <assert.h>
#include <fstream>
#include <string>
using namespace std;

void FVSolver::loadJMSInfo(istream& ifsInput, int taskID) {
	char *broker = new char[256];
	char *smqusername = new char[256];
	char *password = new char[256];
	char *qname = new char[256];
	char *tname = new char[256];
	char *vcusername = new char[256];
	string nextToken;
	int simKey, jobIndex;

	while (!ifsInput.eof()) {			
		nextToken = "";
		ifsInput >> nextToken;			
		if (nextToken.size() == 0) {
			continue;
		} else if (nextToken[0] == '#') {
			getline(ifsInput, nextToken);
			continue;
		}  else if (nextToken == "JMS_PARAM_END") {
			break;
		} else if (nextToken == "JMS_BROKER") {
			memset(broker, 0, 256 * sizeof(char));
			ifsInput >> broker;
		} else if (nextToken == "JMS_USER") {
			memset(smqusername, 0, 256 * sizeof(char));
			memset(password, 0, 256 * sizeof(char));
			ifsInput >> smqusername >> password;
		} else if (nextToken == "JMS_QUEUE") {
			memset(qname, 0, 256 * sizeof(char));
			ifsInput >> qname;
		} else if (nextToken == "JMS_TOPIC") {
			memset(tname, 0, 256 * sizeof(char));
			ifsInput >> tname;
		} else if (nextToken == "VCELL_USER") {
			memset(vcusername, 0, 256 * sizeof(char));
			ifsInput >> vcusername;
		} else if (nextToken == "SIMULATION_KEY") {
			ifsInput >> simKey;
			continue;
		} else if (nextToken == "JOB_INDEX") {
			ifsInput >> jobIndex;
			continue;
		} 
	}

#ifdef USE_MESSAGING	
	if (taskID >= 0) {
		SimulationMessaging::create(broker, smqusername, password, qname, tname, vcusername, simKey, jobIndex, taskID);
	} else {
		SimulationMessaging::create();
	}
#else
	SimulationMessaging::create();
#endif
}

void FVSolver::loadModel(istream& ifsInput) {
	//cout << "loading model " << endl;
	model = new VCellModel();
	string nextToken;
	string feature_name;
	int handle, priority;	
	int numFeatures = 0;

	while (!ifsInput.eof()) {			
		nextToken = "";
		ifsInput >> nextToken;			
		if (nextToken.size() == 0) {
			continue;
		} else if (nextToken[0] == '#') {
			getline(ifsInput, nextToken);
			continue;
		} else if (nextToken == "MODEL_END") {
			break;
		} else if (nextToken == "FEATURE") {
			numFeatures ++;
			ifsInput >> feature_name >> handle >> priority;
			Feature* feature = new Feature(feature_name, handle, priority);
			model->addFeature(feature);

			char line[1000];
			string btstr;
			ifsInput.getline(line, 1000);
			stringstream ss(line);
			for (int i = 0; i < 6; i ++) {
				if (ss.eof()) {
					break;
				}
				ss >> btstr;
				if (btstr.length() == 0) {
					break;
				}
				BoundaryType bt = BOUNDARY_VALUE;
				if (btstr == "flux") {
					bt = BOUNDARY_FLUX;
				} else if (btstr == "value") {
					bt = BOUNDARY_VALUE;
				} else if (btstr == "periodic") {
					bt = BOUNDARY_PERIODIC;					
				} else {
					stringstream ss1;
					ss << "loadModel(), wrong boundary type " << btstr;
					throw ss.str();
				}
				switch (i) {
					case 0: // XM
						feature->setXmBoundaryType(bt);
						break;
					case 1: // XP
						feature->setXpBoundaryType(bt);
						break;
					case 2: // YM
						feature->setYmBoundaryType(bt);
						break;
					case 3: // YP
						feature->setYpBoundaryType(bt);
						break;
					case 4: // ZM
						feature->setZmBoundaryType(bt);
						break;
					case 5: // ZP
						feature->setZpBoundaryType(bt);
						break;
				}
			}			
		}
	}
}

void trimString(string& str)
{
	string::size_type pos = str.find_last_not_of(" \r\n");
	if(pos != string::npos) {
		str.erase(pos + 1);
		pos = str.find_first_not_of(" \r\n");
		if(pos != string::npos) {
			str.erase(0, pos);
		}
	}
	else {
		str.erase(str.begin(), str.end());
	}
}

int loadSolveRegions(CartesianMesh* mesh, string& line, int* solveRegions) {
	int numVolumeRegions = mesh->getNumVolumeRegions();
	istringstream instream(line);
	int regionCount = 0;
	while (true) {
		string feature_name = "";
		instream >> feature_name;					
		if (feature_name == "") {
			break;
		}
		for (int i = 0; i < numVolumeRegions; i++){
			VolumeRegion *volRegion = mesh->getVolumeRegion(i);
			Feature* feature = SimTool::getInstance()->getModel()->getFeatureFromName(feature_name);
			if (feature == NULL) {
				stringstream ss;
				ss << "Feature '" << feature_name << "' doesn't exist!";
				throw ss.str();
			}
			if (volRegion->getFeature()->getHandle() == (FeatureHandle)(0xff & feature->getHandle())){ 
				solveRegions[regionCount++] = volRegion->getId();
			}
		}
	}
	return regionCount;
}

void FVSolver::loadSimulation(istream& ifsInput) {
	//cout << "loading simulation" << endl;
	simulation = new SimulationExpression(mesh);
	string nextToken;
	long sizeX = mesh->getNumVolumeX();
	long sizeY = mesh->getNumVolumeY();
	long sizeZ = mesh->getNumVolumeZ();
	int numVolumeRegions = mesh->getNumVolumeRegions();
	string variable_name, unit;

	while (!ifsInput.eof()) {			
		nextToken = "";
		ifsInput >> nextToken;			
		if (nextToken.size() == 0) {
			continue;
		} else if (nextToken[0] == '#') {
			getline(ifsInput, nextToken);
			continue;
		} else if (nextToken == "VARIABLE_END") {
			break;
		} else if (nextToken == "VOLUME_PDE" || nextToken == "VOLUME_PDE_STEADY") {
			bool bSteady = false;
			if (nextToken == "VOLUME_PDE_STEADY") {
				bSteady = true;
			}

			bool bNoConvection = true;
			bool bTimeDependent = false;
			string advectionflag, time_dependent_diffusion_flag, solve_whole_mesh_flag;
			ifsInput >> variable_name >> unit >> time_dependent_diffusion_flag >> advectionflag >> solve_whole_mesh_flag;			

			bool bSolveVariable = true;
			int numSolveRegions = 0;  // flag specifying to solve for all regions
			int *solveRegions = NULL;

			if (solve_whole_mesh_flag == "false") {
				string line;
				getline(ifsInput, line);
				trimString(line);

				if (line.length() == 0) {
					bSolveVariable = false;
				} else {
					solveRegions = new int[numVolumeRegions];				
					numSolveRegions = loadSolveRegions(mesh, line, solveRegions);
				}
			}

			VolumeVariable* volumeVar = new VolumeVariable(sizeX, sizeY, sizeZ, variable_name, unit);
			if (bSolveVariable) {				
				if (advectionflag == "true" ) {
					bNoConvection = false;
				}
				if (time_dependent_diffusion_flag == "true") {
					bTimeDependent = true;
				}				
				SparseMatrixEqnBuilder* builder = 0;
				if (bSteady) {
					builder = new EllipticVolumeEqnBuilder(volumeVar,mesh, numSolveRegions, solveRegions);
				} else {
					builder = new SparseVolumeEqnBuilder(volumeVar,mesh, bNoConvection, numSolveRegions, solveRegions);
				}
				PDESolver* pdeSolver = new SparseLinearSolver(volumeVar,builder,bTimeDependent);
				simulation->addSolver(pdeSolver);
			}
			simulation->addVariable(volumeVar);
		} else if (nextToken == "VOLUME_ODE") {
			string solve_whole_mesh_flag;
			ifsInput >> variable_name >> unit >> solve_whole_mesh_flag;

			int numSolveRegions = 0;  // flag specifying to solve for all regions
			int *solveRegions = NULL;

			bool bSolveVariable = true;
			if (solve_whole_mesh_flag == "false") {
				string line;
				getline(ifsInput, line);
				trimString(line);

				if (line.length() == 0) {
					bSolveVariable = false;
				} else {
					solveRegions = new int[numVolumeRegions];				
					numSolveRegions = loadSolveRegions(mesh, line, solveRegions);
				}
			}

			VolumeVariable* volumeVar = new VolumeVariable(sizeX, sizeY, sizeZ, variable_name, unit);
			if (bSolveVariable) {
				ODESolver* odeSolver = new ODESolver(volumeVar,mesh,numSolveRegions,solveRegions);
				EqnBuilder* builder = new EqnBuilderReactionForward(volumeVar,mesh,odeSolver);
				odeSolver->setEqnBuilder(builder);
				simulation->addSolver(odeSolver);
			}
			simulation->addVariable(volumeVar);
		} else if (nextToken == "MEMBRANE_ODE") {
			ifsInput >> variable_name >> unit;
			int numSolveRegions = 0;  // flag specifying to solve for all regions
			int *solveRegions = NULL;
			MembraneVariable* membraneVar = new MembraneVariable(mesh->getNumMembraneElements(), variable_name, unit);
			ODESolver* odeSolver = new ODESolver(membraneVar,mesh,numSolveRegions,solveRegions);
			EqnBuilder* builder = new MembraneEqnBuilderForward(membraneVar,mesh,odeSolver);
			odeSolver->setEqnBuilder(builder);
			simulation->addSolver(odeSolver);
			simulation->addVariable(membraneVar);		
		} else if (nextToken == "MEMBRANE_PDE") {
			bool bNoConvection = true;    // define symmflg = 0 (general) or 1 (symmetric)
			bool bTimeDependent = false;
			string time_dependent_diffusion_flag;
			ifsInput >> variable_name >> unit >> time_dependent_diffusion_flag;
			if (time_dependent_diffusion_flag == "true") {
				bTimeDependent = true;
			}
			MembraneVariable* membraneVar = new MembraneVariable(mesh->getNumMembraneElements(), variable_name, unit);
			SparseMatrixEqnBuilder* smbuilder = new MembraneEqnBuilderDiffusion(membraneVar,mesh);
			SparseLinearSolver* slSolver = new SparseLinearSolver(membraneVar,smbuilder,bTimeDependent);
			simulation->addSolver(slSolver);
			simulation->addVariable(membraneVar);
		} else if (nextToken == "VOLUME_REGION") {
			ifsInput >> variable_name >> unit;
			int numSolveRegions = 0;  // flag specifying to solve for all regions
			int *solveRegions = NULL;
			VolumeRegionVariable* volumeRegionVar = new VolumeRegionVariable(mesh->getNumVolumeRegions(), variable_name, unit);
			ODESolver* odeSolver = new ODESolver(volumeRegionVar,mesh,numSolveRegions,solveRegions);
			EqnBuilder* builder = new VolumeRegionEqnBuilder(volumeRegionVar,mesh,odeSolver);
			odeSolver->setEqnBuilder(builder);
			simulation->addSolver(odeSolver);
			simulation->addVariable(volumeRegionVar);
		} else if (nextToken == "MEMBRANE_REGION") {
			ifsInput >> variable_name >> unit;
			int numSolveRegions = 0;  // flag specifying to solve for all regions
			int *solveRegions = NULL;
			MembraneRegionVariable* memRegionVariable = new MembraneRegionVariable(mesh->getNumMembraneRegions(), variable_name, unit);
			ODESolver* odeSolver = new ODESolver(memRegionVariable,mesh,numSolveRegions,solveRegions);
			EqnBuilder* builder = new MembraneRegionEqnBuilder(memRegionVariable,mesh,odeSolver);
			odeSolver->setEqnBuilder(builder);
			simulation->addSolver(odeSolver);
			simulation->addVariable(memRegionVariable);
		}
	}
}

Expression* FVSolver::readExpression(istream& ifsInput, string& var_name, string prefix) {	
	string expStr;
	getline(ifsInput, expStr);
	expStr = prefix + expStr;
	trimString(expStr);
	if (expStr[expStr.size()-1] != ';') {
		stringstream msg;
		msg << "Expression for [" << var_name << "] is not terminated by ';'";
		throw msg.str();
	}
	return new Expression(expStr);
}

VarContext* FVSolver::loadEquation(istream& ifsInput, Feature* feature, Variable* var) {
	string var_name = var->getName();

	//cout << "loading volume var context " << var_name << endl;	
	
	VarContext* varContext = NULL;
	if (var->getVarType() == VAR_VOLUME) {
		varContext = new VolumeVarContextExpression(feature, var_name);	
	} else if (var->getVarType() == VAR_VOLUME_REGION) {
		varContext = new VolumeRegionVarContextExpression(feature, var_name);	
	} else if (var->getVarType() == VAR_MEMBRANE) {
		varContext = new MembraneVarContextExpression(feature, var_name);	
	} else if (var->getVarType() == VAR_MEMBRANE_REGION) {
		varContext = new MembraneRegionVarContextExpression(feature, var_name);	
	} else {
		stringstream ss;
		ss << "loadEquation: variable type not supported yet: " << var_name;
		throw ss.str();
	}

	string nextToken;

	while (!ifsInput.eof()) {			
		nextToken = "";
		ifsInput >> nextToken;			
		if (nextToken.size() == 0) {
			continue;
		} else if (nextToken[0] == '#') {
			getline(ifsInput, nextToken);
			continue;
		} else if (nextToken == "EQUATION_END") {
			break;
		} else if (nextToken == "INITIAL") {
			Expression* init_exp = readExpression(ifsInput, var_name);
			varContext->setExpression(init_exp, INITIAL_VALUE_EXP);
		} else if (nextToken == "DIFFUSION") {
			Expression* diff_exp = readExpression(ifsInput, var_name);
			varContext->setExpression(diff_exp, DIFF_RATE_EXP);
		} else if (nextToken == "RATE") {
			Expression* react_exp = readExpression(ifsInput, var_name);
			varContext->setExpression(react_exp, REACT_RATE_EXP);
		} else if (nextToken == "UNIFORMRATE") {
			Expression* react_exp = readExpression(ifsInput, var_name);
			varContext->setExpression(react_exp, UNIFORM_RATE_EXP);
		} else if (nextToken == "BOUNDARY_XM") {
			Expression* boundaryexp = readExpression(ifsInput, var_name);
			varContext->setExpression(boundaryexp, BOUNDARY_XM_EXP);
		} else if (nextToken == "BOUNDARY_XP") {		
			Expression* boundaryexp = readExpression(ifsInput, var_name);
			varContext->setExpression(boundaryexp, BOUNDARY_XP_EXP);
		} else if (nextToken == "BOUNDARY_YM") {
			Expression* boundaryexp = readExpression(ifsInput, var_name);
			varContext->setExpression(boundaryexp, BOUNDARY_YM_EXP);
		} else if (nextToken == "BOUNDARY_YP") {
			Expression* boundaryexp = readExpression(ifsInput, var_name);
			varContext->setExpression(boundaryexp, BOUNDARY_YP_EXP);
		} else if (nextToken == "BOUNDARY_ZM") {
			Expression* boundaryexp = readExpression(ifsInput, var_name);
			varContext->setExpression(boundaryexp, BOUNDARY_ZM_EXP);
		} else if (nextToken == "BOUNDARY_ZP") {
			Expression* boundaryexp = readExpression(ifsInput, var_name);
			varContext->setExpression(boundaryexp, BOUNDARY_ZP_EXP);
		} else if (nextToken == "VELOCITY_X") {
			Expression* velexp = readExpression(ifsInput, var_name);
			varContext->setExpression(velexp, VELOCITY_X_EXP);
		} else if (nextToken == "VELOCITY_Y") {
			Expression* velexp = readExpression(ifsInput, var_name);
			varContext->setExpression(velexp, VELOCITY_Y_EXP);
		} else if (nextToken == "VELOCITY_Z") {
			Expression* velexp = readExpression(ifsInput, var_name);
			varContext->setExpression(velexp, VELOCITY_Z_EXP);
		} else if (nextToken == "INFLUX") {
			Expression* inexp = readExpression(ifsInput, var_name);
			varContext->setExpression(inexp, IN_FLUX_EXP);
		} else if (nextToken == "OUTFLUX") {
			Expression* outexp = readExpression(ifsInput, var_name);
			varContext->setExpression(outexp, OUT_FLUX_EXP);
		} 
	}	
	return varContext;
}

void FVSolver::loadJumpCondition(istream& ifsInput, Feature* feature, string& var_name) {
	//cout << "loading jump condition " << var_name << endl;
	VarContext* varContext = feature->getVolumeVarContext(var_name);
	string nextToken;
	Expression* inexp = null, *outexp = null;

	while (!ifsInput.eof()) {			
		nextToken = "";
		ifsInput >> nextToken;			
		if (nextToken.size() == 0) {
			continue;
		} else if (nextToken[0] == '#') {
			getline(ifsInput, nextToken);
			continue;
		} else if (nextToken == "JUMP_CONDITION_END") {
			break;
		} else if (nextToken == "INFLUX") {
			inexp = readExpression(ifsInput, var_name);
		} else if (nextToken == "OUTFLUX") {
			outexp = readExpression(ifsInput, var_name);
		} 
	}
	varContext->setExpression(inexp, IN_FLUX_EXP);
	varContext->setExpression(outexp, OUT_FLUX_EXP);
}

void FVSolver::loadPseudoConstants(istream& ifsInput, FastSystemExpression* fastSystem) {
	//cout << "loading pseudo constants for fast system" << endl;
	string nextToken;
	int count = 0;
	int numDep = fastSystem->getNumDependents();
	string* vars = new string[numDep];
	Expression **expressions = new Expression*[numDep];

	while (!ifsInput.eof()) {			
		nextToken = "";
		ifsInput >> nextToken;			
		if (nextToken.size() == 0) {
			continue;
		} else if (nextToken[0] == '#') {
			getline(ifsInput, nextToken);
			continue;
		} else if (nextToken == "PSEUDO_CONSTANT_END") {
			break;
		} else {
			vars[count] = nextToken;
			expressions[count] = readExpression(ifsInput, vars[count]);
			count ++;
		} 		 
	}
	fastSystem->setPseudoConstants(vars, expressions);
	if (count != numDep) {
		throw "In the fast system the number of pseudo constants should be the same as that of dependent variables";
	}
}

void FVSolver::loadFastRates(istream& ifsInput, FastSystemExpression* fastSystem) {
	//cout << "loading fast rates for fast system" << endl;
	string nextToken;
	int count = 0;
	int numIndep = fastSystem->getDimension();
	Expression **expressions = new Expression*[numIndep];

	while (!ifsInput.eof()) {			
		nextToken = "";
		ifsInput >> nextToken;			
		if (nextToken.size() == 0) {
			continue;
		} else if (nextToken[0] == '#') {
			getline(ifsInput, nextToken);
			continue;
		} else if (nextToken == "FAST_RATE_END") {
			break;
		} else {
			string varname("fastRate");
			expressions[count] = readExpression(ifsInput, varname, nextToken);
			count ++;
		} 		 
	}	
	if (count != numIndep) {
		throw "In the fast system the number of fast rates should be the same as that of independent variables";
	}
	fastSystem->setFastRateExpressions(expressions);
}

void FVSolver::loadFastDependencies(istream& ifsInput, FastSystemExpression* fastSystem) {
	//cout << "loading fast dependencies for fast system" << endl;
	string nextToken;
	int count = 0;
	int numDep = fastSystem->getNumDependents();
	string* vars = new string[numDep];
	Expression **expressions = new Expression*[numDep];

	while (!ifsInput.eof()) {			
		nextToken = "";
		ifsInput >> nextToken;			
		if (nextToken.size() == 0) {
			continue;
		} else if (nextToken[0] == '#') {
			getline(ifsInput, nextToken);
			continue;
		} else if (nextToken == "FAST_DEPENDENCY_END") {
			break;
		} else {
			vars[count] = nextToken;
			expressions[count] = readExpression(ifsInput, vars[count]);
			count ++;
		} 		 
	}
	fastSystem->setFastDependencyExpressions(vars, expressions);
	delete[] vars;
	if (count != numDep) {
		throw "In the fast system the number of fast dependencies should be the same as that of dependent variables";
	}
}

void FVSolver::loadJacobians(istream& ifsInput, FastSystemExpression* fastSystem) {
	//cout << "loading jacobians for fast system" << endl;
	string nextToken;
	int count = 0;
	int numIndep = fastSystem->getDimension();
	Expression **expressions = new Expression*[numIndep * numIndep];

	while (!ifsInput.eof()) {			
		nextToken = "";
		ifsInput >> nextToken;			
		if (nextToken.size() == 0) {
			continue;
		} else if (nextToken[0] == '#') {
			getline(ifsInput, nextToken);
			continue;
		} else if (nextToken == "JACOBIAN_END") {
			break;
		} else {
			string varname("jacobian");
			expressions[count] = readExpression(ifsInput, varname, nextToken);
			count ++;
		} 		 
	}	
	if (count != numIndep * numIndep) {
		throw "In the fast system the number of Jacobian should dim*dim";
	}
	fastSystem->setJacobianExpressions(expressions);
}

void FVSolver::loadFastSystem(istream& ifsInput, Feature* feature, FastSystemExpression* fastSystem) {
	//cout << "loading fast system for " << feature->getName() << endl;
	string nextToken;
	int numIndep = fastSystem->getDimension();
	int numDep = fastSystem->getNumDependents();

	while (!ifsInput.eof()) {			
		nextToken = "";
		ifsInput >> nextToken;			
		if (nextToken.size() == 0) {
			continue;
		} else if (nextToken[0] == '#') {
			getline(ifsInput, nextToken);
			continue;
		} else if (nextToken == "FAST_SYSTEM_END") {
			break;
		} else if (nextToken == "DEPENDENT_VARIALBES") {
			string* vars = new string[numDep];
			for (int i = 0; i < numDep; i ++) {
				ifsInput >> vars[i];
			}
			fastSystem->setDependentVariables(vars);
			delete[] vars;
		} else if (nextToken == "INDEPENDENT_VARIALBES") {
			string* vars = new string[numIndep];
			for (int i = 0; i < numIndep; i ++) {
				ifsInput >> vars[i];
			}
			fastSystem->setIndependentVariables(vars);
			delete[] vars;
		} else if (nextToken == "PSEUDO_CONSTANT_BEGIN") {
			loadPseudoConstants(ifsInput, fastSystem);
		} else if (nextToken == "FAST_RATE_BEGIN") {
			getline(ifsInput, nextToken);
			loadFastRates(ifsInput, fastSystem);
		} else if (nextToken == "FAST_DEPENDENCY_BEGIN") {
			getline(ifsInput, nextToken);
			loadFastDependencies(ifsInput, fastSystem);
		} else if (nextToken == "JACOBIAN_BEGIN") {
			getline(ifsInput, nextToken);
			loadJacobians(ifsInput, fastSystem);
		} 
	}	
}

void FVSolver::loadFeature(istream& ifsInput, Feature* feature) {
	//cout << "loading feature " << feature->getName() << endl;
	string nextToken;

	while (!ifsInput.eof()) {			
		nextToken = "";
		ifsInput >> nextToken;			
		if (nextToken.size() == 0) {
			continue;
		} else if (nextToken[0] == '#') {
			getline(ifsInput, nextToken);
			continue;
		} else if (nextToken == "COMPARTMENT_END") {
			break;
		} else if (nextToken == "BOUNDARY_CONDITIONS") {
			char line[1000];			
			ifsInput.getline(line, 1000);
			//ignore, processed in FEATURE, for display purpose only
		} else if (nextToken == "EQUATION_BEGIN") {
			char var_name[256];
			ifsInput >> var_name;			
			Variable* var = simulation->getVariableFromName(var_name);
			VarContext *varContext = loadEquation(ifsInput, feature, var);		
			if (var->getVarType() == VAR_VOLUME) {
				feature->addVolumeVarContext((VolumeVarContext*)varContext);	
			} else {
				feature->addVolumeRegionVarContext((VolumeRegionVarContext*)varContext);			
			}
		} else if (nextToken == "FAST_SYSTEM_BEGIN") {
			int dimension, num_of_dependents;
			ifsInput >> dimension >> num_of_dependents;
			FastSystemExpression* fastSystem = new FastSystemExpression(dimension, num_of_dependents, simulation);	
			loadFastSystem(ifsInput, feature, fastSystem);
			feature->setFastSystem(fastSystem);
		}
	}
}

void FVSolver::loadMembrane(istream& ifsInput, Feature* infeature, char* var_name) {
	//cout << "loading membrane " << var_name << endl;
	string nextToken;
	//int pdecount = 0;
	string btstr[6];
	int btcount = 0;

	while (!ifsInput.eof()) {			
		nextToken = "";
		ifsInput >> nextToken;			
		if (nextToken.size() == 0) {
			continue;
		} else if (nextToken[0] == '#') {
			getline(ifsInput, nextToken);
			continue;
		} else if (nextToken == "MEMBRANE_END") {
			break;
		} else if (nextToken == "BOUNDARY_CONDITIONS") {
			char line[1000];
			ifsInput.getline(line, 1000);
			//ignore, membrane boundary conditions depend on inside feature
			//stringstream ss(line);
			//for (int i = 0; i < 6; i ++) {
			//	ss >> btstr[i];
			//}
		} else if (nextToken == "EQUATION_BEGIN") {
			string var_name;
			ifsInput >> var_name;			
			Variable* var = simulation->getVariableFromName(var_name);
			VarContext *varContext = loadEquation(ifsInput, infeature, var);		
			if (var->getVarType() == VAR_MEMBRANE) {
				infeature->addMembraneVarContext((MembraneVarContext*)varContext);	
			} else {
				infeature->addMembraneRegionVarContext((MembraneRegionVarContext*)varContext);			
			}
			/*Solver* solver = NULL;
			for (int i = 0; i < sim->getNumSolvers(); i ++) {
				solver = sim->getSolver(i);
				if (solver->getVar() == var && solver->idPdeSolver()) {
					pdecount ++;
					break;
				}
			}*/
		} else if (nextToken == "JUMP_CONDITION_BEGIN") {
			string var_name;
			ifsInput >> var_name;
			loadJumpCondition(ifsInput, infeature, var_name);
		}
	}
	//if (pdecount > 0) {
	//	// XM
	//	if (btstr[0] == "flux" && infeature->getXmBoundaryType() != BOUNDARY_FLUX 
	//		|| btstr[0] == "value" && infeature->getXmBoundaryType() != BOUNDARY_VALUE) {
	//			throw "Membrane XM boundary type must be consistent with the boudary type of inside compartment";
	//	}

	//	//XP
	//	if (btstr[1] == "flux" && infeature->getXpBoundaryType() != BOUNDARY_FLUX 
	//		|| btstr[1] == "value" && infeature->getXpBoundaryType() != BOUNDARY_VALUE) {
	//			throw "Membrane XP boundary type must be consistent with the boudary type of inside compartment";
	//	}

	//	if (btcount > 2) {
	//		//YM
	//		if (btstr[2] == "flux" && infeature->getYmBoundaryType() != BOUNDARY_FLUX 
	//			|| btstr[2] == "value" && infeature->getYmBoundaryType() != BOUNDARY_VALUE) {
	//				throw "Membrane YM boundary type must be consistent with the boudary type of inside compartment";
	//		}
	//        
	//		//YP
	//		if (btstr[3] == "flux" && infeature->getYpBoundaryType() != BOUNDARY_FLUX 
	//			|| btstr[3] == "value" && infeature->getYpBoundaryType() != BOUNDARY_VALUE) {
	//				throw "Membrane YP boundary type must be consistent with the boudary type of inside compartment";
	//		}

	//		if (btcount > 4) {
	//			//ZM
	//			if (btstr[4] == "flux" && infeature->getZmBoundaryType() != BOUNDARY_FLUX 
	//				|| btstr[4] == "value" && infeature->getZmBoundaryType() != BOUNDARY_VALUE) {
	//					throw "Membrane ZM boundary type must be consistent with the boudary type of inside compartment";
	//			}
	//		
	//			//ZP
	//			if (btstr[5] == "flux" && infeature->getZpBoundaryType() != BOUNDARY_FLUX 
	//				|| btstr[5] == "value" && infeature->getZpBoundaryType() != BOUNDARY_VALUE) {
	//					throw "Membrane ZP boundary type must be consistent with the boudary type of inside compartment";
	//			}
	//		}
	//	}
		//delete[] btstr;
	//}
}

void FVSolver::loadSimulationParameters(istream& ifsInput) {
	string basefilename;
	double end_time, time_step;
	long keep_every;
	int bStoreEnable=1;
	string nextToken;	

	while (!ifsInput.eof()) {			
		nextToken = "";
		ifsInput >> nextToken;			
		if (nextToken.size() == 0) {
			continue;
		} else if (nextToken[0] == '#') {
			getline(ifsInput, nextToken);
			continue;
		} else if (nextToken == "SIMULATION_PARAM_END") {
			break;
		} else if (nextToken == "BASE_FILE_NAME") {
			getline(ifsInput, basefilename);
			trimString(basefilename);
		} else if (nextToken == "ENDING_TIME") {
			ifsInput >> end_time;
		} else if (nextToken == "TIME_STEP") {
			ifsInput >> time_step;
		} else if (nextToken == "KEEP_EVERY") {
			ifsInput >> keep_every;
		} else if (nextToken == "STORE_ENABLE") {
			ifsInput >> bStoreEnable;
		} else {
			stringstream ss;
			ss << "loadSimulationParameters(), encountered unknown token " << nextToken << endl;
			throw ss.str();
		}
	}

	if (outputPath == 0) {
		SimTool::getInstance()->setBaseFilename((char*)basefilename.c_str());
	} else {
		const char* baseSimName = strrchr(basefilename.c_str(), DIRECTORY_SEPARATOR);
		if (baseSimName == NULL) {
			baseSimName = basefilename.c_str();
		} else {
			baseSimName += 1;
		}
		char newBaseName[512];
		sprintf(newBaseName, "%s%c%s", outputPath, DIRECTORY_SEPARATOR, baseSimName);
		SimTool::getInstance()->setBaseFilename(newBaseName);
	}

	simTool->setTimeStep(time_step);
	simTool->setEndTimeSec(end_time);
	simTool->setKeepEvery(keep_every);
	simTool->setStoreEnable(bStoreEnable!=0);
	//SimTool::getInstance()->setFileCompress(false);
}

void FVSolver::loadMesh(istream& ifsInput) {
	if (SimTool::getInstance()->getModel() == 0) {
		throw "Model has to be initialized before mesh initialization";
	}

	string meshfile = "";
	string nextToken;
	string vcgText = "";

	while (!ifsInput.eof()) {			
		nextToken = "";
		ifsInput >> nextToken;			
		if (nextToken.size() == 0) {
			continue;
		} else if (nextToken[0] == '#') {
			getline(ifsInput, nextToken);
			continue;
		} else if (nextToken == "MESH_END") {
			break;
		} else if (nextToken == "VCG_FILE") {
			getline(ifsInput, meshfile);
			trimString(meshfile);
			struct stat buf;
			if (stat(meshfile.c_str(), &buf)) {
				stringstream ss;
				ss << "Mesh file(.vcg) [" << meshfile <<"] doesn't exist";
				throw ss.str();
			}
		} else { // VCG In file
			vcgText += nextToken;
			while (true) {
				getline(ifsInput, nextToken);
				if (nextToken.find("MESH_END") != string::npos) {
					break;
				}
				vcgText += nextToken + "\n";
			}
			break;
		}
	}

	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_STARTING, "initializing mesh"));
	mesh = new CartesianMesh();
	if (meshfile.size() != 0) {
		ifstream ifs(meshfile.c_str());
		if (!ifs.is_open()){
			stringstream ss;
			ss << "Can't open geometry file '" <<  meshfile << "'";
			throw ss.str();
		}
		cout << "Reading mesh from vcg file '" << meshfile << "'" << endl;
		mesh->initialize(ifs);
	} else {
		if (vcgText.size() == 0) {
			throw "no mesh specified";
		}		
		cout << "Reading mesh from text..." << endl;
		//cout << vcgText << endl;
		istringstream iss(vcgText);
		mesh->initialize(iss);
	}
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_STARTING, "mesh initialized"));
}

void FVSolver::loadFieldData(istream& ifsInput) {
	if (simulation == 0) {
		throw "Simulation has to be initialized before loading field data";
	}

	string nextToken;
	int fdIndex;
	string fdVarType, fdID, fdName, fdVarName, fdFile;
	double fdTime;

	while (!ifsInput.eof()) {			
		nextToken = "";
		ifsInput >> nextToken;			
		if (nextToken.size() == 0) {
			continue;
		} else if (nextToken[0] == '#') {
			getline(ifsInput, nextToken);
			continue;
		} else if (nextToken == "FIELD_DATA_END") {
			break;
		} else {
			fdIndex = -1;
			stringstream ss(nextToken);
			ss >> fdIndex;
			fdTime = -1;
			ifsInput >> fdVarType >> fdID >> fdName >> fdVarName >> fdTime;
			getline(ifsInput,  fdFile);
			trimString(fdFile);
			if (fdVarType == "" || fdID == "" || fdName == "" || fdVarName == "" || fdFile == "" || fdIndex < 0 || fdTime < 0) {
				throw "loadFieldData(), wrong input";
			}
			VariableType varType = VAR_UNKNOWN;
			if (fdVarType == "Membrane") {
				varType = VAR_MEMBRANE;
			} else if (fdVarType == "Volume") {
				varType = VAR_VOLUME;
			} else {
				throw "field data is only supported for volume and membrane variables";
			}
			simulation->addFieldData(new FieldData(fdIndex, varType, fdID, fdName, fdVarName, fdTime, fdFile));
		}
	}
}

void FVSolver::loadParameters(istream& ifsInput) {
	if (simulation == 0) {
		throw "Simulation has to be initialized before loading field data";
	}

	int numParameters;
	string nextToken;

	int nread = 0;

	ifsInput >> numParameters;
	while (!ifsInput.eof()) {		
		ifsInput >> nextToken;
		if (nextToken.size() == 0) {
			continue;
		} else if (nextToken[0] == '#') {
			getline(ifsInput, nextToken);
			continue;
		} else if (nextToken == "PARAMETER_END") {
			break;
		} else {
			nread ++;
			simulation->addParameter(nextToken);
		}
	}
	assert(nread == numParameters);
}

void FVSolver::createSimTool(istream& ifsInput, int taskID)
{	
	SimTool::create();
	simTool = SimTool::getInstance();

	if (taskID < 0) { // no messaging
		SimulationMessaging::create();
	}
	string meshfile;
	
	string nextToken;	

	while (!ifsInput.eof()) {			
		nextToken = "";
		ifsInput >> nextToken;	
		if (nextToken.size() == 0) {
			continue;
		} else if (nextToken[0] == '#') {
			getline(ifsInput, nextToken);
			continue;
		} else if (nextToken == "JMS_PARAM_BEGIN") {
			loadJMSInfo(ifsInput, taskID);
#ifdef USE_MESSAGING
			SimulationMessaging::getInstVar()->start(); // start the thread
#endif
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_STARTING, "preprocessing started"));
		} else if (nextToken == "SIMULATION_PARAM_BEGIN") {
			loadSimulationParameters(ifsInput);
		} else if (nextToken == "MODEL_BEGIN") {
			loadModel(ifsInput);
			if (model == null) {
				throw "Model has 0 features";
			}
			simTool->setModel(model);
		} else if (nextToken == "MESH_BEGIN") {
			loadMesh(ifsInput);
		} else if (nextToken == "VARIABLE_BEGIN") {
			loadSimulation(ifsInput);
			simTool->setSimulation(simulation);
		} else if (nextToken == "PARAMETER_BEGIN") {
			loadParameters(ifsInput);
		} else if (nextToken == "FIELD_DATA_BEGIN") {
			loadFieldData(ifsInput);
		} else if (nextToken == "COMPARTMENT_BEGIN") {
			string feature_name;				
			ifsInput >> feature_name;
			Feature* feature = model->getFeatureFromName(feature_name);
			if (feature != null) {
				loadFeature(ifsInput, feature);
			} else {
				throw "createSimTool(), Invalid compartment when loading feature!";
			}				
		} else if (nextToken == "MEMBRANE_BEGIN") {
			char var_name[256];
			string feature1_name, feature2_name;				
			ifsInput >> var_name >> feature1_name >> feature2_name;
			Feature * infeature = model->getFeatureFromName(feature1_name);
			Feature* outfeature = model->getFeatureFromName(feature2_name);
			if (infeature != null && outfeature != null) {
				if (infeature->getPriority() > outfeature->getPriority()) {
					loadMembrane(ifsInput, infeature, var_name);		
				} else {
					loadMembrane(ifsInput, outfeature, var_name);		
				}
			} else {
				throw "createSimTool(), Invalid compartment when loading membrane!";
			}				
		} else {
			stringstream ss;
			ss << "createSimTool(), encountered unknown token " << nextToken << endl;
			throw ss.str();
		}
	}
}

FVSolver::FVSolver(istream& fvinput, int taskID, char* outdir, bool bSimZip) {
	simTool = 0;
	simulation = 0;
	model = 0;
	mesh = 0;
	outputPath = outdir;
	createSimTool(fvinput, taskID);	
	if (!bSimZip) {
		SimTool::getInstance()->requestNoZip();
	}
}

void FVSolver::solve(bool bLoadFinal, double* paramValues)
{
	if (paramValues != 0) {
		simulation->setParameterValues(paramValues);
	}
	simulation->initSimulation();
	if (bLoadFinal) {
		simTool->loadFinal();   // initializes to the latest file if it exists
	}

	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_STARTING, "preprocessing finished"));

	simTool->start();
#ifdef USE_MESSAGING
	if (!SimTool::getInstance()->isStopped()) {
		SimulationMessaging::getInstVar()->waitUntilFinished();
	}
#endif
}

void FVSolver::init(double* paramValues){
	// setting initial conditions.
	simulation->setParameterValues(paramValues);
	simulation->initSimulation();
	simulation->reset();
}

void FVSolver::step(double* paramValues)
{
	//simulation->setParameterValues(paramValues);
	simulation->iterate();
	simulation->update();
#ifdef USE_MESSAGING
	if (!SimTool::getInstance()->isStopped()) {
		SimulationMessaging::getInstVar()->waitUntilFinished();
	}
#endif
}

string FVSolver::getVariableName(int index){	
	return simulation->getVariable(index)->getName();
}

int FVSolver::getNumVariables(){	
	return simulation->getNumVariables();
}

double* FVSolver::getValue(string& var, int arrayID) {
	if (arrayID==0){
		return simulation->getVariableFromName(var)->getOld();
	} else if (arrayID==1){
		return simulation->getVariableFromName(var)->getCurr();
	} else {
		throw "arrayID out of bounds";
	}
}

int FVSolver::getVariableLength(string& var) {
	return simulation->getVariableFromName(var)->getSize();
}

void FVSolver::setInitialCondition(string& varName, int dataLength, const double* data) {
	Variable* var = simulation->getVariableFromName(varName);
	if (var == 0) {
		char errMsg[512];
		sprintf(errMsg, "FVSolver::setInitialCondition() : variable %s doesn't exist", varName);
		throw errMsg;
	}
	if (var->getSize() != dataLength) {
		char errMsg[512];
		sprintf(errMsg, "FVSolver::setInitialCondition() : variable %s doesn't match in size, %d, %d", varName, var->getSize(), dataLength);
		throw errMsg;
	}
	memcpy(var->getCurr(), data, dataLength * sizeof(double));
	var->update();
}

double FVSolver::getCurrentTime(){
	return simulation->getTime_sec();
}

void FVSolver::setEndTime(double endTime){
	simTool->setEndTimeSec(endTime);
}
