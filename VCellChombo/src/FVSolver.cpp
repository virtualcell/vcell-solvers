#include <VCELL/ChomboIF.h>
#include <Exception.h>
#include <Expression.h>

#include <sys/stat.h>
#include <VCELL/FVSolver.h>
#include <VCELL/VolumeVariable.h>
#include <VCELL/VolumeRegionVariable.h>
#include <VCELL/MembraneVariable.h>
#include <VCELL/MembraneRegionVariable.h>
#include <VCELL/VCellModel.h>
#include <VCELL/Feature.h>
#include <VCELL/Membrane.h>
#include <VCELL/SimTool.h>
#include <VCELL/VolumeVarContextExpression.h>
#include <VCELL/VolumeRegionVarContextExpression.h>
#include <VCELL/MembraneVarContextExpression.h>
#include <VCELL/MembraneRegionVarContextExpression.h>
#include <VCELL/SimulationMessaging.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/ChomboSemiImplicitScheduler.h>
#include <VCELL/ChomboSpec.h>

#include <assert.h>
#include <sstream>
#include <fstream>
#include <string>
using std::ifstream;

/*
# JMS_Paramters
JMS_PARAM_BEGIN
JMS_BROKER tcp://code:2507
JMS_USER serverUser cbittech
JMS_QUEUE workerEventDev
JMS_TOPIC serviceControlDev
VCELL_USER fgao
SIMULATION_KEY 36230826
JOB_INDEX 0
JMS_PARAM_END
*/
void FVSolver::loadJMSInfo(istream& ifsInput, int taskID) {
	char *broker = new char[256];
	char *smqusername = new char[256];
	char *password = new char[256];
	char *qname = new char[256];
	char *tname = new char[256];
	char *vcusername = new char[256];
	string nextToken, line;
	int simKey, jobIndex;

	while (!ifsInput.eof()) {
		getline(ifsInput, line);
		istringstream lineInput(line);

		nextToken = "";
		lineInput >> nextToken;
		if (nextToken.size() == 0 || nextToken[0] == '#') {
			continue;
		}
		if (nextToken == "JMS_PARAM_END") {
			break;
		}

		if (nextToken == "JMS_BROKER") {
			memset(broker, 0, 256 * sizeof(char));
			lineInput >> broker;
		} else if (nextToken == "JMS_USER") {
			memset(smqusername, 0, 256 * sizeof(char));
			memset(password, 0, 256 * sizeof(char));
			lineInput >> smqusername >> password;
		} else if (nextToken == "JMS_QUEUE") {
			memset(qname, 0, 256 * sizeof(char));
			lineInput >> qname;
		} else if (nextToken == "JMS_TOPIC") {
			memset(tname, 0, 256 * sizeof(char));
			lineInput >> tname;
		} else if (nextToken == "VCELL_USER") {
			memset(vcusername, 0, 256 * sizeof(char));
			lineInput >> vcusername;
		} else if (nextToken == "SIMULATION_KEY") {
			lineInput >> simKey;
		} else if (nextToken == "JOB_INDEX") {
			lineInput >> jobIndex;
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

/*
# Model description: FEATURE name handle boundary_conditions
MODEL_BEGIN
FEATURE cyt 0 value value value value 
FEATURE ec 1 value value value value 
MEMBRANE cyt_ec_membrane cyt ec value value value value 
MODEL_END
*/
void FVSolver::loadModel(istream& ifsInput) {
	//cout << "loading model " << endl;
	model = new VCellModel();
	string nextToken, line;
	string feature_name;
	int handle;
	int numFeatures = 0, numMembranes = 0;

	while (!ifsInput.eof()) {
		getline(ifsInput, line);
		istringstream lineInput(line);

		nextToken = "";
		lineInput >> nextToken;
		if (nextToken.size() == 0 || nextToken[0] == '#') {
			continue;
		} 
		if (nextToken == "MODEL_END") {
			break;
		} 
		
		Structure* structure = 0;
		if (nextToken == "FEATURE") {
			numFeatures ++;
			lineInput >> feature_name >> handle;
			Feature* feature = model->addFeature(feature_name);
			structure = feature;
		} else if (nextToken == "MEMBRANE") {
			numMembranes ++;
			string membrane_name, feature1_name, feature2_name;
			lineInput >> membrane_name >> feature1_name >> feature2_name;
			Membrane* membrane = model->addMembrane(membrane_name, feature1_name, feature2_name);
			structure = membrane;
		}

		string btstr;
		for (int i = 0; i < 6; i ++) {
			if (lineInput.eof()) {
				break;
			}
			lineInput >> btstr;
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
				ss1 << "loadModel(), wrong boundary type " << btstr;
				throw ss1.str();
			}
			switch (i) {
				case 0: // XM
					structure->setXmBoundaryType(bt);
					break;
				case 1: // XP
					structure->setXpBoundaryType(bt);
					break;
				case 2: // YM
					structure->setYmBoundaryType(bt);
					break;
				case 3: // YP
					structure->setYpBoundaryType(bt);
					break;
				case 4: // ZM
					structure->setZmBoundaryType(bt);
					break;
				case 5: // ZP
					structure->setZpBoundaryType(bt);
					break;
			}
		}
	}
}

static void trimString(string& str)
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

/*
# Variables : type name time_dependent_flag advection_flag solve_whole_mesh_flag solve_regions
VARIABLE_BEGIN
MEMBRANE_ODE h_c
VOLUME_PDE K false false true
MEMBRANE_ODE m_o
MEMBRANE_ODE n_o
VOLUME_PDE Na false false true
MEMBRANE_REGION Voltage_membrane
VARIABLE_END
*/
void FVSolver::loadSimulation(istream& ifsInput) {
	//cout << "loading simulation" << endl;
	simulation = new SimulationExpression();
	// process geometry
	chomboScheduler = new ChomboSemiImplicitScheduler(simulation, chomboSpec);
	chomboScheduler->initializeGrids();
	simulation->setScheduler(chomboScheduler);
	
	int numMembranePoints = chomboScheduler->getNumMembranePoints();
	string nextToken, line;
	IntVect viewLevelMeshSize = chomboScheduler->getViewLevelMeshSize();
	long sizeX = viewLevelMeshSize[0];
	long sizeY = viewLevelMeshSize[1];
	long sizeZ = chomboScheduler->getChomboGeometry()->getDimension() == 3 ? viewLevelMeshSize[2] : 1;
	string variable_name, variable_domain;

	while (!ifsInput.eof()) {
		getline(ifsInput, line);
		istringstream lineInput(line);

		nextToken = "";
		lineInput >> nextToken;
		if (nextToken.size() == 0 || nextToken[0] == '#') {
			continue;
		}
		if (nextToken == "VARIABLE_END") {
			break;
		}

		if (nextToken == "VOLUME_RANDOM")
		{
			throw "VolumeRandomVariable not supported yet";
		} 
		else if (nextToken == "MEMBRANE_RANDOM")
		{
			throw "MembraneRandomVariable not supported yet";
		}
		else if (nextToken == "VOLUME_PDE" || nextToken == "VOLUME_PDE_STEADY")
		{
			string advectionflag, time_dependent_diffusion_flag, grad_flag;
			lineInput >> variable_name >> variable_domain >> time_dependent_diffusion_flag >> advectionflag >> grad_flag;

			Feature* feature = model->getFeatureFromName(variable_domain);
			if (feature == NULL) {
				stringstream ss;
				ss << "volume variable " << variable_name << " is not defined in any feature.";
				throw ss.str();
			}
			if (time_dependent_diffusion_flag == "true") {
				throw "time dependent diffusion not supported yet";
				simulation->setHasTimeDependentDiffusionAdvection();
			}
			bool bConvection = false;
			if (advectionflag == "true" ) {
				throw "advection not supported yet";
				bConvection = true;
			}
			if (grad_flag == "true")
			{
				throw "gradient not supported yet";
			}
			VolumeVariable* volumeVar = new VolumeVariable(variable_name, feature, sizeX*sizeY*sizeZ);
			volumeVar->setDiffusing();
			if (nextToken == "VOLUME_PDE_STEADY")
			{
				volumeVar->setElliptic();
				simulation->setHasElliptic();
			}
			else
			{
				simulation->setHasParabolic();
			}
			feature->addDefinedVariable(volumeVar);
			simulation->addVariable(volumeVar);
		} else if (nextToken == "VOLUME_ODE") {
			lineInput >> variable_name >> variable_domain;
			Feature* feature = model->getFeatureFromName(variable_domain);
			if (feature == NULL) {
				stringstream ss;
				ss << "volume variable " << variable_name << " is not defined in any feature.";
				throw ss.str();
			}
			VolumeVariable* volumeVar = new VolumeVariable(variable_name, feature, sizeX*sizeY*sizeZ);
			feature->addDefinedVariable(volumeVar);
			simulation->addVariable(volumeVar);
		} 
		else if (nextToken == "MEMBRANE_ODE")
		{
			lineInput >> variable_name >> variable_domain;
			Membrane* membrane = model->getMembraneFromName(variable_domain);
			MembraneVariable* membraneVar = new MembraneVariable(variable_name, membrane, numMembranePoints);
			if (membrane == NULL) {
				stringstream ss;
				ss << "membrane variable " << variable_name << " is not defined on any membrane.";
				throw ss.str();
			}
			membrane->addDefinedVariable(membraneVar);
			simulation->addVariable(membraneVar);
		} else if (nextToken == "MEMBRANE_PDE") {
			throw "Membrane diffusion not supported yet";
		} else if (nextToken == "VOLUME_REGION") {
			throw "VolumeRegionVariable not supported yet";
		} else if (nextToken == "MEMBRANE_REGION") {
			throw "MembraneRegionVariable not supported yet";
		}
	}
}

VCell::Expression* FVSolver::readExpression(istream& lineInput, string& var_name, string prefix) {
	string expStr;
	getline(lineInput, expStr);
	expStr = prefix + expStr;
	trimString(expStr);
	if (expStr[expStr.size()-1] != ';') {
		stringstream msg;
		msg << "Expression for [" << var_name << "] is not terminated by ';'";
		throw msg.str();
	}
	return new VCell::Expression(expStr);
}

/*
EQUATION_BEGIN U
INITIAL (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
RATE 0.0;
DIFFUSION 1.0;
VELOCITY_X 0.0;
VELOCITY_Y 0.0;
BOUNDARY_XM (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
BOUNDARY_XP (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
BOUNDARY_YM (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
BOUNDARY_YP (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
EQUATION_END
*/
void FVSolver::loadVarContext(istream& ifsInput, Structure* structure, Variable* var) {

	//cout << "loading volume var context " << var_name << endl;
	if (var->getStructure() != structure)
	{
		// now Variable only has one VarContext that makes sense. Others are dummy.
		return;
	}

	VarContext* varContext = NULL;
	if (var->getVarType() == VAR_VOLUME) {
		varContext = new VolumeVarContextExpression((Feature*)structure, (VolumeVariable*)var);
	} else if (var->getVarType() == VAR_VOLUME_REGION) {
		varContext = new VolumeRegionVarContextExpression((Feature*)structure, (VolumeRegionVariable*)var);
	} else if (var->getVarType() == VAR_MEMBRANE) {
		varContext = new MembraneVarContextExpression((Membrane*)structure, (MembraneVariable*)var);
	} else if (var->getVarType() == VAR_MEMBRANE_REGION) {
		varContext = new MembraneRegionVarContextExpression((Membrane*)structure, (MembraneRegionVariable*)var);
	} else {
		stringstream ss;
		ss << "loadEquation: variable type '" << var->getVarType() << "' is not supported yet for variable '" << var->getName() << "'";
		throw ss.str();
	}
	var->setVarContext(varContext);

	string nextToken, line;
	while (!ifsInput.eof()) {
		getline(ifsInput, line);
		istringstream lineInput(line);

		nextToken = "";
		lineInput >> nextToken;
		if (nextToken.size() == 0 || nextToken[0] == '#') {
			continue;
		}
		if (nextToken == "EQUATION_END") {
			break;
		}

		string var_name = var->getName();
		VCell::Expression* exp = readExpression(lineInput, var_name);
		ExpressionIndex expIndex;
		if (nextToken == "INITIAL") {
			expIndex = INITIAL_VALUE_EXP;
		} else if (nextToken == "EXACT") {
			expIndex = EXACT_EXP;
		} else if (nextToken == "DIFFUSION") {
			expIndex = DIFF_RATE_EXP;
		} else if (nextToken == "RATE") {
			expIndex = REACT_RATE_EXP;
		} else if (nextToken == "UNIFORMRATE") {
			expIndex = UNIFORM_RATE_EXP;
		} else if (nextToken == "BOUNDARY_XM") {
			expIndex = BOUNDARY_XM_EXP;
		} else if (nextToken == "BOUNDARY_XP") {
			expIndex = BOUNDARY_XP_EXP;
		} else if (nextToken == "BOUNDARY_YM") {
			expIndex = BOUNDARY_YM_EXP;
		} else if (nextToken == "BOUNDARY_YP") {
			expIndex = BOUNDARY_YP_EXP;
		} else if (nextToken == "BOUNDARY_ZM") {
			expIndex = BOUNDARY_ZM_EXP;
		} else if (nextToken == "BOUNDARY_ZP") {
			expIndex = BOUNDARY_ZP_EXP;
		} else if (nextToken == "VELOCITY_X") {
			expIndex = VELOCITY_X_EXP;
		} else if (nextToken == "VELOCITY_Y") {
			expIndex = VELOCITY_Y_EXP;
		} else if (nextToken == "VELOCITY_Z") {
			expIndex = VELOCITY_Z_EXP;
		} else {
			stringstream ss;
			ss << "FVSolver::loadEquation(), unexpected token " << nextToken;
			throw ss.str();
		}
		varContext->setExpression(exp, expIndex);
	}
}

/*
JUMP_CONDITION_BEGIN Ca
FLUX Nucleus (150.0 * (Ca_Cytosol_membrane - Ca_Nucleus_membrane));
FLUX Cytosol  - (150.0 * (Ca_Cytosol_membrane - Ca_Nucleus_membrane));
JUMP_CONDITION_END
*/
void FVSolver::loadJumpCondition(istream& ifsInput, Membrane* membrane, string& var_name) {
	//cout << "loading jump condition " << var_name << endl;
	string nextToken, line;

	Variable* var = simulation->getVariableFromName(var_name);	
	while (!ifsInput.eof()) {
		getline(ifsInput, line);
		istringstream lineInput(line);

		nextToken = "";
		lineInput >> nextToken;
		if (nextToken.size() == 0 || nextToken[0] == '#') {
			continue;
		}  
		if (nextToken == "JUMP_CONDITION_END") {
			break;
		}
		if (nextToken == "FLUX" || nextToken == "VALUE") {
			string featurename;
			lineInput >> featurename;
			assert(!featurename.empty());
			Feature* f = model->getFeatureFromName(featurename);
			f->setEbBcType(membrane, nextToken == "FLUX" ? BOUNDARY_FLUX : BOUNDARY_VALUE);
			string var_name = var->getName();
			VCell::Expression* exp = readExpression(lineInput, var_name);
			VarContext* varContext = var->getVarContext();
			if (var->getVarType() == VAR_VOLUME || var->getVarType() == VAR_VOLUME_REGION)
			{
				varContext->addJumpCondition(membrane, exp);
			} 
			else
			{
				throw "Only volume variables and volume region variables have jump conditions";
			}
		} 
		else
		{
			throw "Expecting FLUX or VALUE in JumpCondition.";
		}
	}
}

/*
COMPARTMENT_BEGIN cyt

BOUNDARY_CONDITIONS value value value value 

EQUATION_BEGIN U
INITIAL (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
RATE 0.0;
DIFFUSION 1.0;
VELOCITY_X 0.0;
VELOCITY_Y 0.0;
BOUNDARY_XM (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
BOUNDARY_XP (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
BOUNDARY_YM (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
BOUNDARY_YP (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
EQUATION_END

COMPARTMENT_END

COMPARTMENT_BEGIN ec

BOUNDARY_CONDITIONS value value value value 

EQUATION_BEGIN U
INITIAL (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
RATE 0.0;
DIFFUSION 0.0;
VELOCITY_X 0.0;
VELOCITY_Y 0.0;
BOUNDARY_XM (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
BOUNDARY_XP (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
BOUNDARY_YM (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
BOUNDARY_YP (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
EQUATION_END

COMPARTMENT_END
*/
void FVSolver::loadFeature(istream& ifsInput, Feature* feature) {
	//cout << "loading feature " << feature->getName() << endl;
	string nextToken, line;

	while (!ifsInput.eof()) {
		getline(ifsInput, line);
		istringstream lineInput(line);

		nextToken = "";
		lineInput >> nextToken;
		if (nextToken.size() == 0 || nextToken[0] == '#') {
			continue;
		}
		if (nextToken == "COMPARTMENT_END") {
			break;
		}

		if (nextToken == "EQUATION_BEGIN") {
			char var_name[256];
			lineInput >> var_name;
			Variable* var = simulation->getVariableFromName(var_name);
			if (var->getStructure() == feature)
			{
				loadVarContext(ifsInput, feature, var);
			}
		} else if (nextToken == "FAST_SYSTEM_BEGIN") {
			throw "Fast system not supported";
		}
	}
}

/*
MEMBRANE_BEGIN subVolume0_subVolume1_membrane subVolume0 subVolume1

BOUNDARY_CONDITIONS flux value flux value 

EQUATION_BEGIN n_o
INITIAL 0.304015731;
RATE (1000.0 * ((0.01 * (1.0 - n_o) * (10.0 - (62.0 + Voltage_membrane)) / (-1.0 + exp((0.1 * (10.0 - (62.0 + Voltage_membrane)))))) - (0.125 * n_o * exp( - (0.0125 * (62.0 + Voltage_membrane))))));
EQUATION_END

EQUATION_BEGIN m_o
INITIAL 0.04759071;
RATE (1000.0 * ((0.1 * (1.0 - m_o) * (25.0 - (62.0 + Voltage_membrane)) / (-1.0 + exp((0.1 * (25.0 - (62.0 + Voltage_membrane)))))) - (4.0 * m_o * exp( - (0.05555555555555555 * (62.0 + Voltage_membrane))))));
EQUATION_END

EQUATION_BEGIN h_c
INITIAL 0.372877409;
RATE  - (1000.0 * ((0.07 * h_c * exp( - (0.05 * (62.0 + Voltage_membrane)))) - ((1.0 - h_c) / (1.0 + exp((0.1 * (30.0 - (62.0 + Voltage_membrane))))))));
EQUATION_END

EQUATION_BEGIN Voltage_membrane
INITIAL -62.897633102;
RATE (100000.0 * ((0.1 * (t < 0.05)) - ((0.0030 * (51.4 + Voltage_membrane)) - (1.2 * ((25.851990049751244 * log((Na_subVolume1_membrane / Na_subVolume0_membrane))) - Voltage_membrane) * pow(m_o,3.0) * (1.0 - h_c)) - (0.36 * ((25.851990049751244 * log((K_subVolume1_membrane / K_subVolume0_membrane))) - Voltage_membrane) * pow(n_o,4.0)))));
UNIFORMRATE 0.0;
EQUATION_END

JUMP_CONDITION_BEGIN K
FLUX subVolume0 (3731.3432835820895 * ((25.851990049751244 * log((K_subVolume1_membrane / K_subVolume0_membrane))) - Voltage_membrane) * pow(n_o,4.0));
FLUX subVolume1  - (3731.3432835820895 * ((25.851990049751244 * log((K_subVolume1_membrane / K_subVolume0_membrane))) - Voltage_membrane) * pow(n_o,4.0));
JUMP_CONDITION_END

JUMP_CONDITION_BEGIN Na
FLUX subVolume0 (12437.810945273632 * ((25.851990049751244 * log((Na_subVolume1_membrane / Na_subVolume0_membrane))) - Voltage_membrane) * pow(m_o,3.0) * (1.0 - h_c));
FLUX subVolume1  - (12437.810945273632 * ((25.851990049751244 * log((Na_subVolume1_membrane / Na_subVolume0_membrane))) - Voltage_membrane) * pow(m_o,3.0) * (1.0 - h_c));
JUMP_CONDITION_END

MEMBRANE_END
*/
void FVSolver::loadMembrane(istream& ifsInput, Membrane* membrane) {
	//cout << "loading membrane " << var_name << endl;
	string nextToken, line;

	while (!ifsInput.eof()) {
		getline(ifsInput, line);
		istringstream lineInput(line);

		nextToken = "";
		lineInput >> nextToken;
		if (nextToken.size() == 0 || nextToken[0] == '#') {
			continue;
		}
		if (nextToken == "MEMBRANE_END") {
			break;
		}
		
		if (nextToken == "EQUATION_BEGIN") {
			string var_name;
			lineInput >> var_name;
			Variable* var = simulation->getVariableFromName(var_name);
			loadVarContext(ifsInput, membrane, var);
		} else if (nextToken == "FAST_SYSTEM_BEGIN") {
			throw "Fast system not supported";
		} else if (nextToken == "JUMP_CONDITION_BEGIN") {
			string var_name;
			lineInput >> var_name;
			loadJumpCondition(ifsInput, membrane, var_name);
		}
	}
}

/*
# Simulation Parameters
SIMULATION_PARAM_BEGIN
SOLVER SUNDIALS_PDE_SOLVER 1.0E-7 1.0E-9 1.0
BASE_FILE_NAME \\cfs01.vcell.uchc.edu\raid\Vcell\users\fgao\SimID_36269803_0_
ENDING_TIME 0.1
TIME_STEP 0.1
KEEP_EVERY 1
SIMULATION_PARAM_END
--------OR--------------
# Simulation Parameters
SIMULATION_PARAM_BEGIN
SOLVER FV_SOLVER 1.0E-8
BASE_FILE_NAME \\cfs01.vcell.uchc.edu\raid\Vcell\users\fgao\SimID_36230826_0_
ENDING_TIME 1.0
TIME_STEP 0.01
KEEP_EVERY 10
SIMULATION_PARAM_END
*/
void FVSolver::loadSimulationParameters(istream& ifsInput) {
	string nextToken, line;

	while (!ifsInput.eof()) {
		getline(ifsInput, line);
		istringstream lineInput(line);

		nextToken = "";
		lineInput >> nextToken;
		if (nextToken.size() == 0 || nextToken[0] == '#') {
			continue;
		} 
		if (nextToken == "SIMULATION_PARAM_END") {
			break;
		}
		
		if (nextToken == "SOLVER") {
			string solver="";
			lineInput >> solver;
			simTool->setSolver(solver);
			if (solver == CHOMBO_SEMIIMPLICIT_SOLVER)
			{
			}
//			else if (solver == CHOMBO_SUNDIALS_SOLVER) {
//				double sundialsRelTol = 1e-7;
//				double sundialsAbsTol = 1e-9;
//				double maxStep = 0.1;
//				lineInput >> sundialsRelTol >> sundialsAbsTol >> maxStep;
//				simTool->setSundialsErrorTolerances(sundialsRelTol, sundialsAbsTol);
//				simTool->setSundialsMaxStep(maxStep);
//			}
		} else if (nextToken == "DISCONTINUITY_TIMES") {
			throw "discontinuity times not supported.";
//			int numDisTimes = 0;
//			lineInput >> numDisTimes;
//			if (numDisTimes > 0) {
//				double* discontinuityTimes = 0;
//				discontinuityTimes = new double[numDisTimes];
//				for (int i = 0; i < numDisTimes; i ++) {
//					lineInput >> discontinuityTimes[i];
//				}
//				simTool->setDiscontinuityTimes(numDisTimes, discontinuityTimes);
//			}
		} else if (nextToken == "BASE_FILE_NAME") {
			string basefilename;
			getline(lineInput, basefilename);
			trimString(basefilename);
			simTool->setBaseFilename((char*)basefilename.c_str());
		} else if (nextToken == "ENDING_TIME") {
			double end_time;
			lineInput >> end_time;
			simTool->setEndTimeSec(end_time);
		} else if (nextToken == "TIME_STEP") {
			double time_step;
			lineInput >> time_step;
			simTool->setTimeStep(time_step);
		} else if (nextToken == "CHECK_SPATIALLY_UNIFORM") {
			throw "check spatially uniform not supported.";
		} else if (nextToken == "KEEP_EVERY") {
			int keep_every = 1;
			string one_step, keep_every_str;
			lineInput >> one_step >> keep_every_str;
			if (one_step == "ONE_STEP") {
				throw "keep every one step not supported";
//				simTool->setSundialsOneStepOutput();
			} else {
				keep_every_str = one_step;
			}
			keep_every = atoi(keep_every_str.c_str());

			simTool->setKeepEvery(keep_every);
		} else if (nextToken == "KEEP_AT_MOST") {
			throw "keep at most not supported.";
//			int keep_at_most;
//			lineInput >> keep_at_most;
//			simTool->setKeepAtMost(keep_at_most);
		} else if (nextToken == "STORE_ENABLE") {
			int bStoreEnable=1;
			lineInput >> bStoreEnable;
			simTool->setStoreEnable(bStoreEnable!=0);
		} else {
			stringstream ss;
			ss << "loadSimulationParameters(), encountered unknown token " << nextToken << endl;
			throw ss.str();
		}
	}
}

/*
# Field Data
FIELD_DATA_BEGIN
#id, name, varname, time filename
0 _VCell_FieldData_0 FRAP_binding_ALPHA rfB 0.1 \\\\SAN2\\raid\\Vcell\\users\\fgao\\SimID_22489731_0_FRAP_binding_ALPHA_rfB_0_1.fdat
FIELD_DATA_END
*/
void FVSolver::loadFieldData(istream& ifsInput) {
	if (simulation == 0) {
		throw "Simulation has to be initialized before loading field data";
	}
	throw "field data not supported";	
}

/*
# Parameters
PARAMETER_BEGIN 3
D
U0
U1
PARAMETER_END
*/
void FVSolver::loadParameters(istream& ifsInput, int numParameters) {
	if (simulation == 0) {
		throw "Simulation has to be initialized before loading field data";
	}
	throw "parameters not supported";
}

void FVSolver::loadSerialScanParameters(istream& ifsInput, int numSerialScanParameters) {
	if (simulation == 0) {
		throw "Simulation has to be initialized before loading serial scan parameters";
	}
	throw "serial scan parameters not supported";	
}

/*
# Parameter Scan Values
PARAMETER_SCAN_BEGIN 2
1.0 
2.0 
PARAMETER_SCAN_END
*/
void FVSolver::loadSerialScanParameterValues(istream& ifsInput, int numSerialScanParameterValues) {
	if (simulation == 0) {
		throw "Simulation has to be initialized before loading serial scan parameter values";
	}
	throw "serial scan parameter not supported";
}

/*
# JMS_Paramters
JMS_PARAM_BEGIN
JMS_BROKER tcp://code:2507
JMS_USER serverUser cbittech
JMS_QUEUE workerEventDev
JMS_TOPIC serviceControlDev
VCELL_USER fgao
SIMULATION_KEY 36269803
JOB_INDEX 0
JMS_PARAM_END

# Simulation Parameters
SIMULATION_PARAM_BEGIN
SOLVER SUNDIALS_PDE_SOLVER 1.0E-7 1.0E-9 1.0
BASE_FILE_NAME \\cfs01.vcell.uchc.edu\raid\Vcell\users\fgao\SimID_36269803_0_
ENDING_TIME 0.1
TIME_STEP 0.1
KEEP_EVERY 1
SIMULATION_PARAM_END

# Model description: FEATURE name handle boundary_conditions
MODEL_BEGIN
FEATURE cyt 0 value value value value 
FEATURE ec 1 value value value value 
MEMBRANE cyt_ec_membrane cyt ec value value value value 
MODEL_END

# Mesh file
MESH_BEGIN
VCG_FILE \\cfs01.vcell.uchc.edu\raid\Vcell\users\fgao\SimID_36269803_0_.vcg
MESH_END

# Variables : type name time_dependent_flag advection_flag solve_whole_mesh_flag solve_regions
VARIABLE_BEGIN
VOLUME_PDE U false false false cyt
VARIABLE_END

COMPARTMENT_BEGIN cyt

BOUNDARY_CONDITIONS value value value value 

EQUATION_BEGIN U
INITIAL (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
RATE 0.0;
DIFFUSION 1.0;
VELOCITY_X 0.0;
VELOCITY_Y 0.0;
BOUNDARY_XM (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
BOUNDARY_XP (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
BOUNDARY_YM (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
BOUNDARY_YP (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
EQUATION_END

COMPARTMENT_END

COMPARTMENT_BEGIN ec

BOUNDARY_CONDITIONS value value value value 

EQUATION_BEGIN U
INITIAL (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
RATE 0.0;
DIFFUSION 0.0;
VELOCITY_X 0.0;
VELOCITY_Y 0.0;
BOUNDARY_XM (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
BOUNDARY_XP (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
BOUNDARY_YM (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
BOUNDARY_YP (sin((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * exp( - (19.739208802178677 * t)));
EQUATION_END

COMPARTMENT_END


MEMBRANE_BEGIN cyt_ec_membrane cyt ec

BOUNDARY_CONDITIONS value value value value 

JUMP_CONDITION_BEGIN U
FLUX cyt (6.28318530717958 * exp( - (19.739208802178677 * t)) * ((cos((3.14159265358979 * x)) * sin((3.14159265358979 * y)) * x) + (sin((3.14159265358979 * x)) * cos((3.14159265358979 * y)) * y)));
FLUX ec 0.0;
JUMP_CONDITION_END

MEMBRANE_END
*/
void FVSolver::createSimTool(istream& ifsInput, int taskID)
{
	SimTool::create();
	simTool = SimTool::getInstance();

	if (taskID < 0) { // no messaging
		SimulationMessaging::create();
	}
	string meshfile;
	string nextToken, line;

	while (!ifsInput.eof()) {
		getline(ifsInput, line);
		istringstream lineInput(line);

		nextToken = "";
		lineInput >> nextToken;
		if (nextToken.size() == 0 || nextToken[0] == '#') {
			continue;
		}

		if (nextToken == "JMS_PARAM_BEGIN") {
			loadJMSInfo(ifsInput, taskID);
#ifdef USE_MESSAGING
			SimulationMessaging::getInstVar()->start(); // start the thread
#endif
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_STARTING, "preprocessing started"));
		} else if (nextToken == "SIMULATION_PARAM_BEGIN") {
			loadSimulationParameters(ifsInput);
		} else if (nextToken == "MODEL_BEGIN") {
			loadModel(ifsInput);
			if (model == NULL) {
				throw "Model has 0 features";
			}
			simTool->setModel(model);
		} else if (nextToken == "MESH_BEGIN") {
			throw "MESH_BEGIN is deprecated.";
		} else if (nextToken == "CHOMBO_SPEC_BEGIN") {
			loadChomboSpec(ifsInput);
		} else if (nextToken == "VARIABLE_BEGIN") {
			loadSimulation(ifsInput);
			simTool->setSimulation(simulation);
		} else if (nextToken == "PARAMETER_BEGIN") {
			int numParams = 0;
			lineInput >> numParams;
			loadParameters(ifsInput, numParams);
		} else if (nextToken == "SERIAL_SCAN_PARAMETER_BEGIN") {
			int numSerialScanParams = 0;
			lineInput >> numSerialScanParams;
			loadSerialScanParameters(ifsInput, numSerialScanParams);
		} else if (nextToken == "SERIAL_SCAN_PARAMETER_VALUE_BEGIN") {
			int numSerialScanParamValues = 0;
			lineInput >> numSerialScanParamValues;
			loadSerialScanParameterValues(ifsInput, numSerialScanParamValues);
		} else if (nextToken == "FIELD_DATA_BEGIN") {
			loadFieldData(ifsInput);
		} else if (nextToken == "COMPARTMENT_BEGIN") {
			string feature_name;
			lineInput >> feature_name;
			Feature* feature = model->getFeatureFromName(feature_name);
			if (feature != NULL) {
				loadFeature(ifsInput, feature);
			} else {
				throw "createSimTool(), Invalid compartment when loading feature!";
			}
		} else if (nextToken == "MEMBRANE_BEGIN") {
			string mem_name, feature1_name, feature2_name;
			lineInput >> mem_name >> feature1_name >> feature2_name;
			Membrane* membrane = model->getMembraneFromName(mem_name);
			if (membrane != 0) {
				loadMembrane(ifsInput, membrane);
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

FVSolver::FVSolver(istream& fvinput, int taskID) {
	simTool = 0;
	simulation = 0;
	model = 0;
	chomboScheduler = 0;
	chomboSpec = 0;
	createSimTool(fvinput, taskID);
}

void FVSolver::solve()
{
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_STARTING, "preprocessing finished"));
	simTool->start();
}

void FVSolver::loadChomboSpec(istream& ifsInput) {

	string nextToken, line;
	int numLevels = 1;
	int* refineratios = 0;
	int maxBoxSize = 64;
	int viewLevel = -1;
	double fillRatio = 0.9;
	string* rois;

	ChomboGeometry* chomboGeometry = new ChomboGeometry();
	chomboSpec = new ChomboSpec();

	while (!ifsInput.eof()) {
		nextToken = "";
		getline(ifsInput, line);
		istringstream lineInput(line);

		lineInput >> nextToken;
		if (nextToken.size() == 0 || nextToken[0] == '#') {
			continue;
		}
		if (nextToken == "CHOMBO_SPEC_END") {
			break;
		}

		if (nextToken == "DIMENSION") {
			int geoDim;
			lineInput >> geoDim;
			assert(geoDim == SpaceDim);
			chomboGeometry->setDimension(geoDim);
		} else if (nextToken == "MESH_SIZE") {
			IntVect meshSize;
			for (int i = 0; i < SpaceDim; i ++) {
				lineInput >> meshSize[i];
			}
			chomboGeometry->setMeshSize(meshSize);
		} else if (nextToken == "DOMAIN_SIZE") {
			RealVect domainSize;
			for (int idir = 0; idir < SpaceDim; idir ++) {
				lineInput >> domainSize[idir];
			}
			chomboGeometry->setDomainSize(domainSize);
		} else if (nextToken == "DOMAIN_ORIGIN") {
			RealVect domainOrigin;
			for (int idir = 0; idir < SpaceDim; idir ++) {
				lineInput >> domainOrigin[idir];
			}
			chomboGeometry->setDomainOrigin(domainOrigin);
		} else if (nextToken == "SUBDOMAINS") {
			int numSubdomains;
			string distanceMap;
			int phaseIndex;
			lineInput >> numSubdomains >> distanceMap;

			// make sure the order of features is the same
			// as the order of features in model.
			// actually we can also use feature->getIndex() to identify the index.
			for (int i = 0; i < numSubdomains; i ++) {
				getline(ifsInput, line);
				istringstream lineInput(line);

				lineInput >> nextToken >> phaseIndex;
				Feature* feature = model->getFeatureFromName(nextToken);

				if (distanceMap.empty()) {
					VCell::Expression* exp = readExpression(lineInput, nextToken);
					chomboGeometry->addSubdomain(feature, phaseIndex, exp);
				} else {
					string distanceMapFile;
					getline(lineInput, distanceMapFile);
					trimString(distanceMapFile);
					chomboGeometry->addSubdomain(feature, phaseIndex, distanceMapFile);
				}
			}
		} else if (nextToken == "REFINEMENTS") {
			lineInput >> numLevels;
			refineratios = new int[numLevels];
			rois = new string[numLevels];
			for (int i = 0; i < numLevels; i ++) {
				getline(ifsInput, line);
				istringstream lineInput0(line);
				lineInput0 >> refineratios[i];
				getline(lineInput0, rois[i]);
				// trim
				rois[i].erase(rois[i].find_last_not_of(" \n\r\t") + 1);
			}
		} else if (nextToken == "MAX_BOX_SIZE") {
			lineInput >> maxBoxSize;
		} else if (nextToken == "VIEW_LEVEL") {
			lineInput >> viewLevel;
		} else if (nextToken == "FILL_RATIO") {
			lineInput >> fillRatio;
		}
	}

	if (viewLevel < 0)
	{
		viewLevel = numLevels - 1; // finest level
	}
	chomboSpec = new ChomboSpec(chomboGeometry, numLevels, maxBoxSize, fillRatio, viewLevel, rois, refineratios);
}
