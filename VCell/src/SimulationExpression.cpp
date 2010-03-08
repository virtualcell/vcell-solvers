/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/SimulationExpression.h>
#include <VCELL/SimTool.h>
#include <VCELL/FieldData.h>
#include <VCELL/VolumeVariable.h>
#include <VCELL/MembraneVariable.h>
#include <VCELL/VolumeRegionVariable.h>
#include <VCELL/MembraneRegionVariable.h>
#include <VCELL/Element.h>
#include <VCELL/Mesh.h>
#include <ValueProxy.h>
#include <VCELL/RandomVariable.h>
#include <VCELL/Feature.h>
#include <VCELL/VCellModel.h>
#include <SimpleSymbolTable.h>
#include <ScalarValueProxy.h>

#define RANDOM_VARIABLE_FILE_EXTENSION ".rv"

class ValueProxyVolumeExtrapolate : public ValueProxy
{
public:
	ValueProxyVolumeExtrapolate(double* arg_values, int* arg_indices, Mesh* arg_mesh, Feature* f) 
		: ValueProxy(arg_values,  -1, arg_indices) {
			mesh = arg_mesh;
			feature = f;
	}
	
	double evaluate() {
		MembraneElement* element = mesh->getMembraneElements() + indices[VAR_MEMBRANE_INDEX];
		int nearIndex, farIndex;
		if (mesh->getVolumeElements()[element->vindexFeatureLo].getFeature() == feature) {
			nearIndex = element->vindexFeatureLo;
			farIndex = element->vindexFeatureLoFar;
		} else if (mesh->getVolumeElements()[element->vindexFeatureHi].getFeature() == feature) {
			nearIndex = element->vindexFeatureHi;
			farIndex = element->vindexFeatureHiFar;
		} else {
			throw "ValueProxyVolumeExtrapolate"; 
		}

		if (farIndex<0){
			return values[nearIndex];
		}else{
			return 1.5 * values[nearIndex] - 0.5 * values[farIndex];
		}	
	}

private:
	Mesh* mesh;
	Feature* feature;
};

//class ValueProxyOutside : public ValueProxy
//{
//public:
//	ValueProxyOutside(double* arg_values, int* arg_indices, Mesh* arg_mesh) 
//		: ValueProxy(arg_values,  -1, arg_indices) {
//			mesh = arg_mesh;
//	}
//	
//	double evaluate() {
//		MembraneElement* element = mesh->getMembraneElements() + indices[VAR_MEMBRANE_INDEX];
//		if (element->outsideIndexFar<0){
//			return values[element->outsideIndexNear];
//		}else{
//			return 1.5 * values[element->outsideIndexNear] - 0.5 * values[element->outsideIndexFar];
//		}	
//	}
//
//private:
//	Mesh* mesh;
//};

SimulationExpression::SimulationExpression(Mesh *mesh) : Simulation(mesh) {
	symbolTable = NULL;	
	//currSymbolTable = NULL;

	indices = new int[NUM_VAR_INDEX];
	for (int i = 0; i < NUM_VAR_INDEX; i ++) {
		indices[i] = -1;
	}

	valueProxyTime = 0;
	valueProxyX = 0;
	valueProxyY = 0;
	valueProxyZ = 0;

	numMemPde = 0;
	numVolPde = 0;
	bHasTimeDependentDiffusionAdvection = false;

	psfFieldDataIndex = -1;

	volVarSize = 0;
	memVarSize = 0;
	volRegionVarSize = 0;
	memRegionVarSize = 0;

	volVarList = 0;
	memVarList = 0;
	volRegionVarList = 0;
	memRegionVarList = 0;
}

SimulationExpression::~SimulationExpression() 
{
	delete valueProxyTime;
	delete valueProxyX;
	delete valueProxyY;
	delete valueProxyZ;
	for (int i = 0; i < (int)paramValueProxies.size(); i ++) {
		delete paramValueProxies[i];
	}
	for (int i = 0; i < (int)volVariableRegionMap.size(); i ++) {
		delete[] volVariableRegionMap[i];
	}
	volVariableRegionMap.clear();
}

void SimulationExpression::addVariable(Variable *var, bool* bSolveRegions)
{
	switch (var->getVarType()) {
	case VAR_VOLUME:
		addVolumeVariable((VolumeVariable*)var, bSolveRegions);
		break;
	case VAR_MEMBRANE:
		addMembraneVariable((MembraneVariable*)var);
		break;
	case VAR_VOLUME_REGION:
		addVolumeRegionVariable((VolumeRegionVariable *)var);
		break;
	case VAR_MEMBRANE_REGION:
		addMembraneRegionVariable((MembraneRegionVariable *)var);
		break;
	default:
		stringstream ss;
		ss << "Variable type " << var->getVarType() << " is not supported yet";
		throw ss.str();
	}	
}

void SimulationExpression::addVolumeVariable(VolumeVariable *var, bool* bSolveRegions)
{
	Simulation::addVariable(var);
	volVariableRegionMap.push_back(bSolveRegions);
	volVarSize ++;
	if (var->isPde()) {
		numVolPde ++;
	}
}

void SimulationExpression::addMembraneVariable(MembraneVariable *var)
{
	Simulation::addVariable(var);
	memVarSize ++;
	if (var->isPde()) {
		numMemPde ++;
	}
}

void SimulationExpression::addVolumeRegionVariable(VolumeRegionVariable *var)
{
	Simulation::addVariable(var);
	volRegionVarSize ++;
}

void SimulationExpression::addMembraneRegionVariable(MembraneRegionVariable *var)
{
	Simulation::addVariable(var);
	memRegionVarSize ++;
}

void SimulationExpression::advanceTimeOn() {
	Simulation::advanceTimeOn();
	valueProxyTime->setValue(getTime_sec()); 
}

void SimulationExpression::advanceTimeOff() {
	Simulation::advanceTimeOff();
	valueProxyTime->setValue(getTime_sec());
}

void SimulationExpression::update() {
	Simulation::update();
	valueProxyTime->setValue(getTime_sec());
}

void SimulationExpression::addParameter(string& param) {
	paramList.push_back(param);
	paramValueProxies.push_back(new ScalarValueProxy());
}

void SimulationExpression::createSymbolTable() {	
	if (symbolTable != NULL) {
		return;
	}

	int numVariables = (int)varList.size();

	if (volVarSize > 0) {
		volVarList = new VolumeVariable*[volVarSize];
	}
	if (memVarSize > 0) {
		memVarList = new MembraneVariable*[memVarSize];
	}
	if (volRegionVarSize > 0) {
		volRegionVarList = new VolumeRegionVariable*[volRegionVarSize];
	}
	if (memRegionVarSize > 0) {
		memRegionVarList = new MembraneRegionVariable*[memRegionVarSize];
	}
	int volVarCount = 0, memVarCount = 0, volRegionVarCount = 0, memRegionVarCount = 0;
	for (int i = 0; i < numVariables; i ++) {
		Variable* var = varList.at(i);
		switch (var->getVarType()) {
		case VAR_VOLUME:
			volVarList[volVarCount ++] = ((VolumeVariable*)var);
			break;
		case VAR_MEMBRANE:
			memVarList[memVarCount ++] = ((MembraneVariable*)var);
			break;
		case VAR_VOLUME_REGION:
			volRegionVarList[volRegionVarCount ++] = (VolumeRegionVariable*)var;
			break;
		case VAR_MEMBRANE_REGION:
			memRegionVarList[memRegionVarCount ++] = (MembraneRegionVariable*)var;
			break;
		}
	}
	
	bool bSundialsPdeSolver = SimTool::getInstance()->isSundialsPdeSolver();

	VCellModel* model = SimTool::getInstance()->getModel();

	// t, x, y, z, VAR, VAR_Feature1_membrane, VAR_Feature2_membrane, ... (for computing membrane flux), field data, parameters
	int numSymbols = 4 + volVarSize * (model->getNumFeatures() + 1) + (numVariables - volVarSize) + (int)fieldDataList.size() + (int)randomVarList.size() + (int)paramList.size();
	string* variableNames = new string[numSymbols];	
	ValueProxy** oldValueProxies = new ValueProxy*[numSymbols];

	valueProxyTime = new ScalarValueProxy();
	valueProxyX = new ScalarValueProxy();
	valueProxyY = new ScalarValueProxy();
	valueProxyZ = new ScalarValueProxy();

	variableNames[0] = "t";
	oldValueProxies[0] = valueProxyTime;

	variableNames[1] = "x";	
	oldValueProxies[1] = valueProxyX;

	variableNames[2] = "y";	
	oldValueProxies[2] = valueProxyY;

	variableNames[3] = "z";	
	oldValueProxies[3] = valueProxyZ;

	int variableIndex = 4;

	// Volume PDE/ODE
	for (int i = 0; i < volVarSize; i ++) {
		Variable* var = volVarList[i];
		oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_VOLUME_INDEX, indices);
		variableNames[variableIndex] = string(var->getName());
		variableIndex ++;

		for (int f = 0; f < model->getNumFeatures(); f ++) {
			Feature* feature = model->getFeatureFromIndex(f);
			oldValueProxies[variableIndex] = new ValueProxyVolumeExtrapolate(var->getOld(), indices, _mesh, feature);
			variableNames[variableIndex] = var->getName() + "_" + feature->getName() + "_membrane";
			variableIndex ++;
		}
	}

	// Membrane ODE/PDE
	for (int i = 0; i < memVarSize; i ++) {
		Variable* var = memVarList[i];
		oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_MEMBRANE_INDEX, indices);
		variableNames[variableIndex] = string(var->getName());
		variableIndex ++;
	}
		
	// Volume Region
	for (int i = 0; i < volRegionVarSize; i ++) {
		Variable* var = volRegionVarList[i];
		oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_VOLUME_REGION_INDEX, indices);
		variableNames[variableIndex] = string(var->getName());
		variableIndex ++;
	} 		
		
	// Membrane Region
	for (int i = 0; i < memRegionVarSize; i ++) {
		Variable* var = memRegionVarList[i];
		oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_MEMBRANE_REGION_INDEX, indices);
		variableNames[variableIndex] = string(var->getName());
		variableIndex ++;
	} 
		
		//} else if (var->getVarType() == VAR_CONTOUR) {
		//	oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_CONTOUR_INDEX, indices);
		//	variableNames[variableIndex ++] = string(var->getName());

		//} else if (var->getVarType() == VAR_CONTOUR_REGION) {
		//	oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_CONTOUR_REGION_INDEX, indices);
		//	variableNames[variableIndex ++] = string(var->getName());
		//}		

	// add field data
	for (int i = 0; i < (int)fieldDataList.size(); i ++) {
		if (fieldDataList[i]->getVariableType() == VAR_VOLUME) {
			oldValueProxies[variableIndex] = new ValueProxy(fieldDataList[i]->getData(), VAR_VOLUME_INDEX, indices);
		} else if (fieldDataList[i]->getVariableType() == VAR_MEMBRANE) {
			oldValueProxies[variableIndex] = new ValueProxy(fieldDataList[i]->getData(), VAR_MEMBRANE_INDEX, indices);
		} else {
			throw "field data is only supported for volume and membrane variables";
		}
		variableNames[variableIndex] = fieldDataList[i]->getID();
		variableIndex ++;
	}

	// add random variable
	if (randomVarList.size() > 0) {
		char rvfile[512];
		sprintf(rvfile, "%s%s", SimTool::getInstance()->getBaseFileName(), RANDOM_VARIABLE_FILE_EXTENSION);
		DataSet::readRandomVariables(rvfile, this);
		for (int i = 0; i < (int)randomVarList.size(); i ++) {
			RandomVariable* rv = randomVarList[i];

			int indexindex = VAR_VOLUME_INDEX;
			if (rv->getVariableType() == VAR_MEMBRANE) {
				indexindex = VAR_MEMBRANE_INDEX;
			}
			if (rv->getRandomNumbers() == 0) {
				stringstream ss;
				ss << "RandomVariable " << rv->getName() << " doesn't have any random numbers" << endl;
				throw ss.str();
			}
			oldValueProxies[variableIndex] = new ValueProxy(rv->getRandomNumbers(), indexindex, indices);
			variableNames[variableIndex] = rv->getName();
			variableIndex ++;
		}
	}

	// add parameters
	for (int i = 0; i < (int)paramList.size(); i ++) {
		oldValueProxies[variableIndex] = paramValueProxies[i];
		variableNames[variableIndex] = paramList[i];
		variableIndex ++;
	}

	/*for (int i = 0; i < variableIndex; i ++) {		
		cout << i << " " << variableNames[i] << endl;
	}*/

	symbolTable = new SimpleSymbolTable(variableNames, variableIndex, oldValueProxies);
	delete[] variableNames;	
}   

string* SimulationExpression::getFieldSymbols() {
	string* symbols = new string[fieldDataList.size()];
	for (int i = 0; i < (int)fieldDataList.size(); i ++) {
		symbols[i] = fieldDataList[i]->getID();
	}
	return symbols;
}

void SimulationExpression::populateFieldValues(double* darray, int index) {
	// here we don't know the index is for Volume or Membrane,
	// so we check array index is in bounds.
	for (int i = 0; i < (int)fieldDataList.size(); i ++) {
		int dataLength = fieldDataList[i]->getDataLength();
		if (index >= 0 && index < dataLength) {
			darray[i] = fieldDataList[i]->getData()[index];
		} else {
			darray[i] = 0;
		}
	}
}

void SimulationExpression::populateRandomValues(double* darray, int index) {
	// here we don't know the index is for Volume or Membrane,
	// so we check array index is in bounds.
	for (int i = 0; i < (int)randomVarList.size(); i ++) {
		RandomVariable* rv = randomVarList[i];
		
		if (index >= 0 && index < rv->getSize()) {
			darray[i] = rv->getRandomNumbers()[index];
		} else {
			darray[i] = 0;
		}
	}
}

void SimulationExpression::initSimulation()
{   
	createSymbolTable();
	Simulation::initSimulation();
}

void SimulationExpression::setParameterValues(double* paramValues) {
	if (paramValues == 0) {
		if (paramList.size() != 0) {
			throw "SimulationExpression::setParameterValues(), empty values for parameters";
		}
		return;
	}
	for (int i = 0; i < (int)paramList.size(); i ++) {
		paramValueProxies[i]->setValue(paramValues[i]);
	}
}

RandomVariable* SimulationExpression::getRandomVariableFromName(char* varName)
{
	for (int i = 0; i < (int)randomVarList.size(); i ++) {
		RandomVariable* var = randomVarList[i];
		if (string(varName) == var->getName()){
			return var;
		}
	}
	return NULL;
}

void SimulationExpression::setCurrentCoordinate(WorldCoord& wc) {
	valueProxyX->setValue(wc.x);
	valueProxyY->setValue(wc.y);
	valueProxyZ->setValue(wc.z);
}
