/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/SimulationExpression.h>
#include <VCELL/SimTypes.h>
#include <VCELL/SimTool.h>
#include <VCELL/FieldData.h>
#include <VCELL/VolumeVariable.h>
#include <VCELL/MembraneVariable.h>
#include <VCELL/VolumeRegionVariable.h>
#include <VCELL/MembraneRegionVariable.h>
#include <VCELL/Element.h>
#include <VCELL/Mesh.h>
#include <ValueProxy.h>

class ValueProxyInside : public ValueProxy
{
public:
	ValueProxyInside(double* arg_values, int* arg_indices, Mesh* arg_mesh) 
		: ValueProxy(arg_values,  -1, arg_indices) {
			mesh = arg_mesh;
	}
	
	double evaluate() {
		MembraneElement* element = mesh->getMembraneElements() + indices[VAR_MEMBRANE_INDEX];
		if (element->insideIndexFar<0){
			return values[element->insideIndexNear];
		}else{
			return 1.5 * values[element->insideIndexNear] -	0.5 * values[element->insideIndexFar];
		}	
	}

private:
	Mesh* mesh;
};

class ValueProxyOutside : public ValueProxy
{
public:
	ValueProxyOutside(double* arg_values, int* arg_indices, Mesh* arg_mesh) 
		: ValueProxy(arg_values,  -1, arg_indices) {
			mesh = arg_mesh;
	}
	
	double evaluate() {
		MembraneElement* element = mesh->getMembraneElements() + indices[VAR_MEMBRANE_INDEX];
		if (element->outsideIndexFar<0){
			return values[element->outsideIndexNear];
		}else{
			return 1.5 * values[element->outsideIndexNear] - 0.5 * values[element->outsideIndexFar];
		}	
	}

private:
	Mesh* mesh;
};

SimulationExpression::SimulationExpression(Mesh *mesh) : Simulation(mesh) {
	oldSymbolTable = NULL;	
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
	if (oldSymbolTable != NULL) {
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

	// t, x, y, z, VAR, VAR_INSIDE, VAR_OUTSIDE, field data, parameters
	int numSymbols = 4 + volVarSize * 3 + (numVariables - volVarSize) + (int)fieldDataList.size() + (int)paramList.size();
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
		variableNames[variableIndex ++] = string(var->getName());

		oldValueProxies[variableIndex] = new ValueProxyInside(var->getOld(), indices, _mesh);
		variableNames[variableIndex ++] = string(var->getName()) + "_INSIDE";

		oldValueProxies[variableIndex] = new ValueProxyOutside(var->getOld(), indices, _mesh);
		variableNames[variableIndex ++] = string(var->getName()) + "_OUTSIDE";	
	}

	// Membrane ODE/PDE
	for (int i = 0; i < memVarSize; i ++) {
		Variable* var = memVarList[i];
		oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_MEMBRANE_INDEX, indices);
		variableNames[variableIndex ++] = string(var->getName());
	}
		
	// Volume Region
	for (int i = 0; i < volRegionVarSize; i ++) {
		Variable* var = volRegionVarList[i];
		oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_VOLUME_REGION_INDEX, indices);
		variableNames[variableIndex ++] = string(var->getName());
	} 		
		
	// Membrane Region
	for (int i = 0; i < memRegionVarSize; i ++) {
		Variable* var = memRegionVarList[i];
		oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_MEMBRANE_REGION_INDEX, indices);
		variableNames[variableIndex ++] = string(var->getName());
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
		variableNames[variableIndex ++] = fieldDataList[i]->getID();
	}

	// add parameters
	for (int i = 0; i < (int)paramList.size(); i ++) {
		oldValueProxies[variableIndex] = paramValueProxies[i];
		variableNames[variableIndex] = paramList[i];
		variableIndex ++;
	}

	oldSymbolTable = new SimpleSymbolTable(variableNames, variableIndex, oldValueProxies);
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
