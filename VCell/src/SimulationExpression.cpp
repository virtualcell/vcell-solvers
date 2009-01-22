/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/SimulationExpression.h>
#include <VCELL/SimTypes.h>
#include <VCELL/FieldData.h>
#include <VCELL/Variable.h>
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

	valueProxyTime = new ScalarValueProxy();
	valueProxyX = new ScalarValueProxy();
	valueProxyY = new ScalarValueProxy();
	valueProxyZ = new ScalarValueProxy();

	psfFieldDataIndex = -1;
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
	int numVolVar = getNumVolumeVariables();

	// t, x, y, z, VAR, VAR_INSIDE, VAR_OUTSIDE, field data, parameters
	int numSymbols = 4 + numVolVar * 3 + (numVariables - numVolVar) + (int)fieldDataList.size() + (int)paramList.size();
	string* variableNames = new string[numSymbols];	
	ValueProxy** oldValueProxies = new ValueProxy*[numSymbols];

	variableNames[0] = "t";
	oldValueProxies[0] = valueProxyTime;

	variableNames[1] = "x";	
	oldValueProxies[1] = valueProxyX;

	variableNames[2] = "y";	
	oldValueProxies[2] = valueProxyY;

	variableNames[3] = "z";	
	oldValueProxies[3] = valueProxyZ;

	int variableIndex = 4;

	for (int i = 0; i < (int)varList.size(); i ++) {
		Variable* var = varList[i];
		if (var->getVarType() == VAR_VOLUME) {
			oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_VOLUME_INDEX, indices);
			//currValueProxies[variableIndex] = new ValueProxy(var->getCurr(), VAR_VOLUME_INDEX, indices);
			variableNames[variableIndex ++] = string(var->getName());

			oldValueProxies[variableIndex] = new ValueProxyInside(var->getOld(), indices, _mesh);
			//currValueProxies[variableIndex] = new ValueProxyInside(var->getCurr(), indices, _mesh);
			variableNames[variableIndex ++] = string(var->getName()) + "_INSIDE";

			oldValueProxies[variableIndex] = new ValueProxyOutside(var->getOld(), indices, _mesh);
			//currValueProxies[variableIndex] = new ValueProxyOutside(var->getCurr(), indices, _mesh);
			variableNames[variableIndex ++] = string(var->getName()) + "_OUTSIDE";	

		} else if (var->getVarType() == VAR_MEMBRANE) {
			oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_MEMBRANE_INDEX, indices);
			//currValueProxies[variableIndex] = new ValueProxy(var->getCurr(), VAR_MEMBRANE_INDEX, indices);
			variableNames[variableIndex ++] = string(var->getName());

		} else if (var->getVarType() == VAR_CONTOUR) {
			oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_CONTOUR_INDEX, indices);
			//currValueProxies[variableIndex] = new ValueProxy(var->getCurr(), VAR_CONTOUR_INDEX, indices);
			variableNames[variableIndex ++] = string(var->getName());

		} else if (var->getVarType() == VAR_VOLUME_REGION) {
			oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_VOLUME_REGION_INDEX, indices);
			//currValueProxies[variableIndex] = new ValueProxy(var->getCurr(), VAR_VOLUME_REGION_INDEX, indices);
			variableNames[variableIndex ++] = string(var->getName());

		} else if (var->getVarType() == VAR_MEMBRANE_REGION) {			
			oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_MEMBRANE_REGION_INDEX, indices);
			//currValueProxies[variableIndex] = new ValueProxy(var->getCurr(), VAR_MEMBRANE_REGION_INDEX, indices);
			variableNames[variableIndex ++] = string(var->getName());

		} else if (var->getVarType() == VAR_CONTOUR_REGION) {
			oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_CONTOUR_REGION_INDEX, indices);
			//currValueProxies[variableIndex] = new ValueProxy(var->getCurr(), VAR_CONTOUR_REGION_INDEX, indices);
			variableNames[variableIndex ++] = string(var->getName());
		}
	}	

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
	for (int i = 0; i < (int)fieldDataList.size(); i ++) {
		darray[i] = fieldDataList[i]->getData()[index];
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
