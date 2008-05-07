/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/FastSystemExpression.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/Variable.h>
#include <VCELL/CartesianMesh.h>
#include <Expression.h>
#include <SimpleSymbolTable.h>

//-----------------------------------------------------------------
//
//  class FastSystem
//
//-----------------------------------------------------------------
FastSystemExpression::FastSystemExpression(int dimension, int numDepend, SimulationExpression* sim) 
: FastSystem(dimension, numDepend)
{	
	simulation = sim;
	pseudoConstants = new double[numDependents];
	memset(pseudoConstants, 0, numDependents * sizeof(double));	
	for (int i = 0; i < dimension; i ++) {
		pVars[i] = NULL;
	}
	for (int i = 0; i < numDependents; i ++) {
		pDependentVars[i] = NULL;
	}
	indepPseudoFieldSymbolTable = NULL;
	indepPseudoFieldValues = new double[4 + simulation->getNumFields() + dimension + numDependents];
	memset(indepPseudoFieldValues, 0, sizeof(double) * (4 + simulation->getNumFields() + dimension + numDependents));
	pseudoConstantExpressions = NULL;
	fastRateExpressions = NULL;
	fastDependencyExpressions = NULL;
	jacobianExpressions = NULL;	
	pseudoSymbols = 0;

	setTolerance(1e-7);
}

FastSystemExpression::~FastSystemExpression()
{
	delete[] pseudoConstants;
	delete[] indepPseudoFieldSymbolTable;
	delete[] indepPseudoFieldValues;
	for (int i = 0; i < dimension; i ++) {
		delete fastRateExpressions[i];
		for (int j = 0; j < dimension; j ++) {
			delete jacobianExpressions[i];
		}
	}
	for (int i = 0; i < numDependents; i ++) {
		delete pseudoConstantExpressions[i];
		delete fastDependencyExpressions[i];
	}
	delete[] pseudoConstantExpressions;
	delete[] fastRateExpressions;
	delete[] fastDependencyExpressions;
	delete[] jacobianExpressions;
}

void FastSystemExpression::setPseudoConstants(string* symbols, Expression** expressions) {
	pseudoSymbols = symbols;
	pseudoConstantExpressions = expressions;	
}

void FastSystemExpression::setFastRateExpressions(Expression** expressions) {
	fastRateExpressions = expressions;
}

void FastSystemExpression::bindAllExpressions() {
	if (dimension < 1 || pVars[0] == 0) {		
		throw "No independent variables defined in Fast System";
	}
	if (numDependents > 0 && pDependentVars[0] == 0) {
		throw "No dependent variables defined in Fast System";
	}

	//bind pseudo constants;
	int numSymbols = 4 + dimension + numDependents;
	string* indepAndDepSymbols = new string[numSymbols];
	indepAndDepSymbols[0] = "t";
	indepAndDepSymbols[1] = "x";
	indepAndDepSymbols[2] = "y";
	indepAndDepSymbols[3] = "z";
	for (int i = 0; i < dimension; i ++) {
		indepAndDepSymbols[i + 4] = pVars[i]->getName();
	}
	for (int i = 0; i < numDependents; i ++) {		
		indepAndDepSymbols[i + 4 + dimension] = pDependentVars[i]->getName();
	}
	SimpleSymbolTable* simpleSymbolTable = new SimpleSymbolTable(indepAndDepSymbols, numSymbols);	
	for (int i = 0; i < numDependents; i ++) {
		pseudoConstantExpressions[i]->bindExpression(simpleSymbolTable);
	}
	delete[] indepAndDepSymbols;

	// bind fast rate
	for (int i = 0; i < dimension; i ++) {		
		fastRateExpressions[i]->bindExpression(getIndepPseudoFieldSymbolTable());
	}

	// bind fast dependency
	for (int i = 0; i < numDependents; i ++) {		
		fastDependencyExpressions[i]->bindExpression(getIndepPseudoFieldSymbolTable());
	}
	// bind jacobian
	for (int i = 0; i < dimension * dimension; i ++) {		
		jacobianExpressions[i]->bindExpression(getIndepPseudoFieldSymbolTable());		
	}
}

void FastSystemExpression::setFastDependencyExpressions(string* symbols, Expression** expressions){
	for (int i = 0; i < numDependents; i ++) {
		if (symbols[i] != pDependentVars[i]->getName()) {
			throw "Fast dependency is out of order";
		}
	}
	fastDependencyExpressions = expressions;
}

void FastSystemExpression::setJacobianExpressions(Expression** expressions) {
	jacobianExpressions = expressions;
}

void FastSystemExpression::setCoordinates(double time_sec, WorldCoord& wc) {
	// t, x, y, x
	indepPseudoFieldValues[0] = time_sec;
	indepPseudoFieldValues[1] = wc.x;
	indepPseudoFieldValues[2] = wc.y;
	indepPseudoFieldValues[3] = wc.z;
	// field data
	simulation->populateFieldValues(indepPseudoFieldValues + 4, currIndex);
}


void FastSystemExpression::initVars()
{
	// independent variables
	int numSymbols = 4 + dimension + numDependents;
	double* values = new double[numSymbols];

	WorldCoord wc = simulation->getMesh()->getVolumeWorldCoord(currIndex);
	values[0] = simulation->getTime_sec();
	values[1] = wc.x;
	values[2] = wc.y;
	values[3] = wc.z;

	for (int i = 0; i < dimension; i ++) {		
		values[4 + i] = pVars[i]->getCurr(currIndex);
		setX(i, values[4 + i]);		
	}

	// dependent variables
	for (int i = 0; i < numDependents; i ++) {
		values[4 + i + dimension] = pDependentVars[i]->getCurr(currIndex);
	}

	// set values of pseudo constants by using initial values of independent and dependent variables.
	for (int i = 0; i < numDependents; i ++) {
		pseudoConstants[i] = pseudoConstantExpressions[i]->evaluateVector(values);
		indepPseudoFieldValues[4 + simulation->getNumFields() + dimension + i] = pseudoConstants[i];
	}

	delete[] values;
}

void FastSystemExpression::updateDependentVars() {	
	updateIndepValues();
	for (int i = 0; i < numDependents; i ++) {
		pDependentVars[i]->setCurr(currIndex, fastDependencyExpressions[i]->evaluateVector(indepPseudoFieldValues));
	}	
}

void FastSystemExpression::setDependentVariables(string* vars) {
	for (int i = 0; i < numDependents; i ++) {		
		pDependentVars[i] = simulation->getVariableFromName(vars[i]);
		if (pDependentVars[i] == NULL){
			stringstream ss;
			ss << "could not resolve variable " << vars[i];
			throw ss.str();
		}
	}
}

void FastSystemExpression::setIndependentVariables(string* vars) {	
	for (int i = 0; i < dimension; i ++) {		
		pVars[i] = simulation->getVariableFromName(vars[i]);
		if (pVars[i] == NULL){
			stringstream ss;
			ss << "could not resolve variable " << vars[i];
			throw ss.str();
		}		
	}
}

SimpleSymbolTable* FastSystemExpression::getIndepPseudoFieldSymbolTable() {
	if (indepPseudoFieldSymbolTable == NULL) {
		for (int i = 0; i < dimension; i ++) {
			if (pVars[i] == NULL) {
				throw "No independent variables defined";
			}
		}
		if (pseudoSymbols == NULL) {
			throw "No pseudo constants defined";
		}

		int numFields = simulation->getNumFields();
		string* indepPseudoFieldSymbols = new string[4 + numFields + dimension + numDependents];

		// t, x, y, x
		indepPseudoFieldSymbols[0] = "t";
		indepPseudoFieldSymbols[1] = "x";
		indepPseudoFieldSymbols[2] = "y";
		indepPseudoFieldSymbols[3] = "z";

		// field data
		string* fieldSymbols = simulation->getFieldSymbols();
		for (int i = 0; i < numFields; i ++) {
			indepPseudoFieldSymbols[4 + i] = fieldSymbols[i];
		}
		delete[] fieldSymbols;

		// dependent variables
		for (int i = 0; i < dimension; i ++) {		
			indepPseudoFieldSymbols[4 + numFields + i] = string(pVars[i]->getName());
		}

		// pseudo constant symbols
		for (int i = 0; i < numDependents; i ++) {		
			indepPseudoFieldSymbols[4 + numFields + dimension + i] = pseudoSymbols[i];
		}
		indepPseudoFieldSymbolTable = new SimpleSymbolTable(indepPseudoFieldSymbols, 4 + numFields + dimension + numDependents);	
		delete[] indepPseudoFieldSymbols;
	}
	return indepPseudoFieldSymbolTable;
}

void FastSystemExpression::updateIndepValues() {
	for (int i = 0; i < dimension; i ++) {		
		indepPseudoFieldValues[4 + simulation->getNumFields() + i] = getX(i);
	}	
}

void FastSystemExpression::resolveReferences(Simulation *sim) {
	bindAllExpressions();
}

void FastSystemExpression::updateMatrix()
{
	updateIndepValues();
	double mvalue = 0;
	int i, j;
	for (i = 0; i < dimension; i ++) {
		for (j = 0; j < dimension; j ++) {			
			mvalue = jacobianExpressions[i * dimension + j]->evaluateVector(indepPseudoFieldValues);
			setMatrix(i, j, mvalue);		
		}
		mvalue = -fastRateExpressions[i]->evaluateVector(indepPseudoFieldValues);
		setMatrix(i, j, mvalue);
	}	 
}
