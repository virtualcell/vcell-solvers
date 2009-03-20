/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VarContext.h>
#include <VCELL/SimTypes.h>
#include <VCELL/Element.h>
#include <VCELL/Solver.h>
#include <VCELL/Variable.h>
#include <VCELL/Feature.h>
#include <VCELL/EqnBuilder.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/Region.h>
#include <VCELL/VolumeRegion.h>
#include <VCELL/MembraneRegion.h>
#include <VCELL/CartesianMesh.h>

#include <sstream>
using namespace std;
#include <Expression.h>

VarContext::VarContext(Feature *Afeature, string& AspeciesName)
{
	feature = Afeature;
	ASSERTION(feature);

	bInitialized = false;

	speciesName = AspeciesName;
	species = NULL;

	sim = NULL;
	mesh = NULL;
	    
	initialValue = NULL;

	expressions = new Expression*[TOTAL_NUM_EXPRESSIONS];
	constantValues = new double*[TOTAL_NUM_EXPRESSIONS];
	needsXYZ = new bool[TOTAL_NUM_EXPRESSIONS];
	memset(expressions, 0, TOTAL_NUM_EXPRESSIONS * sizeof(Expression*));
	memset(constantValues, 0, TOTAL_NUM_EXPRESSIONS * sizeof(double*));
	memset(needsXYZ, 0, TOTAL_NUM_EXPRESSIONS * sizeof(bool));
}

void VarContext::resolveReferences(Simulation *Asim)
{
	sim = Asim;
	if (sim == 0) {
		throw "VarContext::resolveReference(), simulation can't be null";
	}

	species = sim->getVariableFromName(speciesName);
	mesh = sim->getMesh();

	if (!species) {
		throw "VarContext::resolveReference(), variable is null";
	}
	if (!mesh) {
		throw "VarContext::resolveReference(), mesh is null";
	}
}

double VarContext::getInitialValue(long index)
{
	if (initialValue){
		return *initialValue;
	}
	throw "Application Error: neither initialValue nor getInitialValue() specified for VarContext";
}

VarContext::~VarContext()
{
	for (int i = 0; i < TOTAL_NUM_EXPRESSIONS; i ++) {
		delete expressions[i];		
		delete constantValues[i];
	}	
	delete[] expressions;
	delete[] constantValues;
	delete[] needsXYZ;
}

void VarContext::setExpression(Expression* newexp, int expIndex) {
	expressions[expIndex] = newexp;
}

void VarContext::bindAll(SymbolTable* symbolTable) {	
	for (int i = 0; i < TOTAL_NUM_EXPRESSIONS; i ++) {		
		if (expressions[i] == 0) {
			if (isNullExpressionOK(i)) {
				continue;
			} else {
				stringstream ss;
				ss << "VarContext::bindAll(), expression " << String_Expression_Index[i] << " for variable " << speciesName << " not defined";
				throw ss.str();
			}
		}
		try {
			//cout << expressions[i]->infix() << endl;
			double d = expressions[i]->evaluateConstant();
			constantValues[i] = new double[1];
			constantValues[i][0] = d;
		} catch (...) {		
			expressions[i]->bindExpression(symbolTable);
			if (expressions[i]->getSymbolBinding("x") != null ||
					expressions[i]->getSymbolBinding("y") != null ||
					expressions[i]->getSymbolBinding("z") != null) {
				needsXYZ[i] = true;
			}
		}
	}
}

double VarContext::getExpressionValue(MembraneElement* element, long expIndex)
{
	if (expressions[expIndex] == 0) { // not defined
		throw "VarContext::getExpressionValue(), expression not defined";
	}
	if (constantValues[expIndex] != NULL) {
		return constantValues[expIndex][0];
	}
	WorldCoord wc = mesh->getMembraneWorldCoord(element);
	((SimulationExpression*)sim)->setCurrentCoordinate(wc);
	int* indices = ((SimulationExpression*)sim)->getIndices();
	indices[VAR_MEMBRANE_INDEX] = element->index;
	indices[VAR_MEMBRANE_REGION_INDEX] = element->region->getId();
	return expressions[expIndex]->evaluateProxy();	
}

double VarContext::getExpressionConstantValue(long expIndex) {
	if (expressions[expIndex] == 0 || constantValues[expIndex] == 0) { // not defined
		throw "VarContext::getExpressionConstantValue() : expression not defined OR not a constant expression";
	}
	return constantValues[expIndex][0];	
}

double VarContext::getExpressionValue(long volIndex, long expIndex) {	
	if (expressions[expIndex] == 0) { // not defined
		stringstream ss;
		ss << "VarContext::getExpressionValue(), for variable " << speciesName << " expression " << String_Expression_Index[expIndex] << " not defined";
		throw ss.str();
	}
	if (constantValues[expIndex] != NULL) {
		return constantValues[expIndex][0];
	}
	if (needsXYZ[expIndex]) {
		WorldCoord wc = ((CartesianMesh*)mesh)->getVolumeWorldCoord(volIndex);
		((SimulationExpression*)sim)->setCurrentCoordinate(wc);
	}
	int* indices = ((SimulationExpression*)sim)->getIndices();
	indices[VAR_VOLUME_INDEX] = volIndex;
	indices[VAR_VOLUME_REGION_INDEX] = mesh->getVolumeElements()[volIndex].region->getId();
	return expressions[expIndex]->evaluateProxy();	
}


double VarContext::evalExpression(long expIndex, double* values) {	
	if (expressions[expIndex] == 0) { // not defined
		stringstream ss;
		ss << "VarContext::evalExpression(), for variable " << speciesName << " expression " << String_Expression_Index[expIndex] << " not defined";
		throw ss.str();
	}
	if (constantValues[expIndex] != NULL) {
		return constantValues[expIndex][0];
	}
	return expressions[expIndex]->evaluateVector(values);	
}