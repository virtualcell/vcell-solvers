/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <sstream>
using std::stringstream;

#include <VCELL/VarContext.h>
#include <VCELL/Element.h>
#include <VCELL/Variable.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/VolumeRegion.h>
#include <VCELL/MembraneRegion.h>
#include <VCELL/CartesianMesh.h>
#include <VCELL/Structure.h>
#include <VCELL/JumpCondition.h>
#include <VCELL/SimTool.h>
#include <Expression.h>

VarContext::VarContext(Structure *s, Variable* var)
{
	structure = s;
	ASSERTION(feature);

	species = var;
	sim = NULL;	

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
}

double VarContext::getInitialValue(long index)
{
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

	for (int i = 0; i < (int)jumpConditionList.size(); i ++) {
		delete jumpConditionList[i];
	}
	jumpConditionList.clear();	
}

void VarContext::setExpression(Expression* newexp, int expIndex) {
	if (expressions[expIndex] != 0) {
		stringstream ss;
		ss << "Expression " << String_Expression_Index[expIndex] << " for variable " << species->getName() << " in Structure " 
			<< structure->getName() << " has been set already";
		throw ss.str();
	}
	expressions[expIndex] = newexp;
}

void VarContext::bindAll(SimulationExpression* simulation) {
	SymbolTable* symbolTable = simulation->getSymbolTable();

	for (int i = 0; i < TOTAL_NUM_EXPRESSIONS; i ++) {
		if (expressions[i] == 0) {
			if (isNullExpressionOK(i)) {
				continue;
			} else {
				stringstream ss;
				ss << "VarContext::bindAll(), expression " << String_Expression_Index[i] << " for variable " << species->getName() << " not defined";
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

	for (int i = 0; i < (int)jumpConditionList.size(); i ++) {
		jumpConditionList[i]->bindExpression(symbolTable);
	}
}

double VarContext::evaluateExpression(MembraneElement* element, long expIndex)
{
	if (expressions[expIndex] == 0) { // not defined
		throw "VarContext::evaluateExpression(MembaneElement), expression not defined";
	}
	if (constantValues[expIndex] != NULL) {
		return constantValues[expIndex][0];
	}
	//cout << expressions[expIndex]->infix() << endl;
	if (needsXYZ[expIndex]) {
		WorldCoord wc = sim->getMesh()->getMembraneWorldCoord(element);
		((SimulationExpression*)sim)->setCurrentCoordinate(wc);
	}
	int* indices = ((SimulationExpression*)sim)->getIndices();
	indices[VAR_MEMBRANE_INDEX] = element->index;
	indices[VAR_MEMBRANE_REGION_INDEX] = element->getRegionIndex();
	return expressions[expIndex]->evaluateProxy();	
}

double VarContext::evaluateConstantExpression(long expIndex) {
	// pure constant
	if (constantValues[expIndex] != 0) {
		return constantValues[expIndex][0];
	}

	stringstream ss;
	ss << "VarContext::evaluateConstantExpression(), for variable " << species->getName() << " expression " << String_Expression_Index[expIndex] << " not defined OR not a constant expression";
	throw ss.str();
}

double VarContext::evaluateExpression(long volIndex, long expIndex) {
	if (expressions[expIndex] == 0) { // not defined
		stringstream ss;
		ss << "VarContext::evaluateExpression(VolIndex), for variable " << species->getName() << " expression " << String_Expression_Index[expIndex] << " not defined";
		throw ss.str();
	}
	if (constantValues[expIndex] != NULL) {
		return constantValues[expIndex][0];
	}
	if (needsXYZ[expIndex]) {
		WorldCoord wc = ((CartesianMesh*)sim->getMesh())->getVolumeWorldCoord(volIndex);
		((SimulationExpression*)sim)->setCurrentCoordinate(wc);
	}
	int* indices = ((SimulationExpression*)sim)->getIndices();
	indices[VAR_VOLUME_INDEX] = volIndex;
	indices[VAR_VOLUME_REGION_INDEX] = sim->getMesh()->getVolumeElements()[volIndex].getRegionIndex();
	return expressions[expIndex]->evaluateProxy();	
}

double VarContext::evaluateExpression(long expIndex, double* values) {
	if (expressions[expIndex] == 0) { // not defined
		stringstream ss;
		ss << "VarContext::evaluateExpression(), for variable " << species->getName() << " expression " << String_Expression_Index[expIndex] << " not defined";
		throw ss.str();
	}
	if (constantValues[expIndex] != NULL) {
		return constantValues[expIndex][0];
	}
	return expressions[expIndex]->evaluateVector(values);	
}

void VarContext::addJumpCondition(Membrane* membrane, Expression* exp) {
	JumpCondition* jc = new JumpCondition(membrane, exp);
	jumpConditionList.push_back(jc);
}

double VarContext::evaluateJumpCondition(MembraneElement* element)
{
	for (int i = 0; i < (int)jumpConditionList.size(); i ++) {
		if (jumpConditionList[i]->getMembrane() == element->getMembrane()) {
			return jumpConditionList[i]->evaluateExpression(((SimulationExpression*)sim), element);
		}
	}
	stringstream ss;
	ss << "Jump Condition for variable " << species->getName() << " in Feature " << structure->getName() << " not found";
	throw ss.str();
}

double VarContext::evaluateJumpCondition(MembraneElement* element, double* values)
{
	for (int i = 0; i < (int)jumpConditionList.size(); i ++) {
		if (jumpConditionList[i]->getMembrane() == element->getMembrane()) {
			return jumpConditionList[i]->evaluateExpression(values);
		}
	}
	stringstream ss;
	ss << "Jump Condition for variable " << species->getName() << " in Feature " << structure->getName() << " not found";
	throw ss.str();
}

bool VarContext::isConstantExpression(long expIndex) {
		// pure constant
	if (constantValues[expIndex] != 0) {
		return true;
	}

	// not defined
	if (expressions[expIndex] == 0) {
		stringstream ss;
		ss << "VarContext::isConstantExpression(), for variable " << species->getName() << " expression " << String_Expression_Index[expIndex] << " not defined";
		throw ss.str();
	}

	// has parameters only
	vector<string> symbols;
	expressions[expIndex]->getSymbols(symbols);
	for (int i = 0; i < (int)symbols.size(); i ++) {
		if (!((SimulationExpression*)SimTool::getInstance()->getSimulation())->isParameter(symbols[i])) {
			return false;
		}
	}
	return true;
}

double VarContext::evaluateMembraneRegionExpression(long memRegionIndex, long expIndex)
{
	int* indices = ((SimulationExpression*)sim)->getIndices();
	indices[VAR_MEMBRANE_REGION_INDEX] = memRegionIndex;
	return expressions[expIndex]->evaluateProxy();	
}

double VarContext::evaluateVolumeRegionExpression(long volRegionIndex, long expIndex)
{
	int* indices = ((SimulationExpression*)sim)->getIndices();
	indices[VAR_VOLUME_REGION_INDEX] = volRegionIndex;
	return expressions[expIndex]->evaluateProxy();
}

void VarContext::reinitConstantValues() {
	for (int i = 0; i < TOTAL_NUM_EXPRESSIONS; i ++) {
		if (expressions[i] == 0 || !isConstantExpression(i)) {
			continue;
		}
		double d = expressions[i]->evaluateProxy();
		if (constantValues[i] == 0) {
			constantValues[i] = new double[1];
		}
		constantValues[i][0] = d;
	}

	for (int i = 0; i < (int)jumpConditionList.size(); i ++) {
		jumpConditionList[i]->reinitConstantValues();
	}
}