#include <VCELL/JumpCondition.h>
#include <Expression.h>
#include <VCELL/SimTypes.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/CartesianMesh.h>
#include <VCELL/Element.h>
#include <VCELL/SimTool.h>

JumpCondition::JumpCondition(Membrane* m, Expression* e)
{
	membrane = m;
	expression = e;
	constantValue = 0;
	bNeedsXYZ = false;
}

JumpCondition::~JumpCondition(void)
{
	delete expression;
}

void JumpCondition::bindExpression(SymbolTable* symbolTable) {
	try {
			//cout << expression->infix() << endl;
			double d = expression->evaluateConstant();
			constantValue = new double[1];
			constantValue[0] = d;
		} catch (...) {		
			expression->bindExpression(symbolTable);
			if (expression->getSymbolBinding("x") != null ||
				expression->getSymbolBinding("y") != null ||
				expression->getSymbolBinding("z") != null) {
				bNeedsXYZ = true;
			}
		}
}

double JumpCondition::evaluateExpression(SimulationExpression* simulation, MembraneElement* element) {
	if (constantValue != 0) {
		return *constantValue;
	}	
	if (bNeedsXYZ) {
		CartesianMesh* mesh = (CartesianMesh*)simulation->getMesh();
		WorldCoord wc = mesh->getMembraneWorldCoord(element);
		simulation->setCurrentCoordinate(wc);
	}
	int* indices = simulation->getIndices();
	indices[VAR_MEMBRANE_INDEX] = element->index;
	indices[VAR_MEMBRANE_REGION_INDEX] = element->getRegionIndex();
	return expression->evaluateProxy();	
}

double JumpCondition::evaluateExpression(double* values) {
	if (constantValue != 0) {
		return *constantValue;
	}
	return expression->evaluateVector(values);	
}

bool JumpCondition::isConstantExpression() {
	// pure constant
	if (constantValue != 0) {
		return true;
	}

	// not defined
	if (expression == 0) {
		stringstream ss;
		ss << "JumpCondition::isConstantExpression(), expression not defined";
		throw ss.str();
	}

	// has parameters only
	vector<string> symbols;
	expression->getSymbols(symbols);
	for (int i = 0; i < (int)symbols.size(); i ++) {
		if (!((SimulationExpression*)SimTool::getInstance()->getSimulation())->isParameter(symbols[i])) {
			return false;
		}
	}
	return true;
}

void JumpCondition::reinitConstantValues() {
	if (expression == 0 || !isConstantExpression()) {
		return;
	}
	double d = expression->evaluateProxy();
	if (constantValue == 0) {
		constantValue = new double[1];
	}
	constantValue[0] = d;
}