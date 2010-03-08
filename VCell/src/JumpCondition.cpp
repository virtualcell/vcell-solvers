#include <VCELL/JumpCondition.h>
#include <Expression.h>
#include <VCELL/SimTypes.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/CartesianMesh.h>
#include <VCELL/Element.h>

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
	CartesianMesh* mesh = (CartesianMesh*)simulation->getMesh();
	if (bNeedsXYZ) {
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