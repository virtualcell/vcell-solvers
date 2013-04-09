#include <VCELL/Structure.h>
#include <VCELL/Variable.h>
#include <VCELL/VarContext.h>

Structure::Structure(string& Aname) {
	name = Aname;
	size = 0;
//	odeVarCount = 0;
//	pdeVarCount = 0;

	for (int i = 0; i < 6; i ++)
	{
		boundaryType[i] = BOUNDARY_UNKNOWN;
	}
}

Structure::~Structure(void)
{
}

void Structure::addDefinedVariable(Variable* var) {
	definedVariableList.push_back(var);
//	if (var->isDiffusing())
//	{
//		++ pdeVarCount;
//	}
//	else
//	{
//		++ odeVarCount;
//	}
}

bool Structure::isVariableDefined(Variable* var){
	for (int i = 0; i < definedVariableList.size(); i ++) {
		if (var == definedVariableList[i]) {
			return true;
		}
	}
	return false;
}

void Structure::resolveReferences(SimulationExpression *sim)
{
	for (int i = 0; i < definedVariableList.size(); i ++) {
		definedVariableList[i]->getVarContext()->resolveReferences(sim);
	}
}