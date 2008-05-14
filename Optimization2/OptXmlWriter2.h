#ifndef OPTXMLWRITER2_H
#define OPTXMLWRITER2_H

//class OptProblemDescription;
//class ObjectiveFunction;
//class ConstraintDescription;
//class ParameterDescription;
//class ExplicitObjectiveFunction;
//class OdeObjectiveFunction;
//class PdeObjectiveFunction;
class TiXmlElement;
//class SymbolTable;
//class OdeResultSet;
class OptResultSet;

class OptXmlWriter2
{
public:
	OptXmlWriter2();
//	TiXmlElement* getXML(OptProblemDescription* optProblemDescription);
	TiXmlElement* getXML(OptResultSet* optSolverResultSet);
private:
	//TiXmlElement* getOptProblemDescription(OptProblemDescription* rootNode);
	//TiXmlElement* getObjectiveFunction(ObjectiveFunction* objNode, ParameterDescription* paramDescription);
	//TiXmlElement* getExplicitObjectiveFunction(ExplicitObjectiveFunction* objNode, ParameterDescription* paramDescription);
	//TiXmlElement* getOdeObjectiveFunction(OdeObjectiveFunction* objNode, ParameterDescription* paramDescription);
	//TiXmlElement* getPdeObjectiveFunction(PdeObjectiveFunction* objNode, ParameterDescription* paramDescription);
	//TiXmlElement* getParameterDescription(ParameterDescription* parmDescNode);
	//TiXmlElement* getConstraintDescription(ConstraintDescription* constDescNode, SymbolTable* symbolTable);
	//TiXmlElement* getOdeResultSet(OdeResultSet* dataNode);
	TiXmlElement* getOptResultSet(OptResultSet* optResultSetNode);
};

#endif
