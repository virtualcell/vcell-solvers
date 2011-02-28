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
//class OdeResultSetOpt;
class OptResultSet;
struct OptRunResultSet;

class OptXmlWriter2
{
public:
//	TiXmlElement* getXML(OptProblemDescription* optProblemDescription);
	static TiXmlElement* getXML(OptResultSet* optSolverResultSet);

private:
	//TiXmlElement* getOptProblemDescription(OptProblemDescription* rootNode);
	//TiXmlElement* getObjectiveFunction(ObjectiveFunction* objNode, ParameterDescription* paramDescription);
	//TiXmlElement* getExplicitObjectiveFunction(ExplicitObjectiveFunction* objNode, ParameterDescription* paramDescription);
	//TiXmlElement* getOdeObjectiveFunction(OdeObjectiveFunction* objNode, ParameterDescription* paramDescription);
	//TiXmlElement* getPdeObjectiveFunction(PdeObjectiveFunction* objNode, ParameterDescription* paramDescription);
	//TiXmlElement* getParameterDescription(ParameterDescription* parmDescNode);
	//TiXmlElement* getConstraintDescription(ConstraintDescription* constDescNode, SymbolTable* symbolTable);
	//TiXmlElement* getOdeResultSet(OdeResultSetOpt* dataNode);
	static TiXmlElement* getOptResultSet(OptResultSet* optResultSetNode);
	static TiXmlElement* getOptRunResultSet(OptResultSet* optResultSet, OptRunResultSet& optRunResultSet);
};

#endif
