#ifndef OPTXMLREADER2_H
#define OPTXMLREADER2_H

class OptProblemDescription;
class ObjectiveFunction;
class ConstraintDescription;
class ParameterDescription;
class ExplicitObjectiveFunction;
class OdeObjectiveFunction;
class PdeObjectiveFunction;
class TiXmlElement;
class SymbolTable;
class OdeResultSet;
class OptSolverResultSet;

class OptXmlReader2
{
public:
	OptXmlReader2();
	OptProblemDescription* parseOptProblemDescription(const char* xmlText);
	OptProblemDescription* readOptProblemDescription(const char* xmlFile);
	OptSolverResultSet* parseOptSolverResultSet(const char* xmlText);
	OptSolverResultSet* readOptSolverResultSet(const char* xmlFile);
private:
	OptProblemDescription* parseOptProblemDescription(TiXmlElement* rootNode);
	ObjectiveFunction* parseObjectiveFunction(TiXmlElement* objNode, ParameterDescription* paramDescription);
	ExplicitObjectiveFunction* parseExplicitObjectiveFunction(TiXmlElement* objNode, ParameterDescription* paramDescription);
	OdeObjectiveFunction* parseOdeObjectiveFunction(TiXmlElement* objNode, ParameterDescription* paramDescription);
	PdeObjectiveFunction* parsePdeObjectiveFunction(TiXmlElement* objNode, ParameterDescription* paramDescription);
	ParameterDescription* parseParameterDescription(TiXmlElement* parmDescNode);
	ConstraintDescription* parseConstraintDescription(TiXmlElement* constDescNode, SymbolTable* symbolTable);
	OdeResultSet* parseOdeResultSet(TiXmlElement* dataNode);
	OptSolverResultSet* parseOptSolverResultSet(TiXmlElement* dataNode);
};

#endif
