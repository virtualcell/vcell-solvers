#ifndef OPTXMLREADER2_H
#define OPTXMLREADER2_H

class OptProblemDescription;
class ObjectiveFunction;
class ConstraintDescription;
class ParameterDescription;
class ExplicitObjectiveFunction;
class OdeObjectiveFunction;
class TiXmlElement;
class SymbolTable;
class OdeResultSet;
class OptSolverResultSet;

#ifdef INCLUDE_PDE_OPT
class PdeObjectiveFunction;
class SpatialReferenceData;
#endif

class OptXmlReader2
{
public:
	OptXmlReader2();
	OptProblemDescription* parseOptProblemDescription(const char* xmlText);
	OptProblemDescription* readOptProblemDescription(const char* xmlFile);
	OptSolverResultSet* parseOptSolverResultSet(const char* xmlText);
	OptSolverResultSet* readOptSolverResultSet(const char* xmlFile);

private:
	ParameterDescription* parseParameterDescription(TiXmlElement* parmDescNode);
	OptProblemDescription* parseOptProblemDescription(TiXmlElement* rootNode);
	ConstraintDescription* parseConstraintDescription(TiXmlElement* constDescNode, SymbolTable* symbolTable);
	ObjectiveFunction* parseObjectiveFunction(TiXmlElement* objNode, ParameterDescription* paramDescription);

	ExplicitObjectiveFunction* parseExplicitObjectiveFunction(TiXmlElement* objNode, ParameterDescription* paramDescription);	

	OdeObjectiveFunction* parseOdeObjectiveFunction(TiXmlElement* objNode, ParameterDescription* paramDescription);	
	OdeResultSet* parseOdeResultSet(TiXmlElement* dataNode);

#ifdef INCLUDE_PDE_OPT
	SpatialReferenceData* parsePdeResultSet(TiXmlElement* dataNode);
	PdeObjectiveFunction* parsePdeObjectiveFunction(TiXmlElement* objNode, ParameterDescription* paramDescription);
#endif

	OptSolverResultSet* parseOptSolverResultSet(TiXmlElement* dataNode);
};

#endif
