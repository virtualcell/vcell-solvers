#ifndef OPTXMLREADER2_H
#define OPTXMLREADER2_H

#include <vector>
using namespace std;
class OptProblemDescription;
class ObjectiveFunction;
class ConstraintDescription;
class ParameterDescription;
class ExplicitObjectiveFunction;
class ExplicitFitObjectiveFunction;
class OdeObjectiveFunction;
class TiXmlElement;
class SymbolTable;
class OdeResultSetOpt;
class OptSolverResultSet;
class Weights;
namespace VCell {
	class Expression;
}

#ifdef INCLUDE_PDE_OPT
class PdeObjectiveFunction;
class SpatialReferenceData;
#endif

class OptXmlReader2
{
public:
	static OptProblemDescription* parseOptProblemDescription(const char* xmlText);
	static OptProblemDescription* readOptProblemDescription(const char* xmlFile);
	static OptSolverResultSet* parseOptSolverResultSet(const char* xmlText);
	static OptSolverResultSet* readOptSolverResultSet(const char* xmlFile);
	    
private:
	vector<int> f(int x);
	static ParameterDescription* parseParameterDescription(TiXmlElement* parmDescNode, bool bComputeProfileDistributions);
	static OptProblemDescription* parseOptProblemDescription(TiXmlElement* rootNode);
	static ConstraintDescription* parseConstraintDescription(TiXmlElement* constDescNode, SymbolTable* symbolTable);
	static ObjectiveFunction* parseObjectiveFunction(TiXmlElement* objNode, ParameterDescription* paramDescription);

	static ExplicitObjectiveFunction* parseExplicitObjectiveFunction(TiXmlElement* objNode, ParameterDescription* paramDescription);	
	static ExplicitFitObjectiveFunction* parseExplicitFitObjectiveFunction(TiXmlElement* objNode, ParameterDescription* paramDescription);
	static void parseFunctionExpressions(TiXmlElement* expressionListNode, vector<VCell::Expression*>& expressionList, vector<int>& expDataColIndices);

	static OdeObjectiveFunction* parseOdeObjectiveFunction(TiXmlElement* objNode, ParameterDescription* paramDescription);	
	static OdeResultSetOpt* parseOdeResultSet(TiXmlElement* dataNode);
	static Weights* parseWeights(TiXmlElement* argWeightNode, int numDataColumns, int numDataRows);
	
#ifdef INCLUDE_PDE_OPT
	static SpatialReferenceData* parsePdeResultSet(TiXmlElement* dataNode);
	static PdeObjectiveFunction* parsePdeObjectiveFunction(TiXmlElement* objNode, ParameterDescription* paramDescription);
#endif

	static OptSolverResultSet* parseOptSolverResultSet(TiXmlElement* dataNode);
};

#endif
