#ifndef OPTXMLWRITER_H
#define OPTXMLWRITER_H

class TiXmlElement;
class OptSolverResultSet;
class OdeResultSet;

class OptXmlWriter
{
public:
	OptXmlWriter();
	TiXmlElement* getXML(int arg_numParameters, 
		char** paramNames, double* arg_LB, double* arg_UB, double* arg_initialGuess, double* arg_scaleFactors,
		int arg_numNonLinearInequality, int arg_numLinearInequality, 
		int arg_numNonLinearEquality, int arg_numLinearEquality, 
		char** constraintExpressions, OdeResultSet* arg_referenceData, 
		char** refColumnMappingExpressions, char* arg_inputChars, void (*checkStopRequested)(double, long));
	TiXmlElement* getXML(OptSolverResultSet* optSolverResultSet, char* paramNames);
private:
	TiXmlElement* getOptSolverResultSet(OptSolverResultSet* optResultSetNode, char* paramNames);
};

#endif