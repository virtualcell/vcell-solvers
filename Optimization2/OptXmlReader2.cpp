#include "OptXmlReader2.h"

#include <float.h>
#include <iostream>
#include <sstream>
using namespace std;

#include <Expression.h>
using VCell::Expression;

#include "OptXml.h"
#include "OptProblemDescription.h"
#include "ObjectiveFunction.h"
#include "ExplicitObjectiveFunction.h"
#include "ExplicitFitObjectiveFunction.h"
#include "SimpleParameterDescription.h"
#include "ExplicitConstraintDescription.h"
#include "OdeObjectiveFunction.h"
#include "PdeObjectiveFunction.h"
#include "OdeResultSet.h"
#include "Constraint.h"
#include "SpatialReferenceData.h"
#include <tinyxml.h>

OptXmlReader2::OptXmlReader2(){
}

OptProblemDescription* OptXmlReader2::parseOptProblemDescription(const char* xmlText){
	TiXmlDocument doc;
	doc.Parse(xmlText);
	TiXmlElement* root = doc.FirstChildElement(OptProblemDescription_Tag);
	return parseOptProblemDescription(root);
}

OptProblemDescription* OptXmlReader2::readOptProblemDescription(const char* xmlFile){
	TiXmlDocument doc(xmlFile);
	doc.LoadFile();
	TiXmlElement* root = doc.FirstChildElement(OptProblemDescription_Tag);
	return parseOptProblemDescription(root);
}

OptProblemDescription* OptXmlReader2::parseOptProblemDescription(TiXmlElement* rootNode){
	ParameterDescription* paramDesc = parseParameterDescription(rootNode->FirstChildElement(ParameterDescription_Tag));
	SymbolTable* symbolTable = paramDesc->getSymbolTable();
	ConstraintDescription* constraintDesc = parseConstraintDescription(rootNode->FirstChildElement(ConstraintDescription_Tag),symbolTable);
	ObjectiveFunction* objFunction = parseObjectiveFunction(rootNode->FirstChildElement(ObjectiveFunction_Tag),paramDesc);
	return new OptProblemDescription(paramDesc,constraintDesc,objFunction);
}

ParameterDescription* OptXmlReader2::parseParameterDescription(TiXmlElement* paramDescNode){
	vector<string> nameVector;
	vector<double> lowVector;
	vector<double> highVector;
	vector<double> initVector;
	vector<double> scaleVector;
	TiXmlElement* parameter = paramDescNode->FirstChildElement(Parameter_Tag);
	while (parameter!=0){
		nameVector.push_back(string(parameter->Attribute(ParameterName_Attr)));

		const char* lowAttr = parameter->Attribute(ParameterLow_Attr);
		double low = atof(lowAttr);
		if (!strcmp(lowAttr,"-Infinity")){
			low = -DBL_MAX;
		}
		lowVector.push_back(low);

		const char* highAttr = parameter->Attribute(ParameterHigh_Attr);
		double high = atof(highAttr);
		if (!strcmp(highAttr,"Infinity")){
			high = DBL_MAX;
		}
		highVector.push_back(high);

		initVector.push_back(atof(parameter->Attribute(ParameterInit_Attr)));
		scaleVector.push_back(atof(parameter->Attribute(ParameterScale_Attr)));
		parameter = parameter->NextSiblingElement(Parameter_Tag);
	}
	int numParameters = (int)nameVector.size();
	double* lows = new double[numParameters];
	double* highs = new double[numParameters];
	double* inits = new double[numParameters];
	double* scales = new double[numParameters];
	for (int i=0;i<numParameters;i++){
		lows[i] = lowVector[i];
		highs[i] = highVector[i];
		inits[i] = initVector[i];
		scales[i] = scaleVector[i];
	}
	SimpleParameterDescription* simpleParamDescription = new SimpleParameterDescription(numParameters,nameVector,lows,highs,inits,scales);
	return simpleParamDescription;
}

ConstraintDescription* OptXmlReader2::parseConstraintDescription(TiXmlElement* constDescNode, SymbolTable* symbolTable){
	vector<Constraint*> constraints;
	if (constDescNode == 0) {
		return new ExplicitConstraintDescription(constraints, symbolTable);
	}
	TiXmlElement* constraintNode = constDescNode->FirstChildElement(Constraint_Tag);
	while (constraintNode!=0){
		const char* constraintType = constraintNode->Attribute(ConstraintType_Attr);
		ConstraintType cType;
		if (strcmp(constraintType,ConstraintType_Attr_LinearEquality)!=0){
			cType = EQUALITY_LINEAR;
		}else if (strcmp(constraintType,ConstraintType_Attr_NonlinearEquality)!=0){
			cType = EQUALITY_NONLINEAR;
		}else if (strcmp(constraintType,ConstraintType_Attr_LinearInequality)!=0){
			cType = INEQUALITY_LINEAR;
		}else if (strcmp(constraintType,ConstraintType_Attr_NonlinearInequality)!=0){
			cType = INEQUALITY_NONLINEAR;
		}
		TiXmlText* textNode = constraintNode->FirstChild()->ToText();
		const char* expString = textNode->Value();
		Constraint* constraint = new Constraint(cType,expString);
		constraints.push_back(constraint);
		constraintNode = constDescNode->NextSiblingElement(Constraint_Tag);
	}
	ExplicitConstraintDescription* constraintDesc = new ExplicitConstraintDescription(constraints,symbolTable);
	return constraintDesc;
}

ObjectiveFunction* OptXmlReader2::parseObjectiveFunction(TiXmlElement* objFuncNode, ParameterDescription* paramDesc){
	const char* objFuncTypeStr = objFuncNode->Attribute(ObjectiveFunctionType_Attr);
	if (strcmp(objFuncTypeStr, ObjectiveFunctionType_Attr_Explicit)==0){
		return parseExplicitObjectiveFunction(objFuncNode,paramDesc);
	}else if (strcmp(objFuncTypeStr,ObjectiveFunctionType_Attr_ExplicitFit)==0){
		return parseExplicitFitObjectiveFunction(objFuncNode,paramDesc);
	}else if (strcmp(objFuncTypeStr, ObjectiveFunctionType_Attr_PDE)==0){
#ifdef INCLUDE_PDE_OPT
		return parsePdeObjectiveFunction(objFuncNode,paramDesc);
#else
		throw "Pde Optimization not supported.";
#endif
	}else if (strcmp(objFuncTypeStr,ObjectiveFunctionType_Attr_ODE)==0){
		return parseOdeObjectiveFunction(objFuncNode,paramDesc);
	}else{
		throw "unknown objective function";
		// Unknown
	}
}

ExplicitObjectiveFunction* OptXmlReader2::parseExplicitObjectiveFunction(TiXmlElement* objFuncNode, ParameterDescription* paramDesc){
	const char* expressionString = objFuncNode->FirstChildElement(Expression_Tag)->GetText();
	Expression* exp = new Expression(string(expressionString));
	void (*checkStopRequested)(double, long) = 0;
	ExplicitObjectiveFunction* objFunc = new ExplicitObjectiveFunction(exp,paramDesc,paramDesc->getSymbolTable(),checkStopRequested);
	return objFunc;
}

ExplicitFitObjectiveFunction* OptXmlReader2::parseExplicitFitObjectiveFunction(TiXmlElement* objFuncNode, ParameterDescription* paramDesc){
	TiXmlElement* expressionNode = objFuncNode->FirstChildElement(Expression_Tag);
	//
	// get function
	//
	const char* expressionString = expressionNode->GetText();
	Expression* exp = new Expression(string(expressionString));

	//
	// get data set as OdeResultSet
	//
	TiXmlElement* dataNode = objFuncNode->FirstChildElement(Data_Tag);
	OdeResultSet* refData = parseOdeResultSet(dataNode);
	int numColumns = refData->getNumColumns();
	double* weights = new double[numColumns];
	for (int i=0;i<numColumns;i++){
		weights[i] = 1.0;
	}
	refData->setColumnWeights(weights);


	void (*checkStopRequested)(double, long) = 0;
	ExplicitFitObjectiveFunction* objFunc = new ExplicitFitObjectiveFunction(exp,paramDesc,refData,checkStopRequested);
	return objFunc;
}

OdeObjectiveFunction* OptXmlReader2::parseOdeObjectiveFunction(TiXmlElement* objFuncNode, ParameterDescription* paramDesc){
	//
	// get model (idaInput or cvodeInput text)
	//
	TiXmlElement* modelNode = objFuncNode->FirstChildElement(Model_Tag);
	string modelType(modelNode->Attribute(ModelType_Attr));
	if (modelType!=ModelType_Attr_IDA && modelType!=ModelType_Attr_CVODE){
		throw "unexpected model type, expected ida or cvode";
	}
	const char* solverInput = modelNode->GetText();
	//
	// get data set as OdeResultSet
	//
	TiXmlElement* dataNode = objFuncNode->FirstChildElement(Data_Tag);
	OdeResultSet* refData = parseOdeResultSet(dataNode);
	int numColumns = refData->getNumColumns();
	double* weights = new double[numColumns];
	memset(weights,0,numColumns*sizeof(double));

	//
	// parse modelMappings into expression strings
	//
	vector<string> modelMappingExpressions;
	TiXmlElement* modelMappingNode = objFuncNode->FirstChildElement(ModelMapping_Tag);
	while(modelMappingNode!=0){
		modelMappingExpressions.push_back(string(modelMappingNode->GetText()));
		const char* dataColumnName = modelMappingNode->Attribute(ModelMappingDataColumn_Attr);
		int refColumnIndex = refData->findColumn(string(dataColumnName));
		weights[refColumnIndex] = atof(modelMappingNode->Attribute(ModelMappingWeight_Attr));
		modelMappingNode = modelMappingNode->NextSiblingElement(ModelMapping_Tag);
	}
	refData->setColumnWeights(weights);

	OdeObjectiveFunction* odeObjectiveFunction = new OdeObjectiveFunction(paramDesc,refData,modelMappingExpressions,solverInput, 0);
	return odeObjectiveFunction;
}

OdeResultSet* OptXmlReader2::parseOdeResultSet(TiXmlElement* dataNode){
	OdeResultSet* refData = new OdeResultSet();
	string varName;
	string varType;
	int varDim;

	bool bFirst = true;
	TiXmlElement* variableNode = dataNode->FirstChildElement(Variable_Tag);
	while (variableNode!=0){
		varName = variableNode->Attribute(VariableName_Attr);
		varType = variableNode->Attribute(VariableType_Attr);
		varDim = atoi(variableNode->Attribute(VariableDimension_Attr));

		// make sure 1st is independent, rest are dependent, and all are dim=1
		if (varDim != 1){
			throw "unexpected ode data variable dimension != 1";
		}
		if (bFirst){ // t column
			if (varType != VariableType_Attr_Independent){
				throw "expected first data variable to be independent";
			}
		} else {
			if (varType != VariableType_Attr_Dependent) {
				throw "expected remaining data variables to be dependent";
			}
		}		
		refData->addColumn(varName);
		bFirst = false;
		variableNode = variableNode->NextSiblingElement(Variable_Tag);
	}
	
	TiXmlElement* rowNode = dataNode->FirstChildElement(Row_Tag);
	double* rowData = new double[refData->getNumColumns()];
	while (rowNode!=0){
		const char* dataText = rowNode->GetText();
		stringstream ss(dataText);		
		for (int i = 0; i < refData->getNumColumns(); i ++){
			ss >> rowData[i];
		}
		refData->addRow(rowData);
		rowNode = rowNode->NextSiblingElement(Row_Tag);
	}
	delete[] rowData;
	return refData;
}

#ifdef INCLUDE_PDE_OPT
PdeObjectiveFunction* OptXmlReader2::parsePdeObjectiveFunction(TiXmlElement* objFuncNode, ParameterDescription* paramDesc){	
	//
	// get model (fvInput text)
	//
	TiXmlElement* modelNode = objFuncNode->FirstChildElement(Model_Tag);
	string modelType(modelNode->Attribute(ModelType_Attr));
	if (modelType!=ModelType_Attr_FVSOLVER){
		throw "unexpected model type, expecting fvSolver";
	}
	const char* solverInput = modelNode->GetText();
	//
	// get data set as OdeResultSet
	//
	TiXmlElement* dataNode = objFuncNode->FirstChildElement(Data_Tag);
	SpatialReferenceData* refData = parsePdeResultSet(dataNode);
	int numColumns = refData->getNumVariables();
	double* weights = new double[numColumns];
	memset(weights,0,numColumns*sizeof(double));

	//
	// parse modelMappings into expression strings
	//
	vector<string> modelMappingExpressions;
	TiXmlElement* modelMappingNode = objFuncNode->FirstChildElement(ModelMapping_Tag);
	while(modelMappingNode!=0){
		modelMappingExpressions.push_back(string(modelMappingNode->GetText()));
		const char* dataColumnName = modelMappingNode->Attribute(ModelMappingDataColumn_Attr);
		int refVariableIndex = refData->findVariable(string(dataColumnName));
		weights[refVariableIndex] = atof(modelMappingNode->Attribute(ModelMappingWeight_Attr));
		modelMappingNode = modelMappingNode->NextSiblingElement(ModelMapping_Tag);
	}
	refData->setWeights(weights);

	PdeObjectiveFunction* pdeObjectiveFunction = new PdeObjectiveFunction(paramDesc,refData,modelMappingExpressions,solverInput,0);
	return pdeObjectiveFunction;	
}

SpatialReferenceData* OptXmlReader2::parsePdeResultSet(TiXmlElement* dataNode){
	SpatialReferenceData* refData = 0;
	string varName;
	string varType;
	int varDim;
	bool bFirst = true;

	TiXmlElement* variableNode = dataNode->FirstChildElement(Variable_Tag);
	while (variableNode!=0){
		varName = variableNode->Attribute(VariableName_Attr);
		varType = variableNode->Attribute(VariableType_Attr);
		varDim = atoi(variableNode->Attribute(VariableDimension_Attr));

		// make sure 1st is independent, rest are dependent
		if (bFirst){
			if (varType != VariableType_Attr_Independent){
				throw "expected first data variable to be independent";
			}
		} else {
			if (varType != VariableType_Attr_Dependent) {
				throw "expected remaining data variables to be dependent";
			}
		}

		if (!bFirst) { // don't add t
			if (refData == 0) {
				refData = new SpatialReferenceData(varDim);
			} else {
				if (varDim != refData->getDataSize()) {
					throw "SpatialReferenceData: all the variable must have same data size";
				}
			}		
			refData->addVariable(varName);
		}
		bFirst = false;
		variableNode = variableNode->NextSiblingElement(Variable_Tag);
	}

	TiXmlElement* rowNode = dataNode->FirstChildElement(Row_Tag);
	while (rowNode!=0){
		const char* dataText = rowNode->GetText();
		stringstream ss(dataText);
		double timePoint;		
		ss >> timePoint;
		for (int i = 0; i < refData->getNumVariables(); i ++){
			double* rowData = new double[refData->getDataSize()];
			for (int j = 0; j < refData->getDataSize(); j ++) {
				ss >> rowData[j];
			}
			refData->addTimePoint(timePoint);
			refData->addVariableData(i, rowData);
		}		
		rowNode = rowNode->NextSiblingElement(Row_Tag);
	}
	return refData;
}
#endif
