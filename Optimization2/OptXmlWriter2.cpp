#include <iostream>
#include <sstream>
using namespace std;

#include "OptXmlWriter2.h"
#include "OptXml.h"
#include "OptProblemDescription.h"
#include "OptResultSet.h"
#include "ObjectiveFunction.h"
#include "ExplicitObjectiveFunction.h"
#include "ParameterDescription.h"
#include "ConstraintDescription.h"
#include "OdeObjectiveFunction.h"
#include "PdeObjectiveFunction.h"
#include "OdeResultSet.h"
#include "Constraint.h"
#include <Expression.h>
#include <tinyxml.h>


//TiXmlElement* OptXmlWriter2::getXML(OptProblemDescription* optProblemDescription){
//	return getOptProblemDescription(optProblemDescription);
//}
//
TiXmlElement* OptXmlWriter2::getXML(OptResultSet* optResultSet){
	return getOptResultSet(optResultSet);
}

TiXmlElement* OptXmlWriter2::getOptResultSet(OptResultSet* optResultSet){
	TiXmlElement* optResultSetElement = new TiXmlElement(OptSolverResultSet_Tag);
	for (int i=0;i<(int)optResultSet->paramNames.size();i++){
		TiXmlElement* paramNode = new TiXmlElement(Parameter_Tag);
		paramNode->SetAttribute(ParameterName_Attr,optResultSet->paramNames[i].c_str());
		optResultSetElement->LinkEndChild(paramNode);
	}

	TiXmlElement* bestResultSetElement = getOptRunResultSet(optResultSet, optResultSet->bestRunResultSet);
	bestResultSetElement->SetValue(bestOptRunResultSet_Tag);
	optResultSetElement->LinkEndChild(bestResultSetElement);

	if ((int)optResultSet->vectProfileDistribution.size() > 0) {
		TiXmlElement* profileDistributionListElement = new TiXmlElement(ProfileDistributionList_Tag);
		for (int i = 0; i < (int)optResultSet->vectProfileDistribution.size(); i ++) {
			ProfileDistribution& profileDistribution = optResultSet->vectProfileDistribution[i];
			TiXmlElement* profileDistributionElement = new TiXmlElement(ProfileDistribution_Tag);
			profileDistributionElement->SetAttribute(ProfileDistribution_FixedParameter_Attr, profileDistribution.fixedParamName.c_str());
			for (int j = 0; j < (int)profileDistribution.optRunResultSets.size(); j ++) {
				OptRunResultSet& OptRunResultSet = profileDistribution.optRunResultSets[j];
				TiXmlElement* optRunResultSetElement = getOptRunResultSet(optResultSet, OptRunResultSet);
				profileDistributionElement->LinkEndChild(optRunResultSetElement);
			}
			profileDistributionListElement->LinkEndChild(profileDistributionElement);
		}
		optResultSetElement->LinkEndChild(profileDistributionListElement);
	}

	return optResultSetElement;
}

TiXmlElement* OptXmlWriter2::getOptRunResultSet(OptResultSet* optResultSet, OptRunResultSet& optRunResultSet){
	TiXmlElement* optRunResultSetElement = new TiXmlElement(OptRunResultSet_Tag);
	stringstream ss1;
	ss1 << optRunResultSet.objectiveFunctionValue;
	optRunResultSetElement->SetAttribute(OptSolverResultSetBestObjectiveFunction_Attr,ss1.str().c_str());
	stringstream ss2;
	ss2 << optRunResultSet.numObjFuncEvals;
	optRunResultSetElement->SetAttribute(OptSolverResultSetNumObjectiveFunctionEvaluations_Attr,ss2.str().c_str());
	string status = "unknown";
	switch (optRunResultSet.status){
		case unknown: {
			status = OptSolverResultSetStatus_Attr_Unknown;
			break;
		}
		case normalTermination: {
			status = OptSolverResultSetStatus_Attr_NormalTermination;
			break;
		}
		case nonfeasibleLinear: {
			status = OptSolverResultSetStatus_Attr_NonfeasibleLinear;
			break;
		}
		case nonfeasibleNonlinear: {
			status = OptSolverResultSetStatus_Attr_NonfeasibleNonlinear;
			break;
		}
		case noSolutionIterations: {
			status = OptSolverResultSetStatus_Attr_NoSolutionIterations;
			break;
		}
		case failedConstructingD0: {
			status = OptSolverResultSetStatus_Attr_FailedConstructingD0;
			break;
		}
		case failedConstructingD1: {
			status = OptSolverResultSetStatus_Attr_FailedConstructingD1;
			break;
		}
		case failedInconsistentInput: {
			status = OptSolverResultSetStatus_Attr_FailedInconsistentInput;
			break;
		}
		case failedIteratesStalled: {
			status = OptSolverResultSetStatus_Attr_FailedIteratesStalled;
			break;
		}
		case failedPenaltyTooLarge: {
			status = OptSolverResultSetStatus_Attr_FailedPenaltyTooLarge;
			break;
		}
		case failed: {
			status = OptSolverResultSetStatus_Attr_Failed;
			break;
		}
		case stoppedByUser: {
			status = OptSolverResultSetStatus_Attr_StoppedByUser;
			break;
		}
	}
	optRunResultSetElement->SetAttribute(OptSolverResultSetStatus_Attr,status.c_str());
	for (int i=0;i<(int)optRunResultSet.paramValues.size();i++){
		TiXmlElement* paramNode = new TiXmlElement(Parameter_Tag);
		paramNode->SetAttribute(ParameterName_Attr,optResultSet->paramNames[i].c_str());
		stringstream ss3;
		ss3 << optRunResultSet.paramValues[i];
		paramNode->SetAttribute(ParameterBestValue_Attr,ss3.str().c_str());
		optRunResultSetElement->LinkEndChild(paramNode);
	}
	return optRunResultSetElement;
}

//OptProblemDescription* OptXmlWriter2::parseOptProblemDescription(TiXmlElement* rootNode){
//	ParameterDescription* paramDesc = parseParameterDescription(rootNode->FirstChildElement(ParameterDescription_Tag));
//	SymbolTable* symbolTable = paramDesc->getSymbolTable();
//	ConstraintDescription* constraintDesc = parseConstraintDescription(rootNode->FirstChildElement(ConstraintDescription_Tag),symbolTable);
//	ObjectiveFunction* objFunction = parseObjectiveFunction(rootNode->FirstChildElement(ObjectiveFunction_Tag),paramDesc);
//	return new OptProblemDescription(paramDesc,constraintDesc,objFunction);
//}
//
//ParameterDescription* OptXmlWriter2::parseParameterDescription(TiXmlElement* paramDescNode){
//	vector<string> nameVector;
//	vector<double> lowVector;
//	vector<double> highVector;
//	vector<double> initVector;
//	vector<double> scaleVector;
//	TiXmlElement* parameter = paramDescNode->FirstChildElement(Parameter_Tag);
//	while (parameter!=0){
//		nameVector.push_back(string(parameter->Attribute(ParameterName_Attr)));
//		lowVector.push_back(atof(parameter->Attribute(ParameterLow_Attr)));
//		highVector.push_back(atof(parameter->Attribute(ParameterHigh_Attr)));
//		initVector.push_back(atof(parameter->Attribute(ParameterInit_Attr)));
//		scaleVector.push_back(atof(parameter->Attribute(ParameterScale_Attr)));
//		parameter = parameter->NextSiblingElement(Parameter_Tag);
//	}
//	int numParameters = nameVector.size();
//	double* lows = new double[numParameters];
//	double* highs = new double[numParameters];
//	double* inits = new double[numParameters];
//	double* scales = new double[numParameters];
//	for (int i=0;i<numParameters;i++){
//		lows[i] = lowVector[i];
//		highs[i] = highVector[i];
//		inits[i] = initVector[i];
//		scales[i] = scaleVector[i];
//	}
//	SimpleParameterDescription* simpleParamDescription = new SimpleParameterDescription(numParameters,nameVector,lows,highs,inits,scales);
//	return simpleParamDescription;
//}
//
//ConstraintDescription* OptXmlWriter2::parseConstraintDescription(TiXmlElement* constDescNode, SymbolTable* symbolTable){
//	vector<Constraint*> constraints;
//	TiXmlElement* constraintNode = constDescNode->FirstChildElement(Constraint_Tag);
//	while (constraintNode!=0){
//		const char* constraintType = constraintNode->Attribute(ConstraintType_Attr);
//		ConstraintType cType;
//		if (strcmp(constraintType,ConstraintType_Attr_LinearEquality)!=0){
//			cType = ConstraintType::EQUALITY_LINEAR;
//		}else if (strcmp(constraintType,ConstraintType_Attr_NonlinearEquality)!=0){
//			cType = ConstraintType::EQUALITY_NONLINEAR;
//		}else if (strcmp(constraintType,ConstraintType_Attr_LinearInequality)!=0){
//			cType = ConstraintType::INEQUALITY_LINEAR;
//		}else if (strcmp(constraintType,ConstraintType_Attr_NonlinearInequality)!=0){
//			cType = ConstraintType::INEQUALITY_NONLINEAR;
//		}
//		TiXmlText* textNode = constraintNode->FirstChild()->ToText();
//		const char* expString = textNode->Value();
//		Constraint* constraint = new Constraint(cType,expString);
//		constraints.push_back(constraint);
//		constraintNode = constDescNode->NextSiblingElement(Constraint_Tag);
//	}
//	ExplicitConstraintDescription* constraintDesc = new ExplicitConstraintDescription(constraints,symbolTable);
//	return constraintDesc;
//}
//
//ObjectiveFunction* OptXmlWriter2::parseObjectiveFunction(TiXmlElement* objFuncNode, ParameterDescription* paramDesc){
//	const char* objFuncTypeStr = objFuncNode->Attribute(ObjectiveFunctionType_Attr);
//	if (strcmp(objFuncTypeStr, ObjectiveFunctionType_Attr_Explicit)==0){
//		return parseExplicitObjectiveFunction(objFuncNode,paramDesc);
//	}else if (strcmp(objFuncTypeStr, ObjectiveFunctionType_Attr_PDE)==0){
//		return parsePdeObjectiveFunction(objFuncNode,paramDesc);
//	}else if (strcmp(objFuncTypeStr,ObjectiveFunctionType_Attr_ODE)==0){
//		return parseOdeObjectiveFunction(objFuncNode,paramDesc);
//	}else{
//		throw "unknown objective function";
//		// Unknown
//	}
//}
//
//ExplicitObjectiveFunction* OptXmlWriter2::parseExplicitObjectiveFunction(TiXmlElement* objFuncNode, ParameterDescription* paramDesc){
//	TiXmlPrinter printer;
//	TiXmlText* textNode = objFuncNode->FirstChild()->ToText();
//	printer.Visit(*textNode);
//	const char* expString = printer.CStr();
//	Expression* exp = new Expression(string(expString));
//	void (*checkStopRequested)(double, long) = 0;
//	ExplicitObjectiveFunction* objFunc = new ExplicitObjectiveFunction(exp,paramDesc,paramDesc->getSymbolTable(),checkStopRequested);
//	return objFunc;
//}
//
//PdeObjectiveFunction* OptXmlWriter2::parsePdeObjectiveFunction(TiXmlElement* objFuncNode, ParameterDescription* paramDesc){
//	throw "parsing PdeObjectiveFunction not yet supported";
//}
//
//OdeObjectiveFunction* OptXmlWriter2::parseOdeObjectiveFunction(TiXmlElement* objFuncNode, ParameterDescription* paramDesc){
//	//
//	// get model (idaInput or cvodeInput text)
//	//
//	TiXmlElement* modelNode = objFuncNode->FirstChildElement(Model_Tag);
//	string modelType(modelNode->Attribute(ModelType_Attr));
//	if (modelType!=ModelType_Attr_IDA && modelType!=ModelType_Attr_CVODE){
//		throw "unexpected model type, expected ida or cvode";
//	}
//	const char* solverInput = modelNode->GetText();
//	//
//	// get data set as OdeResultSet
//	//
//	TiXmlElement* dataNode = objFuncNode->FirstChildElement(Data_Tag);
//	OdeResultSet* refData = parseOdeResultSet(dataNode);
//	int numColumns = refData->getNumColumns();
//	double* weights = new double[numColumns];
//	memset(weights,0,numColumns*sizeof(double));
//
//	void (*checkStopRequested)(double, long) = 0;
//	//
//	// parse modelMappings into expression strings
//	//
//	vector<string> modelMappingExpressions;
//	TiXmlElement* modelMappingNode = objFuncNode->FirstChildElement(ModelMapping_Tag);
//	while(modelMappingNode!=0){
//		modelMappingExpressions.push_back(string(modelMappingNode->GetText()));
//		const char* dataColumnName = modelMappingNode->Attribute(ModelMappingDataColumn_Attr);
//		int refColumnIndex = refData->findColumn(string(dataColumnName));
//		weights[refColumnIndex] = atof(modelMappingNode->Attribute(ModelMappingWeight_Attr));
//		modelMappingNode = modelMappingNode->NextSiblingElement(ModelMapping_Tag);
//	}
//	refData->setColumnWeights(weights);
//
//	OdeObjectiveFunction* odeObjectiveFunction = new OdeObjectiveFunction(paramDesc,refData,modelMappingExpressions,solverInput,checkStopRequested);
//	return odeObjectiveFunction;
//}
//
//OdeResultSet* OptXmlWriter2::parseOdeResultSet(TiXmlElement* dataNode){
//	OdeResultSet* refData = new OdeResultSet();
//	vector<string> varNames;
//	vector<string> varTypes;
//	vector<int> varDims;
//	TiXmlElement* variableNode = dataNode->FirstChildElement(Variable_Tag);
//	while (variableNode!=0){
//		varNames.push_back(variableNode->Attribute(VariableName_Attr));
//		varTypes.push_back(variableNode->Attribute(VariableType_Attr));
//		varDims.push_back(atoi(variableNode->Attribute(VariableDimension_Attr)));
//		variableNode = variableNode->NextSiblingElement(Variable_Tag);
//	}
//	// make sure 1st is independent, rest are dependent, and all are dim=1
//	for (int i=0;i<varNames.size();i++){
//		if (varDims[i]!=1){
//			throw "unexpected data variable dimension != 1";
//		}
//		if (i==0){
//			if (varTypes[i] != VariableType_Attr_Independent){
//				throw "expected first data variable to be independent";
//			}
//		}else{
//			if (varTypes[i] != VariableType_Attr_Dependent){
//				throw "expected remaining data variables to be dependent";
//			}
//		}
//		refData->addColumn(varNames[i]);
//	}
//	TiXmlElement* rowNode = dataNode->FirstChildElement(Row_Tag);
//	while (rowNode!=0){
//		const char* dataText = rowNode->GetText();
//		stringstream ss(dataText);
//		double* rowData = new double[varNames.size()];
//		for (int i=0;i<varNames.size();i++){
//			ss >> rowData[i];
//		}
//		refData->addRow(rowData);
//		rowNode = rowNode->NextSiblingElement(Row_Tag);
//	}
//	return refData;
//}


/**
      <data>
         <variable type="independent" name="t" dim="1"/>
         <variable type="dependent" name="calcium" dim="20000"/>
         <row>0.0 16.18033988749905 3.819660112500951 1.180339887499049</row>
         <row>2.0 14.233628939985033 3.700193287600382 1.2998067123996204</row>
         <row>4.0 13.444291438073678 3.6445670692237253 1.3554329307762791</row>
         <row>6.0 13.098551382928354 3.618673977212261 1.381326022787741</row>
         <row>8.0 12.941981757625658 3.606619918706895 1.3933800812931074</row>
         <row>10.0 12.870006870163259 3.6010078126079903 1.398992187392012</row>
         <row>12.0 12.810093544564559 6.897400765603068 2.692174238075611</row>
         <row>14.0 12.80890204139876 6.8972206459011325 2.6923543577775444</row>
         <row>16.0 12.808319999477469 6.8971326497788965 2.69244235389978</row>
         <row>18.0 12.808035662528477 6.897089660150106 2.6924853435285723</row>
         <row>20.0 12.807896749788219 6.8970686570791795 2.692506346599499</row>
      </data>
**/
//void SpatialReferenceData::showXML(){
//	int numTimePoints = getNumTimePoints();
//	int numVariables = getNumVariables();
//	std::cout << "<data>" << std::endl;
//	std::cout << "<variable type=\"independent\" name=\"t\" dim=\"1\"/>" << std::endl;
//	for (int i=0;i<numVariables;i++){
//		std::cout << "<variable type=\"dependent\" name=\"" << variableList[i] << "\" dim=\"" << dataSize << "\"/>" << std::endl;
//	}
//	for (int i=0;i<numTimePoints;i++){
//		std::cout << "<row> " << getTimePoint(i) << " ";
//		for (int j=0;j<numVariables;j++){
//			const double *data = getData(i,j);
//			for (int k=0;k<dataSize;k++){
//				std::cout << data[k] << " ";
//			}
//		}
//		std::cout << "</row>\n" << std::endl;
//	}
//	std::cout << "</data>" << std::endl;
//}
