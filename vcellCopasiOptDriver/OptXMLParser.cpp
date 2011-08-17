#include "OptXMLParser.h"
#include <iostream>
#include <sstream>
#define XML_FMT_INT_MOD "l"

static const string OptProblemDescription_Tag = "optProblemDescription";
	
static const string CopasiOptimizationMethod = "CopasiOptimizationMethod";
static const string CopasiOptimizationParameter = "CopasiOptimizationParameter";

static const string ParameterDescription_Tag = "parameterDescription";
static const string Parameter_Tag = "parameter";
static const string Name_Attr = "name";
static const string Value_Attr = "value";
static const string DataType_Attr = "dataType";
static const string ParameterLow_Attr = "low";
static const string ParameterHigh_Attr = "high";
static const string ParameterInit_Attr = "init";
static const string ParameterScale_Attr = "scale";

static const string MathModelSbmlFile_Tag = "MathModelSbmlFile";
static const string ExperimentalDataFile_Tag = "ExperimentalDataFile";
static const string Variable_Tag = "variable";
static const string VariableType_Attr = "type";
static const string VariableType_Attr_Independent = "independent";
static const string VariableType_Attr_Dependent = "dependent";
static const string ExperimentalDataFile_Attr_LastRow = "LastRow";


OptXMLParser::OptXMLParser()
{
	currentElement="";
}

void OptXMLParser::onStartElement(void *data, const char *el, const char **attr)
{
	OptXMLParser* optXMLParser = (OptXMLParser*)data;
	optXMLParser->currentElement = el;
	if(!strcmp(el,Variable_Tag.c_str()))
	{
		string name;
		bool bDependent = false;
		for (int i = 0; attr[i]; i += 2) 
		{			
			if (!strcmp(attr[i], VariableType_Attr.c_str())) {
				if (!strcmp(attr[i + 1], VariableType_Attr_Dependent.c_str())) {
					bDependent = true;
				}
			} else if (!strcmp(attr[i], Name_Attr.c_str())) {
				name = attr[i + 1];
			}
		}
		if (bDependent) {
			optXMLParser->optInfo.dependentVarNames.push_back(name);
		} else {
			optXMLParser->optInfo.independentVarName = name;
		}
		cout << "name=" << name << ",type=" << bDependent << endl;
	}
	else if(!strcmp(el, Parameter_Tag.c_str()))
	{
		OptParameter optParam;
		for (int i = 0; attr[i]; i += 2) 
		{			
			if (!strcmp(attr[i], Name_Attr.c_str())) 
			{
				optParam.name = attr[i + 1];
			}else if (!strcmp(attr[i], ParameterLow_Attr.c_str())) {
				optParam.lowerbound = attr[i + 1];
			}else if (!strcmp(attr[i], ParameterHigh_Attr.c_str())) {
				optParam.upperbound = attr[i + 1];
			}else if (!strcmp(attr[i], ParameterInit_Attr.c_str())) {
				stringstream ss (attr[i + 1]);
				ss >> optParam.iniVal;
			}else if (!strcmp(attr[i], ParameterScale_Attr.c_str())) {
				optParam.scale = attr[i + 1];
			}
		}
		optXMLParser->optInfo.optParameters.push_back(optParam);
	}
	else if(!strcmp(el, CopasiOptimizationParameter.c_str()))
	{
		OptMethodParameter methodParam;
		for (int i = 0; attr[i]; i += 2) 
		{			
			if (!strcmp(attr[i], Name_Attr.c_str())) 
			{
				methodParam.name = attr[i + 1];
			}else if (!strcmp(attr[i], Value_Attr.c_str())) {
				stringstream ss (attr[i + 1]);
				ss >> methodParam.value;
			}else if (!strcmp(attr[i], DataType_Attr.c_str())) {
				methodParam.dataType = attr[i + 1];
			}
		}
		optXMLParser->optInfo.methodParameters.push_back(methodParam);
	}
	else if(!strcmp(el, CopasiOptimizationMethod.c_str()))
	{
		for (int i = 0; attr[i]; i += 2) 
		{			
			if (!strcmp(attr[i], Name_Attr.c_str())) 
			{
				optXMLParser->optInfo.methodName = attr[i + 1];
			}
		}
	}
	else if(!strcmp(el, ExperimentalDataFile_Tag.c_str()))
	{
		for (int i = 0; attr[i]; i += 2) 
		{			
			if (!strcmp(attr[i], ExperimentalDataFile_Attr_LastRow.c_str())) 
			{
				stringstream ss(attr[i + 1]);
				ss >> optXMLParser->optInfo.expDataLastRow;
			}
		}
	}
	
}
void OptXMLParser::onEndElement(void *data, const char *el)
{
	OptXMLParser* optXMLParser = (OptXMLParser*)data;
	optXMLParser->currentElement = "";
}
void OptXMLParser::onCharacterData(void *data, const XML_Char *s, int len)
{
	OptXMLParser* optXMLParser = (OptXMLParser*)data;
	if (optXMLParser->currentElement == MathModelSbmlFile_Tag)
	{
	
		optXMLParser->optInfo.mathModelSBMLFile = string(s,0,len);
	}
	else if (optXMLParser->currentElement == ExperimentalDataFile_Tag)
	{
		optXMLParser->optInfo.experimentalDataFile = string(s,0,len);
	}
	
	cout<<""<<endl;
}
void OptXMLParser::parse(istream& inputstream)
{
	XML_Parser parser = XML_ParserCreate(NULL);
	string line;
	XML_SetUserData(parser, this);
	XML_SetElementHandler(parser, OptXMLParser::onStartElement, OptXMLParser::onEndElement);
	XML_SetCharacterDataHandler(parser, OptXMLParser::onCharacterData);

	while (true)  
	{
		int done = inputstream.eof();
		if (done) 
		{
			break;
		}

		line="";
		getline(inputstream, line);
		if (line == "") {
			continue;
		}
		//int len = (int)fread(Buff, 1, BUFFSIZE, inputFile);
		if (XML_Parse(parser, line.c_str(), line.size(), done) == XML_STATUS_ERROR) 
		{
			fprintf(stderr, "Parse error at line %" XML_FMT_INT_MOD "u:\n%s\n", XML_GetCurrentLineNumber(parser), XML_ErrorString(XML_GetErrorCode(parser)));
			exit(-1);
		}
		
	}
	
	XML_ParserFree(parser);
}