#define COPASI_MAIN
#include <copasi.h>
#include <string>
using namespace std;

#include <CopasiDataModel/CCopasiDataModel.h>
#include <report/CCopasiRootContainer.h>
#include <utilities/CCopasiTask.h>
#include <utilities/CCopasiMethod.h>
#include <parameterFitting/CFitTask.h>
#include <parameterFitting/CFitItem.h>
#include <optimization/COptMethod.h>
#include <parameterFitting/CFitProblem.h>
#include <parameterFitting/CExperiment.h>
#include <parameterFitting/CExperimentSet.h>
#include <parameterFitting/CExperimentObjectMap.h>
#include <model/CModel.h>
#include <report/CKeyFactory.h>
#include <report/CCopasiObject.h>
#include <model/CModelValue.h>
#include <utilities/CCopasiParameterGroup.h>
#include "OptXMLParser.h"
#include <OptResultSet.h>
#include <OptXmlWriter2.h>
#include <tinyxml.h>
#include "CopasiOptDriver.h"


static const string DataType_int = "int";
static const string DataType_float = "float";

static CMetab* getModelMetabolite(CModel* model, string& metName)
{
	CCopasiVector<CMetab>& metVector = model->getMetabolites();
	for (int i = 0; i < metVector.size(); i++)
    {
      CMetab* source = metVector[i];
	  cout << source->getObjectName() << endl;
	  if (source->getObjectName() == metName) return source;
	}
	return NULL;
}

static CCopasiObject* getReactionParameter(CModel* model,string& reacName, string& paraName)
{

	CCopasiVectorNS<CReaction>& reactionVector = model->getReactions();
	//find the reaction
	CReaction* reacFound = NULL;
	for(int i = 0; i < reactionVector.size(); i++)
	{
		CReaction* reac = reactionVector[i];
		cout << reac->getObjectName() << endl;
		if (reac->getObjectName() == reacName)
		{
			reacFound = reac;
		}
	}
	//find the reaction parameter
	if(reacFound != NULL)
	{
		CCopasiParameterGroup& paramGroup = reacFound->getParameters();
		CCopasiContainer::objectMap objMap = paramGroup.getObjects();
		CCopasiContainer::objectMap::iterator it;
		for(it = objMap.begin(); it != objMap.end(); ++it)
	    {
			if(it->first == paraName)
			{
				return it->second;
			}
		}
	}
	return NULL;
}

CModelValue* CopasiOptDriver::getModelValue(CModel* model, string& paramName)
{
	CCopasiVectorN<CModelValue>& paramVector = model->getModelValues();
	for (int i = 0; i < paramVector.size(); i++)
    {
      CModelValue* source = paramVector[i];
	  //cout << source->getObjectName() << endl;
	  if (source->getObjectName() == paramName) return source;
	}
	return NULL;
}

CCopasiMethod::SubType CopasiOptDriver::nameToTypeEnum(const string& nameStr, const char ** enumNames, const CCopasiMethod::SubType & enumDefault)
{
    if (nameStr == "") return enumDefault;

    for (int i = 0; *enumNames; i++, enumNames++)
	{
		if (!strcmp(nameStr.c_str(), *enumNames)) 
			return static_cast<CCopasiMethod::SubType>(i);
	}
    return enumDefault;
}

void CopasiOptDriver::run(string& optXML, string& resultSetXML)
{
	OptXMLParser optParser;
	stringstream ss(optXML);
	optParser.parse(ss);
	OptInfo& optInfo = optParser.getOptInfo();
			
	//get copasi data model from sbml file
	CCopasiRootContainer::init(0, 0);
	CCopasiDataModel* dataModel = NULL;
	dataModel = CCopasiRootContainer::addDatamodel();

	bool isOK = dataModel->importSBML(optInfo.mathModelSBMLFile);
	
	//create parameter estimation task
	CFitTask* fitTask = (CFitTask *)((*dataModel->getTaskList())["Parameter Estimation"]);
    // the method in a fit task is an instance of COptMethod or a subclass of it.
	// set fitMethod from optXML
    COptMethod* fitMethod=(COptMethod*)fitTask->getMethod();
	string methodTypeName = optInfo.methodName;
	CCopasiMethod::SubType type = nameToTypeEnum(methodTypeName, CCopasiMethod::SubTypeName, CCopasiMethod::unset);
	vector<OptMethodParameter>& methodParameters = optInfo.methodParameters;
	fitTask->setMethodType(type);
	for(int i=0; i < methodParameters.size(); i++)
	{
		fitMethod->removeParameter(methodParameters[i].name);
		if(methodParameters[i].dataType == DataType_int)
		{
			fitMethod->addParameter(methodParameters[i].name, CCopasiParameter::UINT, (unsigned C_INT32) methodParameters[i].value);
		}else
		{ 
			fitMethod->addParameter(methodParameters[i].name, CCopasiParameter::DOUBLE, (C_FLOAT64) methodParameters[i].value);
		}
	}
    // the object must be an instance of COptMethod or a subclass thereof
    CFitProblem* fitProblem=(CFitProblem*)fitTask->getProblem();
  
	//Set up experimental data
    CExperimentSet* experimentSet=(CExperimentSet*)fitProblem->getParameter("Experiment Set");
    // first experiment (we only have one here)
    CExperiment* experiment=new CExperiment(dataModel);
    // tell COPASI where to find the data
    experiment->setFileName(optInfo.experimentalDataFile);
    // the data start in row 1 and goes to row 236
    experiment->setFirstRow(1);
	experiment->setLastRow(optInfo.expDataLastRow);
    experiment->setHeaderRow(1);
    experiment->setExperimentType(CCopasiTask.timeCourse); //time course, which will take first column as Time.
	//set up var to exp data map, length is total vars + "t"
	vector<string>& vars = optInfo.dependentVarNames;
	experiment->setNumColumns(1 + vars.size());
    CExperimentObjectMap& objectMap=experiment->getObjectMap();
    bool result = objectMap.setNumCols(1 + vars.size());

	//time mapping
	result = objectMap.setRole(0,CExperiment.time);
    CModel* model=dataModel->getModel();
	CCopasiObjectName objectName("Reference=Time");
    const CCopasiObject* timeReference=model->getObject(objectName);
    objectMap.setObjectCN(0,timeReference->getCN());
	
    // now we tell COPASI which column contain the concentrations of
    // metabolites and belong to dependent variables
	CKeyFactory* keyFactory=CCopasiRootContainer::getKeyFactory();
    int count = 1; //starts from 1 to skip first Time column
	for(int i=0; i<vars.size(); i++)
    {
		string& modelVar =  vars[i];
    	CModelValue* copasiVar = getModelValue(model, modelVar);
    	if(copasiVar != NULL)
    	{
        	//cout << "metabolite's name:" << copasiVar->getObjectName() << endl;
			objectMap.setRole(count,CExperiment.dependent); 
			CCopasiObjectName objName("Reference=Concentration");
        	const CCopasiObject* particleReference = copasiVar->getObject(objName);
	        objectMap.setObjectCN(count,copasiVar->getCN());
	        count ++;
    	}
    }
	        
	experimentSet->addParameter(experiment);
	
    // now we have to define the fit items from opt XML
    CCopasiParameterGroup* optimizationItemGroup=(CCopasiParameterGroup*)fitProblem->getParameter("OptimizationItemList");
	vector<OptParameter>& optParameters = optInfo.optParameters;
	for(int i=0; i<optParameters.size(); i++)
    {
		string& parameterName = optParameters[i].name;
    	// define a CFitItem in copasi
		CModelValue* copasiVar = getModelValue(model, parameterName);
    	
    	CCopasiObjectName cObjName("Reference=InitialValue");
		const CCopasiObject* parameterReference = copasiVar->getObject(cObjName);
        CFitItem* fitItem = new CFitItem(dataModel);
        fitItem->setObjectCN(parameterReference->getCN());
		fitItem->setStartValue(optParameters[i].iniVal); 
		CCopasiObjectName cObjName1(optParameters[i].lowerbound);
		CCopasiObjectName cObjName2(optParameters[i].upperbound);
        fitItem->setLowerBound(cObjName1);
		fitItem->setUpperBound(cObjName2);
        // add the fit item to the parameter group
        optimizationItemGroup->addParameter(fitItem);
    }
    
	//to escape Copasi warnings
	CCopasiMessage::clearDeque();
	assert(CCopasiRootContainer::getDatamodelList()->size() > 0);
	if (!fitTask->initialize(CCopasiTask::NO_OUTPUT, (*CCopasiRootContainer::getDatamodelList())[0], NULL))
	{
		cout << CCopasiMessage::peekLastMessage().getAllMessageText() << endl;
	}

	//dataModel->saveModel("d:\\aaa.cps", NULL, true, false);

    // running the task for this example will probably take some time
    result=fitTask->process(true);
  
	//get results
	OptResultSet optResultSet;
	optResultSet.bestRunResultSet.objectiveFunctionValue = fitProblem->getSolutionValue();
	optResultSet.bestRunResultSet.numObjFuncEvals = (int)fitProblem->getFunctionEvaluations();
	int optItemSize = (int)fitProblem->getOptItemSize();

	optResultSet.paramNames.resize(optItemSize);
	optResultSet.bestRunResultSet.paramValues.resize(optItemSize);
	for(int i=0 ; i<optItemSize; i++)
	{
		COptItem* optItem = fitProblem->getOptItemList()[i];
		optResultSet.paramNames[i] = optItem->getObject()->getCN().getRemainder().getRemainder().getElementName(0);
		optResultSet.bestRunResultSet.paramValues[i] = fitProblem->getSolutionVariables()[i];
		//cout << "value for " << optResultSet.paramNames[i] << ": " << optResultSet.bestRunResultSet.paramValues[i] << endl;
	}

	//save results to XML
	TiXmlElement* optSolverResultSetXMLNode = OptXmlWriter2::getXML(&optResultSet);
	TiXmlPrinter printer;
	optSolverResultSetXMLNode->Accept(&printer);

	resultSetXML = string(printer.CStr());

finish:
	CCopasiRootContainer::destroy();
	cout << resultSetXML << endl;
}

