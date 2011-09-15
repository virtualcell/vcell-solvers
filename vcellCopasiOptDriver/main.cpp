
#include <stdio.h>
#include <iostream>
#include <OptResultSet.h>
#include "CopasiOptDriver.h"
#include <CCopasiException.h>
#include <COptionParser.h>
#include <CNormalTranslation.h>

#include <string>
using namespace std;

int main(int argc, char *argv[])
{
	try{
		string fileName = "D:\\COPASI\\copasiOptxml.txt"; 
		FILE* fp = fopen(fileName.c_str(), "r");
		char buffer[8192];
		int len = fread(buffer, 1, 8192, fp);
		buffer[len] = 0;
		string s(buffer);
		string resultSetXML;
		CopasiOptDriver copasiOptDriver;
		copasiOptDriver.run(s, resultSetXML, 0);

		cout << "the end of program";
	      //return new COPASIOptResults(objFuncValue, numEvaluation, paramNames, paramVals);
	}catch(copasi::option_error &e)
	{
		cout << e.what();
	}
	catch(CCopasiException &e)
	{
		cout<< e.getMessage().getText().c_str();
	}
	//catch(recursion_limit_exception &e)
	//{
	//	jclass newExcCls = jniEnv->FindClass("java/lang/Exception");
	//	if(e.mType == recursion_limit_exception::SIMPLIFY)
	//	{
	//		char* errorMsg = "Recursive count exceeds the recursive limit for simplifying a given CEvaluationNode based tree";
	//		jniEnv->ThrowNew(newExcCls, errorMsg);
	//	}
	//	else //NORM_AND_SIMPLIFY
	//	{
	//		char* errorMsg = "Recursive depth exceeds the recursive limit for simplifying a given CEvaluationNode based tree";
	//		jniEnv->ThrowNew(newExcCls, errorMsg);
	//	}
	//}
	catch(std::exception &e)
	{
		cout << e.what();
	}
	catch(...)
	{
		char* errorMsg = "Unknown exception from Copasi optimization solver.";
		cout << errorMsg;
	}
}