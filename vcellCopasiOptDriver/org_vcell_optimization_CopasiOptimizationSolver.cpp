#include "org_vcell_optimization_CopasiOptimizationSolver.h"

#include <string>
using std::string;

#include "CopasiOptDriver.h"
#include "VCellCallback.h"
#include <CCopasiException.h>
#include <COptionParser.h>
#include <CNormalTranslation.h>

JNIEXPORT jstring JNICALL Java_org_vcell_optimization_CopasiOptimizationSolver_solve
  (JNIEnv *jniEnv, jclass thisClass, jstring j_optProblemXml, jobject object_optSolverCallbacks) {
	try
	{
		const char *c_optProblemXml = jniEnv->GetStringUTFChars(j_optProblemXml, 0);
		string optXml(c_optProblemXml);
		string resultSetXml;

		OptSolverCallbacks* optSolverCallbacks = new OptSolverCallbacks();
		optSolverCallbacks->jniEnv = jniEnv;
		optSolverCallbacks->object_optSolverCallbacks = object_optSolverCallbacks;
		jclass class_OptSolverCallbacks = jniEnv->GetObjectClass(object_optSolverCallbacks);
		optSolverCallbacks->mid_OptSolverCallbacks_getStopRequested = jniEnv->GetMethodID(class_OptSolverCallbacks, "getStopRequested", "()Z");
		optSolverCallbacks->mid_OptSolverCallbacks_setEvaluation = jniEnv->GetMethodID(class_OptSolverCallbacks, "setEvaluation", "(IDDLjava/lang/Double;I)V");

		CopasiOptDriver::run(optXml, resultSetXml, optSolverCallbacks);
		jniEnv->ReleaseStringUTFChars(j_optProblemXml, c_optProblemXml);

		jstring j_optSolverResultSetXML = jniEnv->NewStringUTF(resultSetXml.c_str());
		/*CCopasiMessage msg(CCopasiMessage::Type::ERROR, "%s", "this is a test");
		throw CCopasiException(msg);*/
		return j_optSolverResultSetXML;
	}
	catch(copasi::option_error &e)
	{
		jclass newExcCls = jniEnv->FindClass("java/lang/Exception");
		jniEnv->ThrowNew(newExcCls, e.what());
	}
	catch(CCopasiException &e)
	{
		jclass newExcCls = jniEnv->FindClass("java/lang/Exception");
		jniEnv->ThrowNew(newExcCls, e.getMessage().getText().c_str());
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
		jclass newExcCls = jniEnv->FindClass("java/lang/Exception");
		jniEnv->ThrowNew(newExcCls, e.what());
	}
	catch(...)
	{
		jclass newExcCls = jniEnv->FindClass("java/lang/Exception");
		char* errorMsg = "Unknown exception from Copasi optimization solver.";
		jniEnv->ThrowNew(newExcCls, errorMsg);
	}
}
