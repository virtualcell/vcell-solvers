#include "org_vcell_optimization_CopasiOptimizationSolver.h"

#include <string>
using std::string;

#include "CopasiOptDriver.h"
#include "VCellCallback.h"

JNIEXPORT jstring JNICALL Java_org_vcell_optimization_CopasiOptimizationSolver_solve
  (JNIEnv *jniEnv, jclass thisClass, jstring j_optProblemXml, jobject object_optSolverCallbacks) {
	const char *c_optProblemXml = jniEnv->GetStringUTFChars(j_optProblemXml, 0);
	string optXml(c_optProblemXml);
	string resultSetXml;

	OptSolverCallbacks* optSolverCallbacks = new OptSolverCallbacks();
	optSolverCallbacks->jniEnv = jniEnv;
	optSolverCallbacks->object_optSolverCallbacks = object_optSolverCallbacks;
	jclass class_OptSolverCallbacks = jniEnv->GetObjectClass(object_optSolverCallbacks);
	optSolverCallbacks->mid_OptSolverCallbacks_getStopRequested = jniEnv->GetMethodID(class_OptSolverCallbacks, "getStopRequested", "()Z");
	optSolverCallbacks->mid_OptSolverCallbacks_setEvaluation = jniEnv->GetMethodID(class_OptSolverCallbacks, "setEvaluation", "(IDDLjava/lang/Double;)V");

	CopasiOptDriver::run(optXml, resultSetXml, optSolverCallbacks);
	jniEnv->ReleaseStringUTFChars(j_optProblemXml, c_optProblemXml);

	jstring j_optSolverResultSetXML = jniEnv->NewStringUTF(resultSetXml.c_str());
	return j_optSolverResultSetXML;
}
