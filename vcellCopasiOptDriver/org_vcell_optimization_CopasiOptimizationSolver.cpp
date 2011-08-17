#include "org_vcell_optimization_CopasiOptimizationSolver.h"

#include <string>
using std::string;

#include "CopasiOptDriver.h"

JNIEXPORT jstring JNICALL Java_org_vcell_optimization_CopasiOptimizationSolver_solve
  (JNIEnv *jniEnv, jclass thisClass, jstring j_optProblemXml, jobject object_optSolverCallbacks) {
	const char *c_optProblemXml = jniEnv->GetStringUTFChars(j_optProblemXml, 0);
	string optXml(c_optProblemXml);
	string resultSetXml;
	CopasiOptDriver::run(optXml, resultSetXml);
	jniEnv->ReleaseStringUTFChars(j_optProblemXml, c_optProblemXml);
	jclass class_String = jniEnv->FindClass("java/lang/String");
	jstring j_optSolverResultSetXML = jniEnv->NewStringUTF(resultSetXml.c_str());
	return j_optSolverResultSetXML;
}
