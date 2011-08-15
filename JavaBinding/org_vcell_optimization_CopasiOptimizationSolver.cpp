#include "org_vcell_optimization_CopasiOptimizationSolver.h"

/*
 * Class:     org_vcell_optimization_CopasiOptimizationSolver
 * Method:    solve
 * Signature: (Ljava/lang/String;Ljava/lang/String;Lorg/vcell/optimization/OptSolverCallbacks;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_org_vcell_optimization_CopasiOptimizationSolver_solve
  (JNIEnv *jniEnv, jclass thisClass, jstring j_optProblemXml, jobject object_optSolverCallbacks) {
	const char *c_optProblemXml = jniEnv->GetStringUTFChars(j_optProblemXml, 0);

	jniEnv->ReleaseStringUTFChars(j_optProblemXml, c_optProblemXml);
	return NULL;
}
