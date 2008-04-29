#include "OptSolver2.h"
#include "OptResultSet.h"
//#include "cbit_vcell_opt_solvers_NativeOptSolver.h"
#include "org_vcell_optimization_NativeOptSolver.h"
#include "OptSolverCFSQP.h"
#include "OptProblemDescription.h"
#include "OptXmlReader2.h"
#include "OptXmlWriter2.h"
#include "StoppedByUserException.h"
#include "ParameterDescription.h"
#include "ObjectiveFunction.h"
#include "MemoryManager.h"
#include "tinyxml.h"

static JNIEnv* env_JNI = NULL;
static jobject obj_OptSolverCallbacks;
static jmethodID mid_OptSolverCallbacks_addEvaluation;
static jmethodID mid_OptSolverCallbacks_getStopRequested;

class JavaOptSolverListener : public OptSolver2Listener {
	virtual void handleObjectiveFunctionEvalEvent(int numParameters, double* paramValues, double objValue) {
		jdoubleArray j_parameterValues = env_JNI->NewDoubleArray(numParameters);
		env_JNI->SetDoubleArrayRegion(j_parameterValues, 0, numParameters, paramValues);
		env_JNI->CallVoidMethod(obj_OptSolverCallbacks, mid_OptSolverCallbacks_addEvaluation, j_parameterValues, objValue);
	}
};

void checkStopRequested_NativeOptSolver2(double Time, long iterationCount) {
	jboolean bStopRequested = env_JNI->CallBooleanMethod(obj_OptSolverCallbacks, mid_OptSolverCallbacks_getStopRequested);
	if (bStopRequested) {
		char msg[200];
		sprintf(msg, "Solver stopped by user at time %lf, %ld timesteps", Time, iterationCount);
		throw StoppedByUserException(msg);
	}
}


JNIEXPORT jstring JNICALL Java_org_vcell_optimization_NativeOptSolver_nativeSolve_1CFSQP(
	JNIEnv *jniEnv, jobject, jstring j_OptProblemDescriptionXMLString, jobject object_optSolverCallbacks)
{
	try {
		// assign global variables.
std::cout << "in dll 1" << std::endl;
std::cout.flush();
		env_JNI = jniEnv;		
		obj_OptSolverCallbacks = object_optSolverCallbacks;
		jclass class_OptSolverCallbacks = jniEnv->GetObjectClass(object_optSolverCallbacks);
		mid_OptSolverCallbacks_getStopRequested = jniEnv->GetMethodID(class_OptSolverCallbacks, "getStopRequested", "()Z");
		mid_OptSolverCallbacks_addEvaluation = jniEnv->GetMethodID(class_OptSolverCallbacks, "addEvaluation", "([DD)V");

std::cout << "in dll 2" << std::endl;
std::cout.flush();
		// Input string
		char* c_optProblemDescriptionXMLString = (char*)jniEnv->GetStringUTFChars(j_OptProblemDescriptionXMLString, 0);

		OptXmlReader2* optXmlReader = new OptXmlReader2();
		OptProblemDescription* optProblemDescription = optXmlReader->parseOptProblemDescription(c_optProblemDescriptionXMLString);
		jsize numParameters = optProblemDescription->getParameterDescription()->getNumParameters();
std::cout << "in dll 3" << std::endl;
std::cout.flush();

		OptSolverCFSQP* cfsqpOptSolver = new OptSolverCFSQP(optProblemDescription);
		JavaOptSolverListener* listener = new JavaOptSolverListener();
		cfsqpOptSolver->addListener(listener);
		OptResultSet* optResultSet = 0;

std::cout << "in dll 4" << std::endl;
std::cout.flush();

		try {
			optResultSet = cfsqpOptSolver->solve();
std::cout << "in dll 5" << std::endl;
std::cout.flush();
		} catch (StoppedByUserException& ex) {
			optResultSet = new OptResultSet();
			optResultSet->status = stoppedByUser;			
			optResultSet->statusMessage = ex.getMessage();
			for (int i=0;i<numParameters;i++){
				optResultSet->paramNames.push_back(optProblemDescription->getParameterDescription()->getParameterName(i));
				optResultSet->paramValues.push_back(optProblemDescription->getObjectiveFunction()->getBestParameterValues()[i]);
			}
			optResultSet->objectiveFunctionValue = optProblemDescription->getObjectiveFunction()->getBestObjectiveFunctionValue();
			optResultSet->numObjFuncEvals = optProblemDescription->getObjectiveFunction()->getNumObjFuncEvals();
		}
std::cout << "in dll 6" << std::endl;
std::cout.flush();

		cout << "----------Final Parameters--------------" << endl;
		optResultSet->show();
		cout << "----------------------------------------" << endl;
std::cout << "in dll 7" << std::endl;
std::cout.flush();

		/*
		 * Populate OptimizationResultSet
		*/
		OptXmlWriter2 optXmlWriter;
		TiXmlElement* optSolverResultSetXMLNode = optXmlWriter.getXML(optResultSet);
		TiXmlPrinter printer;
		optSolverResultSetXMLNode->Accept(&printer);
		printer.CStr();
		jclass class_String = jniEnv->FindClass("java/lang/String");
		jstring j_optSolverResultSetXML = jniEnv->NewStringUTF(printer.CStr());

std::cout << "in dll 8" << std::endl;
std::cout.flush();
		/* 
		 * release memory
	    */
		
		// input string
		//jniEnv->ReleaseStringUTFChars(j_OptProblemDescriptionXMLString, c_optProblemDescriptionXMLString);	

		//delete listener;
		//delete optSolverResultSet;
		delete cfsqpOptSolver;
std::cout << "in dll 9" << std::endl;
std::cout.flush();
		
		return j_optSolverResultSetXML;
	} catch (Exception& ex) {
		jclass newExcCls = jniEnv->FindClass("java/lang/Exception");
		jniEnv->ThrowNew(newExcCls, ex.getMessage().c_str());
	} catch (const char* errMsg) {
		jclass newExcCls = jniEnv->FindClass("java/lang/Exception");
		jniEnv->ThrowNew(newExcCls, errMsg);
	} catch (string& errMsg) {
		jclass newExcCls = jniEnv->FindClass("java/lang/Exception");
		jniEnv->ThrowNew(newExcCls, errMsg.c_str());
	} catch (...) {
		jclass newExcCls = jniEnv->FindClass("java/lang/Exception");
		jniEnv->ThrowNew(newExcCls, "Unknown exception");
	}

	return NULL;
}
