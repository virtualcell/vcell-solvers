#include "OptSolver2.h"
#include "OptSolver2Listener.h"
#include "OptResultSet.h"
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

#include <iostream>
using std::cout;

//#define JNI_DEBUG

static JNIEnv* env_JNI = NULL;
static jobject obj_OptSolverCallbacks;
static jmethodID mid_OptSolverCallbacks_addEvaluation;
static jmethodID mid_OptSolverCallbacks_getStopRequested;

class JavaOptSolver2Listener : public OptSolver2Listener {
	void handleObjectiveFunctionEvalEvent(int numParameters, double* paramValues, double objValue) {
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
#ifdef JNI_DEBUG
		cout << "in dll 1" << endl;
		cout.flush();
#endif
		env_JNI = jniEnv;		
		obj_OptSolverCallbacks = object_optSolverCallbacks;
		jclass class_OptSolverCallbacks = jniEnv->GetObjectClass(object_optSolverCallbacks);
		mid_OptSolverCallbacks_getStopRequested = jniEnv->GetMethodID(class_OptSolverCallbacks, "getStopRequested", "()Z");
		mid_OptSolverCallbacks_addEvaluation = jniEnv->GetMethodID(class_OptSolverCallbacks, "addEvaluation", "([DD)V");
#ifdef JNI_DEBUG
		cout << "in dll 2" << endl;
		cout.flush();
#endif
		// Input string
		char* c_optProblemDescriptionXMLString = (char*)jniEnv->GetStringUTFChars(j_OptProblemDescriptionXMLString, 0);

		OptProblemDescription* optProblemDescription = OptXmlReader2::parseOptProblemDescription(c_optProblemDescriptionXMLString);
		jsize numParameters = optProblemDescription->getParameterDescription()->getNumParameters();

#ifdef JNI_DEBUG
		cout << "in dll 3" << endl;
		cout.flush();
#endif

		OptSolverCFSQP* cfsqpOptSolver = new OptSolverCFSQP(optProblemDescription);
		JavaOptSolver2Listener* listener = new JavaOptSolver2Listener();
		cfsqpOptSolver->addListener(listener);
		cfsqpOptSolver->setCheckStopRequested(checkStopRequested_NativeOptSolver2);
		OptResultSet* optResultSet = 0;

#ifdef JNI_DEBUG
		cout << "in dll 4" << endl;
		cout.flush();
#endif

		try {
			optResultSet = cfsqpOptSolver->solve();

#ifdef JNI_DEBUG
			cout << "in dll 5" << endl;
			cout.flush();
#endif
		} catch (StoppedByUserException& ex) {
			optResultSet = new OptResultSet();
			optResultSet->bestRunResultSet.status = stoppedByUser;			
			optResultSet->bestRunResultSet.statusMessage = ex.getMessage();
			for (int i=0;i<numParameters;i++){
				optResultSet->paramNames.push_back(optProblemDescription->getParameterDescription()->getParameterName(i));
				optResultSet->bestRunResultSet.paramValues.push_back(optProblemDescription->getObjectiveFunction()->getBestParameterValues()[i]);
			}
			optResultSet->bestRunResultSet.objectiveFunctionValue = optProblemDescription->getObjectiveFunction()->getBestObjectiveFunctionValue();
			optResultSet->bestRunResultSet.numObjFuncEvals = optProblemDescription->getObjectiveFunction()->getNumObjFuncEvals();
			optResultSet->vectProfileDistribution.clear();
		}
#ifdef JNI_DEBUG
		cout << "in dll 6" << endl;
		cout.flush();
#endif

		cout << "----------Final Parameters--------------" << endl;
		optResultSet->show();
		cout << "----------------------------------------" << endl;

#ifdef JNI_DEBUG
		cout << "in dll 7" << endl;
		cout.flush();
#endif
		/*
		 * Populate OptimizationResultSet
		*/
		OptXmlWriter2 optXmlWriter;
		TiXmlElement* optSolverResultSetXMLNode = optXmlWriter.getXML(optResultSet);
		TiXmlPrinter printer;
		optSolverResultSetXMLNode->Accept(&printer);
		//printer.CStr();
		jclass class_String = jniEnv->FindClass("java/lang/String");
		jstring j_optSolverResultSetXML = jniEnv->NewStringUTF(printer.CStr());

#ifdef JNI_DEBUG
		cout << "in dll 8" << endl;
		cout.flush();
#endif
		/* 
		 * release memory
	    */
		
		// input string
		//jniEnv->ReleaseStringUTFChars(j_OptProblemDescriptionXMLString, c_optProblemDescriptionXMLString);	

		//delete listener;
		//delete optSolverResultSet;
		delete cfsqpOptSolver;

#ifdef JNI_DEBUG
		cout << "in dll 9" << endl;
		cout.flush();
#endif		
		return j_optSolverResultSetXML;
	} catch (VCell::Exception& ex) {
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
