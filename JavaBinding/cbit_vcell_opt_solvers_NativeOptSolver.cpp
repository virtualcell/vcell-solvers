#include "OptSolver.h"
#include "cbit_vcell_opt_solvers_NativeOptSolver.h"
#include "OdeOptJob.h"
#include "OdeMultiShootingOptJob.h"
#include "OdeResultSet.h"
#include "CFSQPSolver.h"
#include "StoppedByUserException.h"
#include "MemoryManager.h"

#define SOLVERTYPE_CFSQP 100
#define SOLVERTYPE_MULTISHOOTING 101

static JNIEnv* env_JNI = NULL;
static jobject obj_OptSolverCallbacks;
static jmethodID mid_OptSolverCallbacks_addEvaluation;
static jmethodID mid_OptSolverCallbacks_getStopRequested;

class JavaOptSolverListener : public OptSolverListener {
	virtual void handleObjectiveFunctionEvalEvent(int numParameters, double* paramValues, double objValue) {
		jdoubleArray j_parameterValues = env_JNI->NewDoubleArray(numParameters);
		env_JNI->SetDoubleArrayRegion(j_parameterValues, 0, numParameters, paramValues);
		env_JNI->CallVoidMethod(obj_OptSolverCallbacks, mid_OptSolverCallbacks_addEvaluation, j_parameterValues, objValue);
	}
};

void checkStopRequested_NativeOptSolver(double Time, long iterationCount) {
	jboolean bStopRequested = env_JNI->CallBooleanMethod(obj_OptSolverCallbacks, mid_OptSolverCallbacks_getStopRequested);
	if (bStopRequested) {
		char msg[200];
		sprintf(msg, "Solver stopped by user at time %lf, %ld timesteps", Time, iterationCount);
		throw StoppedByUserException(msg);
	}
}


jobject nativeSolve(int solverType, JNIEnv *jniEnv, jobject objNativeOptSolver, 
	jobjectArray j_parameterNames, jdoubleArray j_LB, jdoubleArray j_UB, jdoubleArray j_initGuess, jdoubleArray j_scaleFactors, 
	jint numNonLinearInequality, jint numLinearInequality, jint numNonLinearEquality, jint numLinearEquality, jobjectArray j_constraintExpressions, 
	jobjectArray j_refColumnNames, jobjectArray j_refColumnMappingExpressions, jdoubleArray j_refColumnWeights, jobjectArray j_refData, 
	jstring j_idaInputString, jobject object_optSolverCallbacks, double maxTimeStep=0) {
	try {
		// assign global variables.
		env_JNI = jniEnv;		
		obj_OptSolverCallbacks = object_optSolverCallbacks;
		jclass class_OptSolverCallbacks = jniEnv->GetObjectClass(object_optSolverCallbacks);
		mid_OptSolverCallbacks_getStopRequested = jniEnv->GetMethodID(class_OptSolverCallbacks, "getStopRequested", "()Z");
		mid_OptSolverCallbacks_addEvaluation = jniEnv->GetMethodID(class_OptSolverCallbacks, "addEvaluation", "([DD)V");

		// Parameter Names
		int numParameters = jniEnv->GetArrayLength(j_parameterNames);
		char** c_parameterNames = new char*[numParameters];
		for (int i = 0; i < numParameters; i ++) {
			jstring j_parameterName = (jstring)jniEnv->GetObjectArrayElement(j_parameterNames, i);
			c_parameterNames[i] = (char*)jniEnv->GetStringUTFChars(j_parameterName, 0);
		}

		// LB, UB, initial Guess
		jdouble* c_initGuess = jniEnv->GetDoubleArrayElements(j_initGuess, 0);
		jdouble* c_LB = jniEnv->GetDoubleArrayElements(j_LB, 0);
		jdouble* c_UB = jniEnv->GetDoubleArrayElements(j_UB, 0);	
		jdouble* c_scaleFactors = jniEnv->GetDoubleArrayElements(j_scaleFactors, 0);

		// Constraints
		int numConstraints = jniEnv->GetArrayLength(j_constraintExpressions);
		if (numConstraints != numNonLinearInequality + numLinearInequality + numNonLinearEquality + numLinearEquality) {
			throw Exception("Number of constraints don't match the sum of numNonLinearInequality, numLinearInequality, numNonLinearEquality and numLinearEquality!");
		}
		char** c_constraintExpressions = new char*[numConstraints];
		for (int i = 0; i < numConstraints; i ++) {
			jstring j_constraintExpression = (jstring)jniEnv->GetObjectArrayElement(j_constraintExpressions, i);
			c_constraintExpressions[i] = (char*)jniEnv->GetStringUTFChars(j_constraintExpression, 0);
		}

		// Reference Data
		int numRefColumns = jniEnv->GetArrayLength(j_refColumnNames);
		OdeResultSet* referenceData = new OdeResultSet();	

		// Reference Data - column names
		char** c_refColumnNames = new char*[numRefColumns];
		for (int i = 0; i < numRefColumns; i ++) {
			jstring j_columnName = (jstring)jniEnv->GetObjectArrayElement(j_refColumnNames, i);
			c_refColumnNames[i] = (char*)jniEnv->GetStringUTFChars(j_columnName, 0);
			referenceData->addColumn(c_refColumnNames[i]);
		}
	
		// Reference Data - column expressions
		char** c_refColumnMappingExpressions = new char*[numRefColumns-1];
		for (int i = 0; i < numRefColumns-1; i ++) {
			jstring j_columnMappingExpression = (jstring)jniEnv->GetObjectArrayElement(j_refColumnMappingExpressions, i);
			c_refColumnMappingExpressions[i] = (char*)jniEnv->GetStringUTFChars(j_columnMappingExpression, 0);
		}

		// Reference Data - weights
		jdouble* c_refColumnWeights = jniEnv->GetDoubleArrayElements(j_refColumnWeights, 0);
		referenceData->setColumnWeights(c_refColumnWeights);

		// Reference Data - row data
		int numRefRows = jniEnv->GetArrayLength(j_refData);
		jdouble** j_rowDataArray = new jdouble*[numRefRows];
		for (int i = 0; i < numRefRows; i ++) {
			jdoubleArray j_rowdata = (jdoubleArray)jniEnv->GetObjectArrayElement(j_refData, i);
			j_rowDataArray[i] = jniEnv->GetDoubleArrayElements(j_rowdata, 0);
			referenceData->addRow(j_rowDataArray[i]);
		}

		// Input string
		char* c_idaInputString = (char*)jniEnv->GetStringUTFChars(j_idaInputString, 0);

		CFSQPOptJob* optJob = 0;
		if (solverType == SOLVERTYPE_CFSQP) {
			// Solve
			optJob = new OdeOptJob(numParameters, c_parameterNames, c_LB, c_UB, c_initGuess, c_scaleFactors,
				numNonLinearInequality, numLinearInequality, numNonLinearEquality, numLinearEquality, c_constraintExpressions, 
				referenceData, c_refColumnMappingExpressions, c_idaInputString, checkStopRequested_NativeOptSolver);
		} else if (solverType == SOLVERTYPE_MULTISHOOTING) {
			optJob = new OdeMultiShootingOptJob(numParameters, c_parameterNames, c_LB, c_UB, c_initGuess, c_scaleFactors,
				numNonLinearInequality, numLinearInequality, numNonLinearEquality, numLinearEquality, c_constraintExpressions, 
				referenceData, c_refColumnMappingExpressions, c_idaInputString, maxTimeStep, checkStopRequested_NativeOptSolver);
		}

		CFSQPOptSolver* cfsqpOptSolver = new CFSQPOptSolver(optJob);
		JavaOptSolverListener* listener = new JavaOptSolverListener();
		cfsqpOptSolver->addListener(listener);
		OptSolverResultSet* optSolverResultSet = 0;

		try {
			optSolverResultSet = cfsqpOptSolver->solve();
		} catch (StoppedByUserException& ex) {
			optSolverResultSet = new OptSolverResultSet();
			optSolverResultSet->nParams = numParameters;
			optSolverResultSet->status = STOPPED_BY_USER;			
			optSolverResultSet->statusMessage = ex.getMessage();
			optSolverResultSet->params = new double[numParameters];
			memcpy(optSolverResultSet->params, optJob->getBestParameterValues(), numParameters * sizeof(double));
			optSolverResultSet->objectiveFunctionValue = optJob->getBestObjectiveFunctionValue();
			optSolverResultSet->numObjFuncEvals = optJob->getNumObjFuncEvals();
		}

		cout << "----------Final Parameters--------------" << endl;
		optSolverResultSet->show();
		cout << "----------------------------------------" << endl;

		/*
		 * Populate OptimizationResultSet
		*/
		// Parameter Values;
		jdoubleArray j_parameterValues = jniEnv->NewDoubleArray(numParameters);
		jniEnv->SetDoubleArrayRegion(j_parameterValues, 0, numParameters, optSolverResultSet->params);

		// Double objectiveFunctionValue
		jclass class_Double = jniEnv->FindClass("java/lang/Double");
		jmethodID methodID_Double_constructor = jniEnv->GetMethodID(class_Double, "<init>", "(D)V");
		jobject object_Double = jniEnv->AllocObject(class_Double);
		jniEnv->CallVoidMethod(object_Double, methodID_Double_constructor, optSolverResultSet->objectiveFunctionValue);

		// solutionNames
		OdeResultSet* odeResultSet = optJob->getBestResultSet();
		jclass clazzString = jniEnv->FindClass("java/lang/String");
		jobjectArray colNameArray = NULL;
		jobjectArray resultArray = NULL;
		if (odeResultSet->getNumRows() > 0) {
			colNameArray = jniEnv->NewObjectArray(odeResultSet->getNumDataColumns(), clazzString, NULL);
			for (int i = 0; i < odeResultSet->getNumDataColumns(); i ++) {
				jstring name = jniEnv->NewStringUTF(odeResultSet->getColumnName(i).data());
				jniEnv->SetObjectArrayElement(colNameArray,i, name);
			}

			// solutionValues
			jclass clazzDoubleArray = jniEnv->FindClass("[D");
			resultArray = jniEnv->NewObjectArray(odeResultSet->getNumRows(), clazzDoubleArray, NULL);
			for (int i = 0; i < odeResultSet->getNumRows(); i ++) {
				jdoubleArray data = jniEnv->NewDoubleArray(odeResultSet->getNumDataColumns());
				jniEnv->SetDoubleArrayRegion(data, 0, odeResultSet->getNumDataColumns(), odeResultSet->getRowData(i));
				jniEnv->SetObjectArrayElement(resultArray, i, data);
			}
		}

		// OptimizationStatus
		jclass class_OptimizationStatus = jniEnv->FindClass("cbit/vcell/opt/OptimizationStatus");
		jmethodID methodID_OptimizationStatus_constructor = jniEnv->GetMethodID(class_OptimizationStatus, "<init>", "(ILjava/lang/String;)V");
		jobject object_OptimizationStatus = jniEnv->AllocObject(class_OptimizationStatus);
		jstring j_statusMessage = jniEnv->NewStringUTF(optSolverResultSet->statusMessage.data());
		jint j_status = optSolverResultSet->status;
		jniEnv->CallVoidMethod(object_OptimizationStatus, methodID_OptimizationStatus_constructor, j_status, j_statusMessage);

		// OptimizationResultSet
		jclass class_OptimizationResultSet = jniEnv->FindClass("cbit/vcell/opt/OptimizationResultSet");
		jmethodID methodID_OptimizationResultSet_constructor = jniEnv->GetMethodID(class_OptimizationResultSet, "<init>", "([Ljava/lang/String;[DLjava/lang/Double;J[Ljava/lang/String;[[DLcbit/vcell/opt/OptimizationStatus;)V");

		jobject object_OptimizationResultSet = jniEnv->AllocObject(class_OptimizationResultSet);
		jniEnv->CallVoidMethod(object_OptimizationResultSet, methodID_OptimizationResultSet_constructor, j_parameterNames, j_parameterValues,
			object_Double, (jlong)optSolverResultSet->numObjFuncEvals, colNameArray, resultArray, object_OptimizationStatus); 
		//OptimizationResultSet::OptimizationResultSet(String[] parameterNames, double parameterValues[], Double objectiveFunctionValue, long argNumObjFuncEvals, String[] solutionNames, double solutionValues[][], OptimizationStatus argStatus);

		/* 
		 * release memory
	    */
		// Parameter Names
		for (int i = 0; i < numParameters; i ++) {
			jstring j_parameterName = (jstring)jniEnv->GetObjectArrayElement(j_parameterNames, i);
			jniEnv->ReleaseStringUTFChars(j_parameterName, c_parameterNames[i]);	
		}
		delete[] c_parameterNames;

		// LB, UB, initial guess
		jniEnv->ReleaseDoubleArrayElements(j_initGuess, c_initGuess, 0);
		jniEnv->ReleaseDoubleArrayElements(j_LB, c_LB, 0);
		jniEnv->ReleaseDoubleArrayElements(j_UB, c_UB, 0);
		jniEnv->ReleaseDoubleArrayElements(j_scaleFactors, c_scaleFactors, 0);
		
		// Constraint expressions
		for (int i = 0; i < numConstraints; i ++) {
			jstring j_constraintExpression = (jstring)jniEnv->GetObjectArrayElement(j_constraintExpressions, i);
			jniEnv->ReleaseStringUTFChars(j_constraintExpression, c_constraintExpressions[i]);	
		}
		delete[] c_constraintExpressions;
		
		// Reference Data - column names
		for (int i = 0; i < numRefColumns; i ++) {
			jstring j_refColumnName = (jstring)jniEnv->GetObjectArrayElement(j_refColumnNames, i);
			jniEnv->ReleaseStringUTFChars(j_refColumnName, c_refColumnNames[i]);
		}
		delete[] c_refColumnNames;

		// Reference Data - column expressions
		for (int i = 0; i < numRefColumns-1; i ++) {
			jstring j_columnMappingExpression = (jstring)jniEnv->GetObjectArrayElement(j_refColumnNames, i);
			jniEnv->ReleaseStringUTFChars(j_columnMappingExpression, c_refColumnMappingExpressions[i]);
		}
		delete[] c_refColumnMappingExpressions;

		// Reference Data - weights
		jniEnv->ReleaseDoubleArrayElements(j_refColumnWeights, c_refColumnWeights, 0);

		// Reference Data - row data
		for (int i = 0; i < numRefRows; i ++) {
			jdoubleArray j_rowdata = (jdoubleArray)jniEnv->GetObjectArrayElement(j_refData, i);
			jniEnv->ReleaseDoubleArrayElements(j_rowdata, j_rowDataArray[i], 0);
		}
		delete[] j_rowDataArray;

		// input string
		jniEnv->ReleaseStringUTFChars(j_idaInputString, c_idaInputString);	

		delete referenceData;
		delete optJob;
		delete listener;
		delete optSolverResultSet;
		delete cfsqpOptSolver;
		
		return object_OptimizationResultSet;
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


JNIEXPORT jobject JNICALL Java_cbit_vcell_opt_solvers_NativeOptSolver_nativeSolve_1CFSQP 
(JNIEnv *jniEnv, jobject objNativeOptSolver, 
	jobjectArray j_parameterNames, jdoubleArray j_LB, jdoubleArray j_UB, jdoubleArray j_initGuess, jdoubleArray j_scaleFactors,
	jint numNonLinearInequality, jint numLinearInequality, jint numNonLinearEquality, jint numLinearEquality, jobjectArray j_constraintExpressions, 
	jobjectArray j_refColumnNames, jobjectArray j_refColumnMappingExpressions, jdoubleArray j_refColumnWeights, jobjectArray j_refData, 
	jstring j_idaInputString, jobject object_optSolverCallbacks ) {

	return nativeSolve(SOLVERTYPE_CFSQP, jniEnv, objNativeOptSolver, j_parameterNames, j_LB, j_UB, j_initGuess, j_scaleFactors,
		numNonLinearInequality, numLinearInequality, numNonLinearEquality, numLinearEquality, j_constraintExpressions, 
		j_refColumnNames, j_refColumnMappingExpressions, j_refColumnWeights, j_refData, j_idaInputString, object_optSolverCallbacks);
}

JNIEXPORT jobject JNICALL Java_cbit_vcell_opt_solvers_NativeOptSolver_nativeSolve_1MultiShooting
(JNIEnv *jniEnv, jobject objNativeOptSolver, 
	jobjectArray j_parameterNames, jdoubleArray j_LB, jdoubleArray j_UB, jdoubleArray j_initGuess, jdoubleArray j_scaleFactors, 
	jint numNonLinearInequality, jint numLinearInequality, jint numNonLinearEquality, jint numLinearEquality, jobjectArray j_constraintExpressions, 
	jobjectArray j_refColumnNames, jobjectArray j_refColumnMappingExpressions, jdoubleArray j_refColumnWeights, jobjectArray j_refData, 
	jstring j_idaInputString, jdouble maxTimeStep, jobject object_optSolverCallbacks ) {

	return nativeSolve(SOLVERTYPE_MULTISHOOTING, jniEnv, objNativeOptSolver, j_parameterNames, j_LB, j_UB, j_initGuess, j_scaleFactors,
		numNonLinearInequality, numLinearInequality, numNonLinearEquality, numLinearEquality, j_constraintExpressions, 
		j_refColumnNames, j_refColumnMappingExpressions, j_refColumnWeights, j_refData, j_idaInputString, object_optSolverCallbacks, maxTimeStep);
}
