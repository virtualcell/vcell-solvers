#include "cbit_vcell_opt_solvers_NativeMultiShootingOptSolver.h"
#include "OptSolver.h"
#include "OdeMultiShootingOptJob.h"
#include "OdeResultSet.h"
#include <Exception.h>

JNIEnv* env_JNI = NULL;
jmethodID mid_NativeMultiShootingOptSolver_isStopRequested;
jobject obj_NativeMultiShootingOptSolver;
jmethodID mid_OptSolverCallbacks_addEvaluation;
jobject obj_OptSolverCallbacks;

#define OPTIMIZATION_STATUS_STOPPED_BY_USER 11

class JavaOptSolverListener : public OptSolverListener {
	virtual void handleObjectiveFunctionEvalEvent(int numParameters, double* paramValues, double objValue) {
		jdoubleArray j_parameterValues = env_JNI->NewDoubleArray(numParameters);
		env_JNI->SetDoubleArrayRegion(j_parameterValues, 0, numParameters, paramValues);
		env_JNI->CallVoidMethod(obj_OptSolverCallbacks, mid_OptSolverCallbacks_addEvaluation, j_parameterValues, objValue);
	}
};

void checkStopRequested(double Time, long iterationCount) {
	jboolean bStopRequested = env_JNI->CallBooleanMethod(obj_NativeMultiShootingOptSolver, mid_NativeMultiShootingOptSolver_isStopRequested);
	if (bStopRequested) {
		char msg[200];
		sprintf(msg, "Solver stopped by user at time %lf, %ld timesteps", Time, iterationCount);
		throw Exception(msg);
	}
}

/*
 * Class:     cbit_vcell_opt_solvers_NativeMultiShootingOptSolver
 * Method:    nativeSolve
 * Signature: ([Ljava/lang/String;[D[D[DIIII[Ljava/lang/String;[Ljava/lang/String;[D[[DLjava/lang/String;DLcbit/vcell/opt/solvers/OptSolverCallbacks;)Lcbit/vcell/opt/OptimizationResultSet;
 */
JNIEXPORT jobject JNICALL Java_cbit_vcell_opt_solvers_NativeMultiShootingOptSolver_nativeSolve
(JNIEnv *jniEnv, jobject objNativeMultiShootingOptSolver, 
	jobjectArray j_parameterNames, jdoubleArray j_LB, jdoubleArray j_UB, jdoubleArray j_initGuess, 
	jint numNonLinearInequality, jint numLinearInequality, jint numNonLinearEquality, jint numLinearEquality, jobjectArray j_constraintExpressions, 
	jobjectArray j_refColumnNames, jdoubleArray j_refColumnWeights, jobjectArray j_refData, 
	jstring j_idaInputString, jdouble maxTimeStep, jobject object_optSolverCallbacks ) {
	try {
		// assign global variables.
		env_JNI = jniEnv;
		obj_NativeMultiShootingOptSolver = objNativeMultiShootingOptSolver;
		jclass cls_NativeMultiShootingOptSolver = jniEnv->GetObjectClass(obj_NativeMultiShootingOptSolver);
		mid_NativeMultiShootingOptSolver_isStopRequested = jniEnv->GetMethodID(cls_NativeMultiShootingOptSolver, "isStopRequested", "()Z");
		obj_OptSolverCallbacks = object_optSolverCallbacks;
		jclass class_OptSolverCallbacks = jniEnv->GetObjectClass(object_optSolverCallbacks);
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
		// Reference Data - weights
		jdouble* c_refColumnWeights = jniEnv->GetDoubleArrayElements(j_refColumnWeights, 0);
		referenceData->setColumnWeights(c_refColumnWeights);

		// Reference Data - row data
		int numRefRows = jniEnv->GetArrayLength(j_refData);
		for (int i = 0; i < numRefRows; i ++) {
			jdoubleArray j_rowdata = (jdoubleArray)jniEnv->GetObjectArrayElement(j_refData, i);
			referenceData->addRow(jniEnv->GetDoubleArrayElements(j_rowdata, 0));
		}

		// Input string
		char* c_idaInputString = (char*)jniEnv->GetStringUTFChars(j_idaInputString, 0);
		cout << c_idaInputString << endl;

		// Solve
		OdeMultiShootingOptJob* odeMultiShootingOptJob = new OdeMultiShootingOptJob(numParameters, c_parameterNames, c_LB, c_UB, c_initGuess, 
			numNonLinearInequality, numLinearInequality, numNonLinearEquality, numLinearEquality, c_constraintExpressions, 
			referenceData, c_idaInputString, maxTimeStep);

		
		CFSQPOptSolver* cfsqpOptSolver = new CFSQPOptSolver(odeMultiShootingOptJob);
		JavaOptSolverListener* listener = new JavaOptSolverListener();
		cfsqpOptSolver->addListener(listener);
		OptSolverResultSet* optSolverResultSet = 0;
		try {
			optSolverResultSet = cfsqpOptSolver->solve();
			cout << "----------Final Parameters--------------" << endl;
			optSolverResultSet->show();
			cout << "----------------------------------------" << endl;
		} catch (Exception& ex) {
				cout << "catch stop request" << endl;

			optSolverResultSet = new OptSolverResultSet();
			optSolverResultSet->nParams = numParameters;
			optSolverResultSet->status = OPTIMIZATION_STATUS_STOPPED_BY_USER;			
			optSolverResultSet->statusMessage = ex.getMessage();
			optSolverResultSet->params = new double[numParameters];
			memcpy(optSolverResultSet->params, odeMultiShootingOptJob->getLastParameterValues(), numParameters * sizeof(double));
			optSolverResultSet->objectiveFunctionValue = odeMultiShootingOptJob->getLastObjectiveFunctionValue();
			optSolverResultSet->numObjFuncEvals = odeMultiShootingOptJob->getNumObjFuncEvals();
		}

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
		OdeResultSet* odeResultSet = odeMultiShootingOptJob->getOdeResultSet();
		jclass clazzString = jniEnv->FindClass("java/lang/String");
		jobjectArray colNameArray = jniEnv->NewObjectArray(odeResultSet->getNumColumns(), clazzString, NULL);
		for (int i = 0; i < odeResultSet->getNumColumns(); i ++) {
			jstring name = jniEnv->NewStringUTF(odeResultSet->getColumnName(i).data());
			jniEnv->SetObjectArrayElement(colNameArray,i, name);
		}

		// solutionValues
		jclass clazzDoubleArray = jniEnv->FindClass("[D");
		jobjectArray resultArray = jniEnv->NewObjectArray(odeResultSet->getNumRows(), clazzDoubleArray, NULL);
		for (int i = 0; i < odeResultSet->getNumRows(); i ++) {
			jdoubleArray data = jniEnv->NewDoubleArray(odeResultSet->getNumColumns());
			jniEnv->SetDoubleArrayRegion(data, 0, odeResultSet->getNumColumns(), odeResultSet->getRowData(i));
			jniEnv->SetObjectArrayElement(resultArray, i, data);
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

		// Reference Data - weights
		jniEnv->ReleaseDoubleArrayElements(j_refColumnWeights, c_refColumnWeights, 0);

		// Reference Data - row data
		for (int i = 0; i < numRefRows; i ++) {
			jdoubleArray j_rowdata = (jdoubleArray)jniEnv->GetObjectArrayElement(j_refData, i);
			jniEnv->ReleaseDoubleArrayElements(j_rowdata, referenceData->getRowData(i), 0);
		}

		// input string
		jniEnv->ReleaseStringUTFChars(j_idaInputString, c_idaInputString);	

		delete referenceData;
		delete odeMultiShootingOptJob;
		delete listener;
		delete optSolverResultSet;
		delete cfsqpOptSolver;
		
		return object_OptimizationResultSet;
	} catch (Exception& ex) {
		jclass newExcCls = jniEnv->FindClass("java/lang/Exception");
		jniEnv->ThrowNew(newExcCls, ex.getMessage().data());
	} catch (char* errMsg) {
		jclass newExcCls = jniEnv->FindClass("java/lang/Exception");
		jniEnv->ThrowNew(newExcCls, errMsg);
	} catch (string& errMsg) {
		jclass newExcCls = jniEnv->FindClass("java/lang/Exception");
		jniEnv->ThrowNew(newExcCls, errMsg.data());
	} catch (...) {
		jclass newExcCls = jniEnv->FindClass("java/lang/Exception");
		jniEnv->ThrowNew(newExcCls, "Unknown exception");
	}
}