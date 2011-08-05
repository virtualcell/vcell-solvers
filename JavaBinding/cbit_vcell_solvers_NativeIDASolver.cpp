#include "cbit_vcell_solvers_NativeIDASolver.h"
#include "VCellIDASolver.h"
#include "OdeResultSet.h"
#include "Exception.h"
using namespace VCell;

#include <sstream>
using std::istringstream;

static JNIEnv* jniEnv = NULL;
static jmethodID mid_NativeIDASolver_isStopRequested;
static jobject obj_NativeIDASolver;

static void checkStopRequested_IDA(double Time, long iterationCount) {
	jboolean bStopRequested = jniEnv->CallBooleanMethod(obj_NativeIDASolver, mid_NativeIDASolver_isStopRequested);
	if (bStopRequested) {
		char msg[200];
		sprintf(msg, "Solver stopped by user at time %lf, %ld timesteps", Time, iterationCount);
		throw Exception(msg);
	}
}

JNIEXPORT jobject JNICALL Java_cbit_vcell_solvers_NativeIDASolver_nativeSolve
(JNIEnv *env, jobject obj, jstring j_idaInput, jdoubleArray j_paramValues) {
	try {
		const char *c_idaInput = env->GetStringUTFChars(j_idaInput, 0);
		istringstream inputstream(c_idaInput);

		jniEnv = env;
		jclass cls_NativeIDASolver = env->GetObjectClass(obj);
		obj_NativeIDASolver = obj;
		mid_NativeIDASolver_isStopRequested = env->GetMethodID(cls_NativeIDASolver, "isStopRequested", "()Z");

		jdouble* c_paramValues = 0;
		if (j_paramValues != 0) {
			c_paramValues = env->GetDoubleArrayElements(j_paramValues, 0);
		}

		VCellIDASolver* idaSolver = new VCellIDASolver();
		idaSolver->readInput(inputstream);
		idaSolver->solve(c_paramValues, false, 0, checkStopRequested_IDA);
		OdeResultSet* odeResultSet = idaSolver->getResultSet();
		env->ReleaseStringUTFChars(j_idaInput, c_idaInput);
		if (j_paramValues != 0) {
			env->ReleaseDoubleArrayElements(j_paramValues, c_paramValues, 0);
		}

		int num_columns = odeResultSet->getNumColumns();
		int num_timepoints = odeResultSet->getNumRows();

		jclass clazzString = env->FindClass("java/lang/String");
		jobjectArray colNameArray = env->NewObjectArray(num_columns, clazzString, NULL);
		for (int i = 0; i < num_columns; i ++) {
			jstring name = env->NewStringUTF(odeResultSet->getColumnName(i).data());
			env->SetObjectArrayElement(colNameArray,i, name);
		}

		jclass rowColResSetClass = env->FindClass("cbit/vcell/util/RowColumnResultSet");
		jmethodID constructorID = env->GetMethodID(rowColResSetClass, "<init>", "([Ljava/lang/String;)V");

		jobject rowColResultSetObject = env->AllocObject(rowColResSetClass);
		env->CallVoidMethod(rowColResultSetObject, constructorID, colNameArray); //RowColumnResultSet::RowColumnResultsSet(String[])

		jmethodID addRowID = env->GetMethodID(rowColResSetClass, "addRow", "([D)V");

		for (int i = 0; i < num_timepoints; i ++) {
			jdoubleArray data = env->NewDoubleArray(num_columns);
			env->SetDoubleArrayRegion(data, 0, num_columns, odeResultSet->getRowData(i));
			env->CallVoidMethod(rowColResultSetObject, addRowID, data); //RowColumnResultSet::addRow(double[])
		}
		
		delete idaSolver;
		return rowColResultSetObject;
	} catch (Exception& ex) {
		jclass newExcCls = env->FindClass("java/lang/Exception");
		env->ThrowNew(newExcCls, ex.getMessage().c_str());
	} catch (const char* ex) {
		jclass newExcCls = env->FindClass("java/lang/Exception");
		env->ThrowNew(newExcCls, ex);
	} catch (...) {
		jclass newExcCls = env->FindClass("java/lang/Exception");
		env->ThrowNew(newExcCls, "unknown exception");
	}
	return 0;
}
