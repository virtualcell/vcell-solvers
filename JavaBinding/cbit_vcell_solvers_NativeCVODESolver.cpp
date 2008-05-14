#include "cbit_vcell_solvers_NativeCVODESolver.h"
#include <VCellCVodeSolver.h>
#include <OdeResultSet.h>
#include <Exception.h>
using namespace VCell;

JNIEnv* jniEnv_cvode = NULL;
jmethodID mid_NativeCVODESolver_isStopRequested;
jobject obj_NativeCVODESolver;

void checkStopRequested_CVODE(double Time, long iterationCount) {
	jboolean bStopRequested = jniEnv_cvode->CallBooleanMethod(obj_NativeCVODESolver, mid_NativeCVODESolver_isStopRequested);
	if (bStopRequested) {
		char msg[200];
		sprintf(msg, "Solver stopped by user at time %lf, %ld timesteps", Time, iterationCount);
		throw Exception(msg);
	}
}

JNIEXPORT jobject JNICALL Java_cbit_vcell_solvers_NativeCVODESolver_nativeSolve
(JNIEnv *env, jobject obj, jstring j_cvodeInput, jdoubleArray j_paramValues) {
	try {
		const char *c_cvodeInput = env->GetStringUTFChars(j_cvodeInput, 0);
		istringstream inputstream(c_cvodeInput);

		jniEnv_cvode = env;
		jclass cls_NativeCVODESolver = env->GetObjectClass(obj);
		obj_NativeCVODESolver = obj;
		mid_NativeCVODESolver_isStopRequested = env->GetMethodID(cls_NativeCVODESolver, "isStopRequested", "()Z");

		jdouble* c_paramValues = 0;
		if (j_paramValues != 0) {
			c_paramValues = env->GetDoubleArrayElements(j_paramValues, 0);
		}

		VCellCVodeSolver* cvodeSolver = new VCellCVodeSolver();
		cvodeSolver->readInput(inputstream);
		cvodeSolver->solve(c_paramValues, false, 0, checkStopRequested_CVODE);
		OdeResultSet* odeResultSet = cvodeSolver->getResultSet();
		env->ReleaseStringUTFChars(j_cvodeInput, c_cvodeInput);
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
		
		delete cvodeSolver;
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
