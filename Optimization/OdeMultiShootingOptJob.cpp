#include "OdeMultiShootingOptJob.h"
#include "VCellCVodeSolver.h"
#include "OdeResultSet.h"
#include "Expression.h"
#include "Exception.h"
using namespace VCell;

#include <assert.h>
#include <float.h>
#include <math.h>
#include <iostream>
#include <sstream>
#include <string>
using namespace std;

OdeMultiShootingOptJob::OdeMultiShootingOptJob(int arg_numParameters, char** arg_paramNames, double* arg_LB, 
					 double* arg_UB, double* arg_initialGuess, double* arg_scaleFactors,
					 int arg_numNonLinearInequality, int arg_numLinearInequality, 
					 int arg_numNonLinearEquality, int arg_numLinearEquality, char** constraintExpressions, 
					 OdeResultSet* arg_referenceData, char** refColumnMappingExpressions, 
					 char* arg_inputChars, double arg_maxTimeStep, 
					 void (*checkStopRequested)(double, long))
: CFSQPOptJob (arg_numParameters, arg_paramNames, arg_LB, arg_UB, arg_initialGuess, arg_scaleFactors, arg_numNonLinearInequality, arg_numLinearInequality,
			   arg_numNonLinearEquality, arg_numLinearEquality, constraintExpressions, arg_referenceData, checkStopRequested) {

	maxTimeStep = arg_maxTimeStep;
	istringstream inputStream(arg_inputChars);
	cvodeSolver = new VCellCVodeSolver();
	cvodeSolver->readInput(inputStream);

	computeTimePoints();

	numVariables = cvodeSolver->getNumEquations();
	allValues = new double[1 + numParameters + numVariables];
	testResultSet = new OdeResultSet();
	for (int i = 0; i < numVariables + 1; i ++) {
		testResultSet->addColumn(cvodeSolver->getResultSet()->getColumnName(i));
	}
	for (int i = 1; i < referenceData->getNumColumns(); i ++) {
		string columnName = referenceData->getColumnName(i);
		int index = testResultSet->findColumn(columnName);
		if (index == -1) {
			testResultSet->addFunctionColumn(columnName, refColumnMappingExpressions[i-1]);
		}
	}
	testResultSet->bindFunctionExpression(cvodeSolver->getSymbolTable());

	// preallocating storage
	testResultSet->addEmptyRows((int)timePoints.size());

	computeGradObjectiveMask();

	unscaled_x = new double[numParameters + timePoints.size() * numVariables];
	memset(unscaled_x, 0, (numParameters + timePoints.size() * numVariables) * sizeof(double));

	variableScales = new double[numVariables];
	memset(variableScales, 0, numVariables * sizeof(double));

	Expression**  initialConditionExpressions = cvodeSolver->getInitialConditionExpressions();
	for (int i = 0; i < numVariables; i ++) {	
		double varInitGuess = initialConditionExpressions[i]->evaluateVector((double*)initGuess); // at t=0
		if (abs(varInitGuess) < 1e-10) {
			variableScales[i] = 1.0;
		} else {
			variableScales[i] = abs(varInitGuess);
		}
	}
}

OdeMultiShootingOptJob::~OdeMultiShootingOptJob(){

	timePoints.clear();
	delete testResultSet;
	delete cvodeSolver;
	delete[] allValues;
	delete[] gradObjectiveMask;
	delete[] variableScales;
}

int OdeMultiShootingOptJob::getNumParameters() { 
	return numParameters + (int)timePoints.size() * numVariables; 
}

int OdeMultiShootingOptJob::getNumNonlinearEquality() { 
	return numNonLinearEquality + ((int)timePoints.size() - 1) * numVariables; 
}

void OdeMultiShootingOptJob::getLimits(double* bl, double* bu) {
	CFSQPOptJob::getLimits(bl, bu);

	Expression**  initialConditionExpressions = cvodeSolver->getInitialConditionExpressions();
	for (int i = 0; i < numVariables; i ++) {
		// if initial conditions are fixed, enforce with bounds equal to that value				
		try {
			// constant
			double initValue = initialConditionExpressions[i]->evaluateConstant();			
			bl[i + numParameters] = initValue;
			bu[i + numParameters] = initValue;
		} catch (Exception ex) {
			// not a constant, must be one-symbol expression
			vector<string> symbols;
			initialConditionExpressions[i]->getSymbols(symbols);
			if (symbols.size() != 1) {
				throw Exception(string("Initial condition [") +  initialConditionExpressions[i]->infix() + "] is invalid, expecting one symbol only!");
			}
			try {
				bl[i + numParameters] = initialConditionExpressions[i]->evaluateVector((double*)LB);
			} catch (...) {
				bl[i + numParameters] = -(DBL_MAX/2);
			}
			try {
				bu[i + numParameters] = initialConditionExpressions[i]->evaluateVector((double*)UB);
			} catch (...) {
				bu[i + numParameters] = DBL_MAX;
			}
		}
	}	
	// only scale bounds for variables at time 0
	for (int i = 0; i < numVariables; i ++) {
		int index  = numParameters + i;
		if (bl[index] > -DBL_MAX/2 && bl[index] < DBL_MAX) {
			bl[index]  /= variableScales[i];
		}
		if (bu[index] > -DBL_MAX/2 && bu[index] < DBL_MAX) {
			bu[index]  /= variableScales[i];
		}
	}
	for (int i = numParameters + numVariables; i < numParameters + (int)timePoints.size() * numVariables; i ++) {
		bl[i] = -(DBL_MAX/2);
		bu[i] = DBL_MAX;
	}
}

void OdeMultiShootingOptJob::getInitialGuess(double* x) {
	memcpy(x, initGuess, numParameters * sizeof(double));
	Expression**  initialConditionExpressions = cvodeSolver->getInitialConditionExpressions();
	for (int i = 0; i < numVariables; i ++) {		
		x[i + numParameters] = initialConditionExpressions[i]->evaluateVector((double*)initGuess); // at t=0
		string varname = testResultSet->getColumnName(i + 1); // t is the first column
		int refIndex = referenceData->findColumn(varname);
		if (refIndex != -1) {
			int startingIndex = 0;
			double prevRefTime = 0;
			double prevRefValue = x[i + numParameters];
			// if reference data starts from time 0, skip the first point.
			if (referenceData->getRowData(0)[0] == 0) {
				startingIndex = 1;
			}
			int testRowIndex = 1;	// we interpolate at all t>0 based on reference data
			for (int j = startingIndex; j < referenceData->getNumRows(); j ++) {
				double* refRowData = referenceData->getRowData(j);			
				double currRefTime = refRowData[0];
				double currRefValue = refRowData[refIndex];
				if (testRowIndex < (int)timePoints.size() && timePoints[testRowIndex] <= currRefTime) {					
					while (testRowIndex < (int)timePoints.size() && timePoints[testRowIndex] <= currRefTime) {
						x[numParameters + testRowIndex * numVariables + i] = prevRefValue + 
							(currRefValue - prevRefValue) * (timePoints[testRowIndex] - prevRefTime) / (currRefTime - prevRefTime);
						testRowIndex ++;
					}					
				}
				prevRefTime = currRefTime;
				prevRefValue = currRefValue;
			}
		} else {	
			// for variable i, set the rest of the time points to be the same as t=0
			for (int j = 1; j < (int)timePoints.size(); j ++) {
				x[numParameters + j * numVariables + i] = x[numParameters + i];
			}
		}
	}	
	scaleX(x, x);
}

void OdeMultiShootingOptJob::scaleX(const double* unscaled_x, double* scaled_x) {
	CFSQPOptJob::scaleX(unscaled_x, scaled_x);
	for (int i = 0; i < (int)timePoints.size(); i ++) {
		for (int j = 0; j < numVariables; j ++) {
			int index = numParameters + i * numVariables + j;
			scaled_x[index]  = unscaled_x[index] / variableScales[j];			
		}
	}
}

void OdeMultiShootingOptJob::unscaleX(const double* scaled_x, double* unscaled_x) {
	CFSQPOptJob::unscaleX(scaled_x, unscaled_x);
	for (int i = 0; i < (int)timePoints.size(); i ++) {
		for (int j = 0; j < numVariables; j ++) {
			int index = numParameters + i * numVariables + j;
			unscaled_x[index] = scaled_x[index] * variableScales[j];
		}
	}
}

void OdeMultiShootingOptJob::objective(int nparams, double* x, double* f) {	
	if (fn_checkStopRequested != 0) {
		fn_checkStopRequested(-1, numObjFuncEvals);
	}

	numObjFuncEvals ++;

	unscaleX(x, unscaled_x);
	for (int i = 0; i < (int)timePoints.size(); i ++) {
		double* data = testResultSet->getRowData(i);
		data[0] = timePoints[i];
		memcpy(data + 1, unscaled_x + numParameters + i * numVariables, numVariables * sizeof(double));
	}

	*f = computeL2error(unscaled_x);
	//if (bestObjectiveFunctionValue > *f) {
		bestObjectiveFunctionValue = *f;
		memcpy(bestParamterValues, unscaled_x, numParameters * sizeof(double));
		testResultSet->copyInto(bestResultSet);
	//}
}

void OdeMultiShootingOptJob::computeGradObjectiveMask() {
	gradObjectiveMask = new int[numParameters + numVariables * timePoints.size()];
	memset(gradObjectiveMask, 0 , (numParameters + numVariables * timePoints.size()) * sizeof(int));

	for (int i = 0; i < numParameters; i ++) {
		gradObjectiveMask[i] = 1;
	}

	int refNumColumns = referenceData->getNumColumns();
	int refNumRows = referenceData->getNumRows();
	int testNumColumns = testResultSet->getNumColumns();

	// required symbols
	int* stateVariableMask = new int[numVariables];
	memset(stateVariableMask, 0, numVariables * sizeof(int));
	vector<string> expSymbols;
	for (int i = 1; i < refNumColumns; i ++){
		int testColumnIndex = testResultSet->findColumn(referenceData->getColumnName(i));
		assert(testColumnIndex < testNumColumns && testColumnIndex >= 1);

		Expression* exp = testResultSet->getColumnFunctionExpression(testColumnIndex);
		if (exp == 0) {
			stateVariableMask[testColumnIndex - 1] = 1; // since t is the first column in the testResultSet
		} else {
			expSymbols.clear();
			exp->getSymbols(expSymbols);
			for (int j = 0; j < (int)expSymbols.size(); j ++) {
				testColumnIndex = testResultSet->findColumn(expSymbols[j]);
				if (testColumnIndex != -1 ) {
					stateVariableMask[testColumnIndex - 1] = 1;
				}
			}
		}
	}	
	
	int testTimeIndex = 0;	
	for (int i = 0; i < refNumRows; i ++) {				
		double refTime = referenceData->getRowData(i)[0];
		
		while (timePoints[testTimeIndex] < refTime) {	
			testTimeIndex ++;
		}
		memcpy(gradObjectiveMask + numParameters + testTimeIndex * numVariables, stateVariableMask, numVariables * sizeof(int));		
	}

	delete[] stateVariableMask;
}

// j is the index of the objective function, always 1 in our case
void OdeMultiShootingOptJob::gradObjective(int nparam, int j, double *x, double *gradfj, void (* obj)(int,int,double*,double*,void*), void *cd){  
	if (fn_checkStopRequested != 0) {
		fn_checkStopRequested(-1, numObjFuncEvals);
	}

	//CFSQPOptJob::gradObjective(nparam, j, x, gradfj, obj, cd);
   double xi;
   double delta;
   double udelta = 0.0;

   double nomObjectiveFunctionValue;
   (*obj)(nparam, j, x, &nomObjectiveFunctionValue, cd);

   for (int i = 0; i < nparam; i ++) {
	   if (gradObjectiveMask[i]) {   
			xi = x[i];
			delta = max(udelta, 2e-8 * max(1.e0,fabs(xi)));
			if (xi < 0.e0) {
				delta = -delta;
			}
			x[i] = xi + delta;
			(*obj)(nparam,j,x,&gradfj[i],cd);
			gradfj[i]=(gradfj[i] - nomObjectiveFunctionValue)/delta;
			x[i]=xi;
	   } else {
		   gradfj[i] = 0;
	   }
   }	
}

/* 
 * for parameter vector
 *		0 ~ NPARAM-1, regular paramters;
 *		NPARAM ~ NEQ * NTIMEPOINTS - 1, state variables for all the times such that x[i][j]
 *         p0, p1, ..., p[NPARAM-1], A0, B0, ..., A1, B1, ..., ..., A[NTIMEPOINTS-1], B[NTIMEPOINTS-1]
 * 
 * for constraints vector
 *      NonLinearInequality, 
 *		LinearInequality, 
 *		NonLinearEquality,
 *		constraint_A1(A1 - A0 - (RHS_A0(t0,A0,B0, p) + RHS_A1(t1,A1,B1, p))/2 * deltaT), 
 *		constraint_B1(B1 - B0 - (RHS_B0(t0,A0,B0, p) + RHS_B1(t1,A1,B1, p))/2 * deltaT),
 *		constraint_A2(A2 - A1 - (RHS_A1(t1,A1,B1, p) + RHS_A2(t2,A2,B2, p))/2 * deltaT),
 *		constraint_B2(B2 - B1 - (RHS_B1(t1,A1,B1, p) + RHS_B2(t2,A2,B2, p))/2 * deltaT),
 *		...
 *		LinearEquality
 *
 * for evaluating RHS
 *		0: t, 
 *		1 ~ NEQ: A, B, C, ..., variable values at t
 *      NEQ+1 ~ NEQ+NPARAM, p0, p1, p2, parameter values
*/						
void OdeMultiShootingOptJob::constraints(int nparams, int j, double* x, double* gj) { // j starts from 1
	if (fn_checkStopRequested != 0) {
		fn_checkStopRequested(-1, numObjFuncEvals);
	}

	unscaleX(x, unscaled_x);

	int numConstraintsLow = numNonLinearInequality + numLinearInequality + numNonLinearEquality;
	int numShootingConstraints = ((int)timePoints.size() - 1) * numVariables; //excluding time 0
	if (j <= numConstraintsLow) {
		*gj = constraintList.at(j - 1)->evaluate(x);
	} else if (j <= numConstraintsLow + numShootingConstraints) {
		int timeIndex = (j - 1 - numConstraintsLow) / numVariables;
		int equationIndex = (j - 1 - numConstraintsLow) % numVariables;

		// compute RHS_A0(t0,A0,B0, p) 
		double t0 = timePoints[timeIndex];
		allValues[0] = t0;
		memcpy(allValues + 1, unscaled_x + numParameters + timeIndex * numVariables, numVariables * sizeof(double));
		memcpy(allValues + 1 + numVariables, unscaled_x, numParameters * sizeof(double));
		double A0 = allValues[1 + equationIndex];
		double f0 = cvodeSolver->RHS(allValues, equationIndex);

		// compute RHS_A1(t1,A1,B1, p)
		double t1 = timePoints[timeIndex + 1];
		allValues[0] = t1;
		memcpy(allValues + 1, unscaled_x + numParameters + (timeIndex + 1) * numVariables, numVariables * sizeof(double));
		memcpy(allValues + 1 + numVariables, unscaled_x, numParameters * sizeof(double));
		double A1 = allValues[1 + equationIndex];
		double f1 = cvodeSolver->RHS(allValues, equationIndex);		

		// trapezoidal rule
		// compute constraint_A1(A1 - A0 - (RHS_A0(t0,A0,B0, p) + RHS_A1(t1,A1,B1, p))/2 * deltaT)
		*gj = A1 - A0 - (f0 + f1) * 0.5 * (t1 - t0);

		/*
		// higher order
		if (timeIndex + 2 >= timePoints.size()) {
			// trapezoidal rule
			// compute constraint_A1(A1 - A0 - (RHS_A0(t0,A0,B0, p) + RHS_A1(t1,A1,B1, p))/2 * deltaT)
			*gj = A1 - A0 - (f0 + f1) * 0.5 * (t1 - t0);
		} else {
			// compute RHS_A2(t2,A2,B2, p)
			double t2 = timePoints[timeIndex + 2];
			allValues[0] = t2;
			memcpy(allValues + 1, x + numParameters + (timeIndex + 2) * numVariables, numVariables * sizeof(double));
			memcpy(allValues + 1 + numVariables, x, numParameters * sizeof(double));
			double A2 = allValues[1 + equationIndex];
			double f2 = idaSolver->RHS(allValues, equationIndex);		

			double alpha=((f2 - f1) * pow((t0 - t1), 2) - (f0 - f1) * pow((t2 - t1), 2)) / (2 * (t2 - t1) * (t0 - t1) * (t0 - t2));

			double beta =((f2 - f1) * (t0 - t1) - (f0 - f1) * (t2 - t1)) /(3 * (t2 - t1) * (t0 - t1) * (t2 - t0));

			double I0 = f1 * (t0 - t1) + alpha * pow((t0 - t1), 2) + beta * pow((t0 - t1), 3);
			double I2 = f1 * (t2 - t1) + alpha * pow((t2 - t1), 2) + beta * pow((t2 - t1), 3);

			*gj =  (A2 + A0)/2 - (I2 + I0)/2 - A1; 
		}
		*/
	} else {
		*gj = constraintList.at(j - numShootingConstraints - 1)->evaluate(unscaled_x);
	}
}

double OdeMultiShootingOptJob::computeL2error(double* paramValues) {

	double L2Error = 0.0;

	int refNumColumns = referenceData->getNumColumns();
	int refNumRows = referenceData->getNumRows();
	int testNumColumns = testResultSet->getNumColumns();
	int numFunctionColumns = testResultSet->getNumFunctionColumns();
	double* values = 0; // should contain t and state variables and parameters
	if (numFunctionColumns != 0) {
		values = new double[1 + numVariables + numParameters];
		memcpy(values + 1 + numVariables, paramValues, numParameters * sizeof(double));
	}

	int testRowIndex = 0;
	for (int i = 0; i < refNumRows; i ++) {
		double* refRowData = referenceData->getRowData(i);
		double refTime = refRowData[0];
		while (timePoints[testRowIndex] < refTime) {
			testRowIndex ++;
		}
		assert (testRowIndex < timePoints.size());

		double* pvar = testResultSet->getRowData(testRowIndex);

		if (numFunctionColumns != 0) {
			memcpy(values, pvar, (1 + numVariables) * sizeof(double));
		}

		for (int j = 0; j < refNumColumns; j ++){
			int testColumnIndex = testResultSet->findColumn(referenceData->getColumnName(j));
			assert(testColumnIndex < testNumColumns);
			
			double weight = referenceData->getColumnWeight(j);
			Expression* exp = testResultSet->getColumnFunctionExpression(testColumnIndex);
			double testValue = 0.0;
			if (exp == 0) {
				testValue = pvar[testColumnIndex];
			} else {
				testValue = exp->evaluateVector(values);
			}			
			double diff = refRowData[j] - testValue;
			L2Error += weight * diff * diff;
		}
	}

	delete[] values;
	
	return L2Error;
}

void OdeMultiShootingOptJob::computeTimePoints() {
	int timeIndex = referenceData->findColumn("t");
	double* refTimes = new double[referenceData->getNumRows()];
	referenceData->getColumnData(timeIndex, numParameters, 0, refTimes);
	computeTimePoints(timePoints, refTimes, referenceData->getNumRows(), maxTimeStep);
	delete[] refTimes;
}

void OdeMultiShootingOptJob::computeTimePoints(vector<double>& timePoints, double* refTimes, int numRefTimes, double maxTimeStep) {
	// add initial time
	timePoints.push_back(0.0);
	for (int i = 0; i < numRefTimes; i ++) {
		double currTime = timePoints.at(timePoints.size() - 1);
		if ( currTime >= refTimes[i]) {
			continue;
		}
		double timediff = refTimes[i] - currTime;
		int numIntervals = (int)ceil(timediff / maxTimeStep);
		for (int j = 1; j < numIntervals; j ++) {
			double nextTime = currTime + j * timediff / numIntervals;
			timePoints.push_back(nextTime);
		}
		timePoints.push_back(refTimes[i]);		
	}
	for (int i = 0; i < timePoints.size() - 1; i ++) {
		if (timePoints[i + 1] <= timePoints[i]) {
			throw "Unable to compute timepoints for multiple shooting methods, time points are not in asc order";
		}
	}
}