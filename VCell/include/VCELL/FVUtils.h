/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef FVUTILS_H
#define FVUTILS_H

#include <VCELL/SimTypes.h>

#include <string>
using std::string;

extern "C"
{
#if ( defined(WIN32) || defined(WIN64) )
#ifdef WIN32
	#define PCGWRAPPER pcgwrapper
#endif
	void PCGWRAPPER(long *, long *, int *, int32 *, double *, double *, double *, double *, int *, double *, double *, double *, double*);	
#else
	#define PCGWRAPPER pcgwrapper_
	extern void PCGWRAPPER(...);
#endif
}

double computeRHSscale(long length, double* rhs, string& varname);
void throwPCGExceptions(int errorCode, int additional);
bool isNAN(double number);
bool isInfinity(double number);
void validateNumber(string& variableName, int index, char* coeffName, double coeffValue);
void sortColumns(int numCols, int* columnIndices, double* columnValues);
void trimString(string& str);

#ifndef SUNDIALS_PDE_SOLVER
#define SUNDIALS_PDE_SOLVER "SUNDIALS_PDE_SOLVER"
#endif

#ifndef FV_SOLVER
#define FV_SOLVER "FV_SOLVER"
#endif

#endif
