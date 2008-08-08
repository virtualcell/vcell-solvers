/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef FVUTILS_H
#define FVUTILS_H

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

extern double double_infinity;
double computeRHSscale(long length, double* rhs, string& varname);
void handlePCGExceptions(int errorCode, int additional);
bool isNAN(double number);
bool isInfinity(double number);
void validateNumber(string& variableName, int index, char* coeffName, double coeffValue);

#ifndef max
#define max(a,b)            (((a) > (b)) ? (a) : (b))
#endif

#ifndef min
#define min(a,b)            (((a) < (b)) ? (a) : (b))
#endif

#ifndef PCG_TOLERANCE
#define PCG_TOLERANCE 1.E-8
#endif

#endif
