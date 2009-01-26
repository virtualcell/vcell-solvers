/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef FVUTILS_H
#define FVUTILS_H

extern "C"
{
	//SUBROUTINE PCILU(ICODE,N,IJA,A,W,ISP,RSP)

#if ( defined(WIN32) || defined(WIN64) )
#ifdef WIN32
	#define PCGWRAPPER pcgwrapper
	#define PCILU pcilu
	#define DSCAL dscal
#endif
	void PCGWRAPPER(long *, long *, int *, int32 *, double *, double *, double *, double *, int *, double *, double *, double *, double*);	
	void PCILU(int *, long *, int32 *, double *, double *, double *, double *);
	void DSCAL(int *, double *, double *, int *); 
#else
	#define PCGWRAPPER pcgwrapper_
	#define PCILU pcilu_
	#define DSCAL dscal_
	extern void PCGWRAPPER(...);
	extern void PCILU(...);
	extern void DSCAL(...);
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
