/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <stdio.h>
#include <float.h>
#include <math.h>
#include <iostream>
#include <limits>
#include <sstream>
using namespace std;

extern "C"
{
#ifdef WIN32
	#define IDAMAX idamax
	int IDAMAX(long *, double*, int*);
#else
	#define IDAMAX idamax_
	extern int IDAMAX(...);
#endif
}

#ifdef LINUX
#include <cmath>
double double_infinity = INFINITY;
#else
#include <limits>
double double_infinity = numeric_limits<double>::infinity();
#endif

bool isNAN(double number) {
	if (number != number) {
		return true;
	}
	return false;
}

bool isInfinity(double number) {
	if (number == double_infinity || -number == double_infinity) {
		return true;
	}
	return false;
}

void validateNumber(string& variableName, int index, char* coeffName, double coeffValue) {
	if (isNAN(coeffValue)) {
		stringstream ss;
		ss << "Exception: " << coeffName << " for variable " << variableName << " is NaN at index " << index;
		throw ss.str();
	}
	if (isInfinity(coeffValue)) {
		stringstream ss;
		ss << "Exception: " << coeffName << " for variable " << variableName << " is Infinity at index " << index;
		throw ss.str();
	}
}

double computeRHSscale(long length, double* rhs, string& varname) {
	int increment = 1;
	int max_rhs_index = IDAMAX(&length, rhs, &increment);
	double RHSMagnitude = rhs[max_rhs_index - 1];
	validateNumber(varname, max_rhs_index - 1, "RHS", RHSMagnitude);
	RHSMagnitude = fabs(RHSMagnitude);
	if (RHSMagnitude == 0) {
		return 0;
	}	
	double RHSscale = 1.0/RHSMagnitude;	
	int count = 0;

	// Ensure scale doesn't approach the max representable double.
	while (RHSscale >= DBL_MAX / 100) {		
		cout << "Variable : " << varname << ", RHSscale=" << RHSscale << ", RHSMagnitue=" << RHSMagnitude << endl;
		RHSMagnitude *= 100;
		RHSscale = 1.0/RHSMagnitude;		
		count ++;
		if (count >= 100) {
			stringstream ss;
			ss << "Exception: Unable to scale RHS for variable " << varname << ", too many tries";
			throw ss.str();
		}
	} 	
	return RHSscale;
}

void throwPCGExceptions(int errorCode, int additional)
{
	switch (errorCode){
		case 1: {
			throw "linear solver error 1: maximum iterations reached without satisfying stopping criterion, try reducing time step";
		}		
		case 2: {			
			char msg[1000];
			sprintf(msg,"PCG Error 2: insufficient workspace for iterative method subroutine, additional (%ld) needed",(long)additional);
			throw msg;
		}
		case 3: {
			char msg[1000];
			sprintf(msg,"PCG Error 3: insufficient workspace to form incomplete factorization, additional (%ld) needed, AT LEAST",(long)additional);
			throw msg;
		}
		case 4: {
			char msg[1000];
			sprintf(msg,"PCG Error 4: insufficient workspace for stopping subroutine, additional (%ld) needed",(long)additional);
			throw msg;
		}		
		case 5: {
			throw "linear solver error 5: the incomplete factorization failed, zero pivot, try refining mesh";
		}
		case 8: {
			throw "linear solver error 8: the iteration is stagnant, try reducing time step";
		}		
		case 9: {
			char msg[1000];
			sprintf(msg,"PCG Error 9: insufficient workspace for special matrix form, additional (%ld) needed",(long)additional);
			throw msg;
		}
		case 10: {
			char msg[1000];
			sprintf(msg,"PCG Error 10: insufficient workspace to form reduced system, additional (%ld) needed",(long)additional);
			throw msg;
		}
		case 11: {
			throw "PCG Error 11: reduced system computation failed due to zero diagonal entry or singular diagonal block";
		}
		case 12: {
			throw "PCG Error 12: the ORTHOMIN or GCR computation failed";
		}
		case 13: {
			throw "PCG Error 13: the CG method failed because M is not positive definite";
		}
		case 15: {
			char msg[1000];
			sprintf(msg,"PCG Error 15: insufficient workspace for top-level driver, additional (%ld) needed",(long)additional);
			throw msg;
		}
		case 16: {
			throw "PCG Error 16: at least one entry of IPARM is invalid";
		}
		case 17: {
			throw "PCG Error 17: IJA has been setup incorrectly";
		}
		case 18: {
			throw "PCG Error 18: at least one entry of RPARM is invalid";
		}
		case 19: {
			throw "PCG Error 19: the matrix D2 is not block diagonal";
		}
		default: {
			char msg[1000];
			sprintf(msg,"PCG Error %ld: unknown error",(long)errorCode);
			throw msg;
		}
	}
}