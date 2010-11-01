#include <math.h>
#include "MathUtil.h"

#if (defined(LINUX) || defined(CYGWIN))
#include <cmath>
static double MathUtil::double_infinity = INFINITY;
#else
#include <limits>
double MathUtil::double_infinity = std::numeric_limits<double>::infinity();
#endif

// Inverse hyperbolic cosine of a double number
double MathUtil::acosh(double a){
    return log(a+sqrt(a*a-1));
}

// Inverse cotangent
double MathUtil::acot(double a){
    return atan(1.0/a);
}

double MathUtil::acot2(double a, double b){
    return atan2(b, a);
}

// Inverse hyperbolic cotangent of a double number
double MathUtil::acoth(double a){
    return 0.5*log((a + 1.0)/(a - 1.0));
}

// Inverse cosecant
double MathUtil::acsc(double a){
    return asin(1.0/a);
}

// Inverse hyperbolic cosecant of a double number
double MathUtil::acsch(double a){
	if (a > 0) {
		return log( (1.0 + sqrt(1 + a*a)) / a );
	} else if (a < 0) {
		return log( (1.0 - sqrt(1 + a*a)) / a );
	}
} 

// Inverse secant
double MathUtil::asec(double a){
    return acos(1.0/a);
}

// Inverse hyperbolic secant of a double number
double MathUtil::asech(double a){
	return log( (1.0 + sqrt(1.0 - a*a)) / a );
}

// Inverse hyperbolic sine of a double number
double MathUtil::asinh(double a){
    return log(a+sqrt(a*a+1));
}

// Inverse hyperbolic tangent of a double number
double MathUtil::atanh(double a){
    return 0.5*(log( (1.0 + a)/(1.0 - a) ));
}

// Cotangent
double MathUtil::cot(double a){
    return 1.0/tan(a);
}

//Hyperbolic cotangent of a double number
double MathUtil::coth(double a){
    return 1.0/tanh(a);
}

// Cosecant
double MathUtil::csc(double a){
    return 1.0/sin(a);
}

//Hyperbolic cosecant of a double number
double MathUtil::csch(double a){
    return 1.0/sinh(a);
}

// Secant
double MathUtil::sec(double a){
    return 1.0/cos(a);
}

//Hyperbolic secant of a double number
double MathUtil::sech(double a){
    return 1.0/cosh(a);
}

// factorial of n
// Argument is of type double but must be, numerically, an integer
// factorial returned as double but is, numerically, should be an integer
// numerical rounding may makes this an approximation after n = 21
double MathUtil::factorial(double n){
    double f = 1.0;
    int nn = (int)n;
    for(int i = 1; i <= nn; i ++)
		f *= i;
    return f;
}

double MathUtil::round(double a) {
	return floor(a+0.5);
}