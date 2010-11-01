#ifndef MATHUTIL_H
#define MATHUTIL_H

class MathUtil {
public:
    // Cotangent
    static double cot(double a);

    // Secant
    static double sec(double a);

    // Cosecant
    static double csc(double a);

    // Inverse functions of real numbers

    // Inverse cotangent
    static double acot(double a);

    static double acot2(double a, double b);

    // Inverse secant
    static double asec(double a);

    // Inverse cosecant
    static double acsc(double a);

    //Hyperbolic cotangent of a double number
    static double coth(double a);

    //Hyperbolic secant of a double number
    static double sech(double a);

    //Hyperbolic cosecant of a double number
    static double csch(double a);

    // Inverse hyberbolic functions of real numbers

    // Inverse hyperbolic sine of a double number
    static double asinh(double a);

    // Inverse hyperbolic cosine of a double number
    static double acosh(double a);

    // Inverse hyperbolic tangent of a double number
    static double atanh(double a);

    // Inverse hyperbolic cotangent of a double number
    static double acoth(double a);

    // Inverse hyperbolic secant of a double number
    static double asech(double a);

    // Inverse hyperbolic cosecant of a double number
    static double acsch(double a);  

    // factorial of n
    // argument and return are integer, therefore limited to 0<=n<=12
    // see below for long and double arguments
    static int factorial(int n);

    // factorial of n
    // argument and return are long, therefore limited to 0<=n<=20
    // see below for double argument
    static long factorial(long n);

    // factorial of n
    // Argument is of type double but must be, numerically, an integer
    // factorial returned as double but is, numerically, should be an integer
    // numerical rounding may makes this an approximation after n = 21
    static double factorial(double n);

    // log to base e of the factorial of n
    // log[e](factorial) returned as double
    // numerical rounding may makes this an approximation
    static double logFactorial(int n);

    // log to base e of the factorial of n
    // Argument is of type double but must be, numerically, an integer
    // log[e](factorial) returned as double
    // numerical rounding may makes this an approximation
    static double logFactorial(double n);

    // Returns the length of the hypotenuse of a and b
    // i.e. sqrt(a*a+b*b) [without unecessary overflow or underflow]
    // double version
    static double hypot(double aa, double bb);

    // Returns the length of the hypotenuse of a and b
    // i.e. sqrt(a*a+b*b) [without unecessary overflow or underflow]
    // float version
    static float hypot(float aa, float bb);

    // Maximum of a 1D array of doubles, aa
    static double maximum(double* aa, int length);

    // Maximum of a 1D array of floats, aa
    static float maximum(float* aa, int length);

    // Maximum of a 1D array of ints, aa
    static int maximum(int* aa, int length);

    // Minimum of a 1D array of doubles, aa
    static double minimum(double* aa, int length);

    // Minimum of a 1D array of floats, aa
    static float minimum(float* aa, int length);

    // Minimum of a 1D array of ints, aa
    static int minimum(int* aa, int length);

    // sort elements in an array of doubles into ascending order
    // using selection sort method
    static double* selectionSort(double* aa, int length); 

    /*      returns -1 if x < 0 else returns 1   */
    //  double version
    static double sign(double x);

    /*      returns -1 if x < 0 else returns 1   */
    //  float version
    static float sign(float x);

    /*      returns -1 if x < 0 else returns 1   */
    //  int version
    static int sign(int x);

    /*      returns -1 if x < 0 else returns 1   */
    // long version
    static long sign(long x);

	static double round(double x);

	static double double_infinity;
};
#endif
