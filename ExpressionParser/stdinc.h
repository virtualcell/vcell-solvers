#ifndef STDINC_H
#define STDINC_H

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <iomanip>

using namespace std;

#define null 0

#if (!defined(MACOSX) && !defined(LINUX64))
#define round(a) (floor((a)+0.5))
#endif

#ifndef int64
typedef long long int64;
#endif

#ifndef boolean
typedef unsigned char boolean;
#endif

#ifndef EP_DOUBLE_INFINITY
#define EP_DOUBLE_INFINITY
#if (defined(LINUX) || defined(CYGWIN))
#include <cmath>
static double EP_double_infinity = INFINITY;
#else
#include <limits>
static double EP_double_infinity = numeric_limits<double>::infinity();
#endif
#endif

#endif
