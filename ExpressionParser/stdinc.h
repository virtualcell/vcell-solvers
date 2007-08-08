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
#define round(a) (floor((a)+0.5))

#ifndef int64
typedef long long int64;
#endif

#ifndef boolean
typedef unsigned char boolean;
#endif

#ifndef max
#define max(a,b)            (((a) > (b)) ? (a) : (b))
#endif

#ifndef min
#define min(a,b)            (((a) < (b)) ? (a) : (b))
#endif

#endif
