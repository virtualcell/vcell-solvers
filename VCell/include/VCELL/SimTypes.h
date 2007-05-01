/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef SIMTYPES_H
#define SIMTYPES_H

#include <assert.h>
#include <math.h>

#if (defined(WIN32) || defined(CXX))
#define VCELL_USES_NAMESPACES
#endif

#if (!defined(VCELL_USES_NAMESPACES) || defined(VC60))
#include <iostream.h>
#else
#include <iostream>
using namespace std;
#endif

#if (defined(IRIX) || defined(CXX) || defined(LINUX))
#define UNIX_TIMER
#endif

#ifndef SIZE64
//typedef long INT32;
typedef int INT32;
typedef long           int32;
typedef unsigned long  uint32;
#else
typedef int INT32;
typedef int            int32;
typedef unsigned int   uint32;
#endif

#ifndef M_PI
#define M_PI 3.14159265359
#endif

#ifdef VCELL_DEBUG
#ifdef WIN32
#define ASSERTION(x) \
	if (!(x)) { \
		char str[300]; \
		sprintf(str, "File '%s', Line %d : Assertion Failed...\n",__FILE__, __LINE__); \
		throw str; \
	} 
#else
#define ASSERTION(x)  assert(x)
#endif
#else 
#define ASSERTION(x) ((void *)0)
#endif

typedef enum {
	VAR_UNKNOWN =			0,
	VAR_VOLUME =			1,
	VAR_MEMBRANE =			2,
	VAR_CONTOUR =			3,
	VAR_VOLUME_REGION =		4,
	VAR_MEMBRANE_REGION=	5,
	VAR_CONTOUR_REGION =	6
} VariableType;

typedef enum {
    LOCATION_VOLUME, 
    LOCATION_MEMBRANE, 
    LOCATION_CONTOUR, 
    LOCATION_ALL
} LocationContext;

typedef enum {
	BOUNDARY_VALUE, 
	BOUNDARY_FLUX, 
	BOUNDARY_PERIODIC
} BoundaryType;

struct DoubleVector3 {
	double x;
	double y;
	double z;

	DoubleVector3() {
		x = 0.0;
		y = 0.0;
		z = 0.0;
	}

	DoubleVector3(double arg_x, double arg_y, double arg_z) {
		x = arg_x;
		y = arg_y;
		z = arg_z;
	}

	double length() {
		return sqrt(x * x + y * y + z * z);
	}

	double lengthSquare() {
		return x * x + y * y + z * z;
	}

	DoubleVector3 scale(double scaler) {
		return DoubleVector3(x * scaler, y * scaler, z * scaler);
	}

	void normalize() {
		double l = length();
		if (l > 0) {
			x /= l; 
			y /= l; 
			z /= l;
		} else {
			throw "DoubleVector3::normalize(), Can't normalize zero-length vector";
		}
	}

	double dotProduct(const DoubleVector3& wc) {
		return x * wc.x + y * wc.y + z * wc.z;
	}

	DoubleVector3 crossProduct(const DoubleVector3& wc) { // cross product
		return DoubleVector3(
			y * wc.z - z * wc.y,
			z * wc.x - x * wc.z,
            x * wc.y - y * wc.x
		);
	}

	DoubleVector3 operator-(const DoubleVector3& wc) {
		return DoubleVector3(x - wc.x, y - wc.y, z - wc.z);
	}

	void operator-=(const DoubleVector3& wc) {
		x = x - wc.x;
		y = y - wc.y;
		z = z - wc.z;
	}

	DoubleVector3 operator+(const DoubleVector3& wc) {
		return DoubleVector3(x + wc.x, y + wc.y, z + wc.z);
	}

	void operator+=(const DoubleVector3& wc) {
		x = x + wc.x;
		y = y + wc.y;
		z = z + wc.z;
	}

	friend ostream& operator<<(ostream& os, const DoubleVector3& wc) {
		os << "[" << wc.x << "," << wc.y << "," << wc.z << "]";
		return os;
	}
};
typedef struct {
	double u;
	double v;
	double w;
} LocalCoord;

typedef struct {
	int x;
	int y;
	int z;
} IntVector3;

typedef DoubleVector3 WorldCoord;
typedef unsigned char GeoDataType;

typedef IntVector3    MeshCoord;

typedef long          MeshIndex;
typedef unsigned char FeatureHandle;
typedef unsigned char ContourHandle;

typedef enum {
	MATRIX_GENERAL=0,
	MATRIX_SYMMETRIC=1,
} MatrixSymmFlag;
#endif
