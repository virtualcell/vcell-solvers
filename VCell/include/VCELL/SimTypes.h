/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef SIMTYPES_H
#define SIMTYPES_H

struct DoubleVector3;

#if (defined(IRIX) || defined(CXX) || defined(LINUX))
#define UNIX_TIMER
#endif

typedef int int32;
typedef unsigned int uint32;

#ifdef VCELL_DEBUG
#if ( defined(WIN32) || defined(WIN64) )
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
typedef IntVector3    MeshCoord;

typedef long          MeshIndex;
typedef unsigned char FeatureHandle;
//typedef unsigned char ContourHandle;

typedef enum {
	MATRIX_GENERAL=0,
	MATRIX_SYMMETRIC=1,
} MatrixSymmFlag;
#endif
