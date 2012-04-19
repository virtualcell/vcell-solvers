#ifndef _INCLUDE_JTYPES_H_
#define _INCLUDE_JTYPES_H_
/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

/* 
 * Java Primitive Type Definitions
 */
#include <sys/types.h>
/*
 * Get types from JNI which should be included before this, otherwise define our types.
 */
#ifndef _JAVASOFT_JNI_H_
typedef unsigned char jboolean;
typedef unsigned short jchar;
typedef short  jshort;
typedef float  jfloat;
typedef double  jdouble;
typedef int jint;
typedef signed char jbyte;

#if (defined(SOLARIS) || defined(SOLARIS_GCC)) && defined(_LP64)
typedef long jlong;
#elif defined(WIN32)
typedef __int64 jlong;
#else
typedef long long jlong;
#endif

#endif

/*
 * Formatting of types.
 */
#define FMT_JINT "%d"
#define FMT_JINT_HEX "%X"

#if (defined(SOLARIS) || defined(SOLARIS_GCC)) && defined(_LP64)
#define FMT_JLONG "%ld"
#define FMT_JLONG_HEX "%lX"
#elif defined(WIN32)
#define FMT_JLONG "%I64d"
#define FMT_JLONG_HEX "%I64X"
#else
#define FMT_JLONG "%lld"
#define FMT_JLONG_HEX "%llX"
#endif



#define jtrue ((jboolean )1)
#define jfalse ((jboolean)0)
#define null 0

#define MAX_JSHORT_DIGITS	 5
#define MAX_JINT_DIGITS		10
#define MAX_JLONG_DIGITS	21

#define Short_MAX_VALUE ((jshort)0x7fff)
#define Short_MIN_VALUE ((jshort)0x8000)
#define Integer_MAX_VALUE ((jint)0x7fffffff)
#define Integer_MIN_VALUE ((jint)0x80000000)


#ifdef __cplusplus
extern "C" {
#endif

/* Generic object handle for C API */
typedef void *HOBJ;

/* Signatures for callback functions */
typedef void (*pfnCExceptionListener)(HOBJ hobjException);
typedef void (*pfnCExceptionListener2)(HOBJ hobjException, HOBJ hobjUser);
typedef void (*pfnCMessageListener)(HOBJ msg);

typedef void (*pfnCConnectionStateChangeListener)(jint newState, HOBJ hobjUser);
/* ILoginSPICMap */
typedef struct _loginSPIMap
{
    HOBJ username;
    HOBJ password;
    HOBJ transformed_username;
    HOBJ transformed_password;

    jboolean (*login)(struct _loginSPIMap *map);

   	void (*setUsername)(struct _loginSPIMap *map, HOBJ username);
	HOBJ (*getUsername)(struct _loginSPIMap *map);

    void (*setPassword)(struct _loginSPIMap *map, HOBJ username);
	HOBJ (*getPassword)(struct _loginSPIMap *map);

	void (*setTransformedUsername)(struct _loginSPIMap *map, HOBJ username);
	HOBJ (*getTransformedUsername)(struct _loginSPIMap *map);

    void (*setTransformedPassword)(struct _loginSPIMap *map, HOBJ username);
	HOBJ (*getTransformedPassword)(struct _loginSPIMap *map);

} ILoginSPICMap;

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _INCLUDE_JTYPES_H_ */
