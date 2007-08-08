#ifndef _INCLUDE_JAVA_DECLS_H_
#define _INCLUDE_JAVA_DECLS_H_
/*
 * Copyright (c) 2003 Sonic Software Corporation. All Rights Reserved.
 *
 * This software is the confidential and proprietary information of Sonic
 * Software Corporation. ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Sonic.
 *
 * SONIC MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. SONIC SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 * CopyrightVersion 1.0
 */

#ifdef WIN32
/* disable certain compiler warnings */
#pragma warning(disable : 4290)	/* C++ Exception Spec ignored */
#pragma warning(disable : 4250)	/* method inherited by dominance of one base class */
#pragma warning(disable : 4251)	/* dll interface needed to be used by clients */
#ifdef SMQ_EXPORTS
#define SMQ_API __declspec(dllexport)
#else
#define SMQ_API __declspec(dllimport)
#endif
#ifdef SMQJ_EXPORTS
#define SMQJ_API __declspec(dllexport)
#else
#define SMQJ_API __declspec(dllimport)
#endif
#endif /* WIN32 */

#ifdef UNIX
#define SMQ_API
#define SMQJ_API
#endif

/*
 * Include the enum of all classtypes
 */
#include <classtypes.h>

/*
 * Include typedefs for the primitive Java types.
 */
#include <jtypes.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Return the length of a null-terminated string of jchars */
SMQJ_API jint jcharLen(jchar *p);

#ifdef __cplusplus
}; /* extern "C" */
#endif

#endif /* _INCLUDE_JAVA_DECLS_H_ */
