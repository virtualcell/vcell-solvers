#ifndef _JAVA_UTIL_PACKAGE_DECLS_H_
#define _JAVA_UTIL_PACKAGE_DECLS_H_
/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#include <java/lang/package_decls.h>

//===========================================================================
// Ref class forward declarations
//===========================================================================
namespace java { namespace util {
    class DictionaryRef;
    class EnumerationRef;
    class HashtableRef;
    class NoSuchElementExceptionRef;
}} // namespace java::util

using namespace java::util;

//===========================================================================
// Class and Ref class declarations
//===========================================================================
#include <java/util/Enumeration.h>
namespace java { namespace util {
    SMQJ_DECLARE_REF(Enumeration, Object)
}}

#include <java/util/Dictionary.h>
namespace java { namespace util {
    SMQJ_DECLARE_REF(Dictionary, Object)
}}

#include <java/util/Hashtable.h>
namespace java { namespace util {
    SMQJ_DECLARE_REF(Hashtable, Dictionary)
}}

#include <java/util/NoSuchElementException.h>
namespace java { namespace util {
    SMQJ_DECLARE_REF(NoSuchElementException, RuntimeException)
}}

#endif // _JAVA_UTIL_PACKAGE_DECLS_H_
