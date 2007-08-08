#ifndef _JAVA_LANG_PACKAGE_DECLS_H_
#define _JAVA_LANG_PACKAGE_DECLS_H_
/*
 * Copyright (c) 2001 Sonic Software Corporation. All Rights Reserved.
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

#include <java/lang/Object.h>
#include <Reference.h>
#include <Array.h>
#include <ctype.h>

#if defined(LINUX) || defined(SOLARIS_GCC)
#include <wctype.h>
#include <wchar.h>
#endif

//===========================================================================
// Ref class forward declarations
//===========================================================================

namespace java { namespace lang {

	// Class Object is already declared.

	class ArrayIndexOutOfBoundsExceptionRef;
	class AStringRef;
	class BooleanRef;
	class ByteRef;
	class ClassCastExceptionRef;
	class CloneNotSupportedExceptionRef;
	class DoubleRef;
	class ExceptionRef;
	class FloatRef;
	class IndexOutOfBoundsExceptionRef;
	class IntegerRef;
	class IllegalArgumentExceptionRef;
	class LongRef;
	class NullPointerExceptionRef;
	class NumberRef;
	class NumberFormatExceptionRef;
	class RuntimeExceptionRef;
	class ShortRef;
	class StringRef;
	class StringIndexOutOfBoundsExceptionRef;
	class ThrowableRef;
	class UTF8StringRef;
	class WStringRef;

} }

using namespace java::lang;

//===========================================================================
// Class and Ref class declarations
//===========================================================================

#include <java/lang/String.h> //includes AString, WString, and UTF8String
namespace java { namespace lang {
    SMQJ_DECLARE_REF(String, Object)
    SMQJ_DECLARE_REF2(AString, char, Object)
    SMQJ_DECLARE_REF2(WString, wchar_t, Object)
    SMQJ_DECLARE_REF2(UTF8String, unsigned char, Object)
} }

#include <java/lang/Throwable.h>
namespace java { namespace lang {
    SMQJ_DECLARE_REF(Throwable, Object)
} }

#include <java/lang/Exception.h>
namespace java { namespace lang {
    SMQJ_DECLARE_REF(Exception, Throwable)
} }


#include <java/lang/Boolean.h>
namespace java { namespace lang {
	SMQJ_DECLARE_REF(Boolean, Object)
} }

#include <java/lang/Number.h>
namespace java { namespace lang {
	SMQJ_DECLARE_REF(Number, Object)
} }

#include <java/lang/Byte.h>
namespace java { namespace lang {
	SMQJ_DECLARE_REF(Byte, Number)
} }

#include <java/lang/Double.h>
namespace java { namespace lang {
	SMQJ_DECLARE_REF(Double, Number)
} }

#include <java/lang/Float.h>
namespace java { namespace lang {
	SMQJ_DECLARE_REF(Float, Number)
} }

#include <java/lang/Integer.h>
namespace java { namespace lang {
	SMQJ_DECLARE_REF(Integer, Number)
} }

#include <java/lang/Long.h>
namespace java { namespace lang {
	SMQJ_DECLARE_REF(Long, Number)
} }

#include <java/lang/Short.h>
namespace java { namespace lang {
	SMQJ_DECLARE_REF(Short, Number)
} }

#include <java/lang/RuntimeException.h>
namespace java { namespace lang {
	SMQJ_DECLARE_REF(RuntimeException, Exception)
} }

#include <java/lang/ClassCastException.h>
namespace java { namespace lang {
	SMQJ_DECLARE_REF(ClassCastException, RuntimeException)
} }

#include <java/lang/IndexOutOfBoundsException.h>
namespace java { namespace lang {
	SMQJ_DECLARE_REF(IndexOutOfBoundsException, RuntimeException)
} }

#include <java/lang/ArrayIndexOutOfBoundsException.h>
namespace java { namespace lang {
	SMQJ_DECLARE_REF(ArrayIndexOutOfBoundsException, IndexOutOfBoundsException)
} }

#include <java/lang/StringIndexOutOfBoundsException.h>
namespace java { namespace lang {
	SMQJ_DECLARE_REF(StringIndexOutOfBoundsException, IndexOutOfBoundsException)
} }

#include <java/lang/CloneNotSupportedException.h>
namespace java { namespace lang {
	SMQJ_DECLARE_REF(CloneNotSupportedException, Exception)
} }

#include <java/lang/NullPointerException.h>
namespace java { namespace lang {
	SMQJ_DECLARE_REF(NullPointerException, RuntimeException)
} }

#include <java/lang/IllegalArgumentException.h>
namespace java { namespace lang {
	SMQJ_DECLARE_REF(IllegalArgumentException, RuntimeException)
} }

#include <java/lang/NumberFormatException.h>
namespace java { namespace lang {
	SMQJ_DECLARE_REF(NumberFormatException, IllegalArgumentException)
} }

#endif // _JAVA_LANG_PACKAGE_DECLS_H_
