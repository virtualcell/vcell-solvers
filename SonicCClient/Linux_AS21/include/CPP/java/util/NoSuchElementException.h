#ifndef _JAVA_UTIL_NOSUCHELEMENTEXCEPTION_H_
#define _JAVA_UTIL_NOSUCHELEMENTEXCEPTION_H_
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

#include <java/util/package_decls.h>
#include <java/lang/RuntimeException.h>

using java::lang::StringRef;
using java::lang::RuntimeException;

namespace java { namespace util {

  /**
   * Thrown to indicate that the code has attempted to get an object 
   * from an enumeration after the enumeration has finished. 
   */
  class SMQJ_API NoSuchElementException : public RuntimeException
  {
  public:
    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
	virtual ~NoSuchElementException();

    /**
     * Returns the type-code for this class' classtype.
     *
     * @return     the type-code for this class.
     */
	static int		type(void);

    /**
     * Returns the type-code for this object's classtype.
     *
     * @return     the type-code for this object.
     */
	virtual int		getType() const;

    /**
     * Indicates whether this object is an instance of the given classtype. 
     * An object is an instance of the given classtype if it is exactly
     * that classtype or derives from that classtype.
     *
     * @param   classtype   the type-code for the classtype with which to compare.
     * @return  <code>jtrue</code> if this object is the same as or derived from
     *          the given classtype;<code>jfalse</code> otherwise.
     */
	virtual bool	instanceof(int classtype) const;
  };

}} // namespace java::lang

#endif // _JAVA_UTIL_NOSUCHELEMENTEXCEPTION_H_
