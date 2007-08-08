#ifndef _JAVA_LANG_NULLPOINTEREXCEPTION_H_
#define _JAVA_LANG_NULLPOINTEREXCEPTION_H_
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

#include <java/lang/package_decls.h>
#include <java/lang/RuntimeException.h>

namespace java { namespace lang {

  /**
   * Thrown to indicate that a null pointer was derefenced within
   * the SonicMQ client library.
   */
  class SMQJ_API NullPointerException : public RuntimeException
  {
  public:
    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
	virtual ~NullPointerException();

    /**
     * Returns the type-code for this class' classtype.
     *
     * @return     the type-code for this class.
     */
	static int		type();

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

using java::lang::NullPointerException;
using java::lang::NullPointerExceptionRef;

#endif // _JAVA_LANG_NULLPOINTEREXCEPTION
