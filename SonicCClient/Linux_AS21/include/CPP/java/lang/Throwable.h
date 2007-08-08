#ifndef _JAVA_LANG_THROWABLE_H_
#define _JAVA_LANG_THROWABLE_H_
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

namespace java { 
    namespace lang {

	/**
	 * The <code>Throwable</code> class is the superclass of all errors 
	 * and exceptions in the SonicMQ Client Runtime. Only objects that are 
	 * instances of this class (or of one of its subclasses) are thrown 
	 * by the runtime.
	 */
	class SMQJ_API Throwable : public Object // public java::io::Serializable
	    {
	    public:
		/**
		 * For Sonic Software Internal use only: DO NOT USE.
		 */
		virtual ~Throwable();

		/**
		 * Returns the type-code for this class' classtype.
		 *
		 * @return     the type-code for this class.
		 */
		static  int type();

		/**
		 * Returns the type-code for this object's classtype.
		 *
		 * @return     the type-code for this object.
		 */
		virtual int getType() const;

		/**
		 * Indicates whether this object is an instance of the given classtype. 
		 * An object is an instance of the given classtype if it is exactly
		 * that classtype or derives from that classtype.
		 *
		 * @param   classtype   the type-code for the classtype with which to compare.
		 * @return  <code>jtrue</code> if this object is the same as or derived from
		 *          the given classtype;<code>jfalse</code> otherwise.
		 */
		virtual bool instanceof(int classType) const;

		/**
		 * Returns the errort message string of this throwable object.
		 *
		 * @return  the error message string of this <code>Throwable</code> 
		 *          object. This may be <code>null</code>.
		 */
		virtual StringRef getMessage();

		/**
		 * Creates a localized description of this <code>Throwable</code>.
		 * Subclasses may override this method in order to produce a
		 * locale-specific message.  For subclasses that do not override this
		 * method, the default implementation returns the same result as
		 * <code>getMessage()</code>.
		 */
		virtual StringRef getLocalizedMessage();

		/**
		 * Returns a short description of this throwable object.
		 * If this <code>Throwable</code> object was 
		 * {@link #Throwable(String) created} with an error message string, 
		 * then the result is the concatenation of three strings: 
		 * <ul>
		 * <li>The name of the actual class of this object 
		 * <li>": " (a colon and a space) 
		 * <li>The result of the <code>getMessage</code> method for this object 
		 * </ul>
		 *
		 * @return  a string representation of this <code>Throwable</code>.
		 */
		virtual StringRef toString();

		/**
		 * For Sonic Software Internal use only: DO NOT USE.
		 */
		virtual void printStackTrace();

		/**
		 * For Sonic Software Internal use only: DO NOT USE.
		 */
		virtual ThrowableRef fillInStackTrace();

	    }; // class Throwable

    } // namespace lang
} // namespace java

#endif // _JAVA_LANG_THROWABLE_H_
