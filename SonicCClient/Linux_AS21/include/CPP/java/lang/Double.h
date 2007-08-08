#ifndef _JAVA_LANG_DOUBLE_H_
#define _JAVA_LANG_DOUBLE_H_
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
#include <java/lang/Number.h>

using java::lang::Object;

namespace java { namespace lang {

/**
 * Creates a new <code>Double</code> object that initially represents the 
 *   value passed in.
 *
 * @param  d   the initial value of the Double.
 * @return the newly created Double object.
 */
SMQJ_API DoubleRef createDouble(jdouble d);

class SMQJ_API Double : public Number
{
public:
    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
	virtual ~Double();

/*
 * Run-time type stuff (Object overrides)
 */
public:
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
     * @return  <code>true</code> if this object is the same as or derived from
     *          the given classtype;<code>false</code> otherwise.
     */
	virtual bool	instanceof(int classtype) const;

/*
 * Public methods 
 */
public:
    /**
     * Returns the value of this Double as a jbyte (by casting to a jbyte).
     *
     * @return  the double value represented by this object is converted 
	 *          to type jbyte and the result of the conversion is returned.
     */
	virtual jbyte byteValue();

    /**
     * Returns the double value of this Double.
     *
     * @return  the double value represented by this object.
     */
	virtual jdouble doubleValue();

    /**
     * Returns the value of this Double as a jfloat (by casting to a jfloat).
     *
     * @return  the double value represented by this object is converted 
	 *          to type jfloat and the result of the conversion is returned.
     */
	virtual jfloat floatValue();

    /**
     * Returns the value of this Double as a jint (by casting to a jint).
     *
     * @return  the double value represented by this object is converted 
	 *          to type jint and the result of the conversion is returned.
     */
	virtual jint intValue();

    /**
     * Returns the value of this Double as a jlong (by casting to a jlong).
     *
     * @return  the double value represented by this object is converted 
	 *          to type jlong and the result of the conversion is returned.
     */
	virtual jlong longValue();

    /**
     * Returns the value of this Double as a jshort (by casting to a jshort).
     *
     * @return  the double value represented by this object is converted 
	 *          to type jshort and the result of the conversion is returned.
     */
	virtual jshort shortValue();

    /**
     * Returns a String representation of this Double object.
     *
     * @return  a string representation of this object.
     */
	virtual StringRef	toString();

    /**
     * Compares this object to the specified object. 
	 *
     * @param   obj the object to compare with.
     * @return  jtrue if the objects are the same; jfalse otherwise.
     */
	virtual jboolean	equals(ObjectRef obj) const;
/*
 * Static methods
 */

	/**
	 * Returns a jdouble w/ the value represented by the specified String, 
	 *  as performed by the valueOf method of class Double.
	 *
	 * @param   s the string to be parsed.
	 * @return  the jdouble value represented by the string argument.
	 */
	static	jdouble		parseDouble(StringRef s);

	/**
	 * Returns a new Double object initialized to the value represented by the 
	 *  specified string. The string s is interpreted as the representation 
	 *  of a floating-point value and a Double object representing that value 
	 *  is created and returned. If s is null, then a NullPointerException is thrown. 
	 *
	 * @param   s the string to be parsed.
	 * @return  a newly constructed Double initialized to the value represented 
	 *          by the string argument.
	 */
	static	DoubleRef	valueOf(StringRef s);

};

} } // namespace java::lang

#endif // _JAVA_LANG_LONG_H_
