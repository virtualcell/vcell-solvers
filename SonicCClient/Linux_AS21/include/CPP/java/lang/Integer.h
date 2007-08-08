#ifndef _JAVA_LANG_INTEGER_H_
#define _JAVA_LANG_INTEGER_H_
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
 * Creates a new <code>Integer</code> object that initially represents the 
 *   value passed in.
 *
 * @param  i   the initial value of the Integer.
 * @return the newly created Integer object.
 */
SMQJ_API IntegerRef createInteger(jint i);

class SMQJ_API Integer : public Number
{
public:
    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
	virtual ~Integer();

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
     * Returns the value of this Integer as a jbyte.
     *
     * @return  the jint value represented by this object is converted 
	 *          to type jbyte and the result of the conversion is returned.
     */
	virtual jbyte byteValue();

    /**
     * Returns the value of this Integer as a jdouble.
     *
     * @return  the jint value represented by this object is converted 
	 *          to type jdouble and the result of the conversion is returned.
     */
	virtual jdouble doubleValue();

    /**
     * Returns the value of this Integer as a jfloat.
     *
     * @return  the jint value represented by this object is converted 
	 *          to type jfloat and the result of the conversion is returned.
     */
	virtual jfloat floatValue();

    /**
     * Returns the value of this Integer as a jint
     *
     * @return  the jint value represented by this object.
     */
	virtual jint intValue();

    /**
     * Returns the value of this Integer as a jlong.
     *
     * @return  the jint value represented by this object is converted 
	 *          to type jlong and the result of the conversion is returned.
     */
	virtual jlong longValue();

    /**
     * Returns the value of this Integer as a jshort.
     *
     * @return  the jint value represented by this object is converted 
	 *          to type jshort and the result of the conversion is returned.
     */
	virtual jshort shortValue();

    /**
     * Creates a string representation of the integer argument as an 
	 *  unsigned integer in base 16.
     *
	 * @param i an integer
     * @return  the string representation of the unsigned integer value 
	 *          represented by the argument in hexadecimal (base 16).
     */
	static  StringRef	toHexString(jint i);

    /**
     * Returns a String representation of this Integer object.
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
	 * Returns a jint w/ the value represented by the specified String, 
	 *  as performed by the valueOf method of class Integer.
	 *
	 * @param   s the string to be parsed.
	 * @return  the jint value represented by the string argument.
	 */
	static	jint			parseInt(StringRef s);

	/**
	 * Returns a new Integer object initialized to the value represented by the 
	 *  specified string.
	 *
	 * @param   s the string to be parsed.
	 * @return  a newly constructed Integer initialized to the value represented 
	 *          by the string argument.
	 */
	static	IntegerRef		valueOf(StringRef s);

/*
 * Static data
 */
	/**
	 * The maximum value an Integer can have.
	 */
	static const jint MAX_VALUE;

	/**
	 * The minimum value an Integer can have.
	 */
	static const jint MIN_VALUE;

};

} } // namespace java::lang

#endif // _JAVA_LANG_INTEGER_H_
