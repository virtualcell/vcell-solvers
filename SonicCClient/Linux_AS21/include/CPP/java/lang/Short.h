#ifndef _JAVA_LANG_SHORT_H_
#define _JAVA_LANG_SHORT_H_
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
 * Creates a new <code>Short</code> object that initially represents the 
 *   value passed in.
 *
 * @param  s   the initial value of the Short.
 * @return the newly created Short object.
 */
SMQJ_API ShortRef createShort(jshort s);

class SMQJ_API Short : public Number
{
public:
    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
	virtual ~Short();

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
     * Returns the value of this Short as a jbyte.
     *
     * @return  the numeric value represented by this object after 
	 *          conversion to type jbyte.
     */
	virtual jbyte byteValue();

    /**
     * Returns the value of this Short as a jdouble.
     *
     * @return  the numeric value represented by this object after 
	 *          conversion to type jdouble.
     */
	virtual jdouble doubleValue();

    /**
     * Returns the value of this Short as a jfloat.
     *
     * @return  the numeric value represented by this object after 
	 *          conversion to type jfloat.
     */
	virtual jfloat floatValue();

    /**
     * Returns the value of this Short as an jint.
     *
     * @return  the numeric value represented by this object after 
	 *          conversion to type jint.
     */
	virtual jint intValue();

    /**
     * Returns the value of this Short as a jlong.
     *
     * @return  the numeric value represented by this object after 
	 *          conversion to type jlong.
     */
	virtual jlong longValue();

    /**
     * Returns the value of this Short as a jshort.
     *
     * @return  the numeric value represented by this object after 
	 *          conversion to type jshort.
     */
	virtual jshort shortValue();

    /**
     * Returns a String object representing this Short's value. 
     *
     * @return  a string representation of the object.
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
	 * Assuming the specified String represents a short, returns that 
	 *  short's value. Throws an exception if the String cannot be parsed 
	 *  as a short. The radix is assumed to be 10.
	 *
	 * @param   s the String containing the short.
	 * @return  the short value represented by the specified string
	 */
	static	jshort		parseShort(StringRef s);

	/**
	 * Assuming the specified String represents a short, returns a new Short 
	 *  object initialized to that value. Throws an exception if the String 
	 *  cannot be parsed as a short.
	 *
	 * @param   s the String containing the short.
	 * @return  Short of the value represented by the specified string in radix 10.
	 */
	static	ShortRef	valueOf(StringRef s);

/*
 * Static member variables
 */
	/**
	 * The maximum value a Short can have.
	 */
	const static jshort MAX_VALUE;

	/**
	 * The minimum value a Short can have.
	 */
	const static jshort MIN_VALUE;

};

} } // namespace java::lang

#endif // _JAVA_LANG_SHORT_H_
