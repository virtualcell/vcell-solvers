#ifndef _JAVA_LANG_LONG_H_
#define _JAVA_LANG_LONG_H_
/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#include <java/lang/package_decls.h>
#include <java/lang/Number.h>

using java::lang::Object;

namespace java { namespace lang {

/**
 * Creates a new <code>Long</code> object that initially represents the 
 *   value passed in.
 *
 * @param  l   the initial value of the Long.
 * @return the newly created Long object.
 */
SMQJ_API LongRef createLong(jlong l);

class SMQJ_API Long : public Number
{
public:
    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
	virtual ~Long();

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

    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
	virtual int operator==( Object* p_) const; 

/*
 * Public methods 
 */
public:
    /**
     * Returns the value of this Long as a jbyte.
     *
     * @return  the jlong value represented by this object is converted 
	 *          to type jbyte and the result of the conversion is returned.
     */
	virtual jbyte byteValue();

    /**
     * Returns the value of this Long as a jdouble.
     *
     * @return  the jlong value represented by this object is converted 
	 *          to type jdouble and the result of the conversion is returned.
     */
	virtual jdouble doubleValue();

    /**
     * Returns the value of this Long as a jfloat.
     *
     * @return  the jlong value represented by this object is converted 
	 *          to type jfloat and the result of the conversion is returned.
     */
	virtual jfloat floatValue();

    /**
     * Returns the value of this Long as a jint.
     *
     * @return  the jlong value represented by this object is converted 
	 *          to type jint and the result of the conversion is returned.
     */
	virtual jint intValue();

    /**
     * Returns the value of this Long as a jlong.
     *
     * @return  the jlong value represented by this object.
     */
	virtual jlong longValue();

    /**
     * Returns the value of this Long as a jshort.
     *
     * @return  the jlong value represented by this object is converted 
	 *          to type jshort and the result of the conversion is returned.
     */
	virtual jshort shortValue();

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
     * Creates a string representation of the jlong argument as an 
	 *  unsigned integer in base 16.
     *
	 * @param i a jlong
     * @return  the string representation of the unsigned integer value 
	 *          represented by the argument in hexadecimal (base 16).
     */
	static  StringRef	toHexString(jlong l);

	/**
	 * Returns a jlong w/ the value represented by the specified String, 
	 *  as performed by the valueOf method of class Long.
	 *
	 * @param   s the string to be parsed.
	 * @return  the jlong value represented by the string argument.
	 */
	static	jlong		parseLong(StringRef s);

	/**
	 * Returns a new Long object initialized to the value represented by the 
	 *  specified string.
	 *
	 * @param   s the string to be parsed.
	 * @return  a newly constructed Long initialized to the value represented 
	 *          by the string argument.
	 */
	static	LongRef		valueOf(StringRef s);

};

} } // namespace java::lang

#endif // _JAVA_LANG_LONG_H_
