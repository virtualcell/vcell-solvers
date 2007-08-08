#ifndef _JAVA_LANG_BYTE_H_
#define _JAVA_LANG_BYTE_H_
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
#include <java/lang/Object.h>
#include <java/lang/Number.h>

using java::lang::Object;

namespace java { namespace lang {

/**
 * Creates a new <code>Byte</code> object that initially represents the 
 *   value passed in.
 *
 * @param  b   the initial value of the Byte.
 * @return the newly created Byte object.
 */
SMQJ_API ByteRef createByte(jbyte b);

class SMQJ_API Byte : public Number
{
public:
    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
	virtual ~Byte();

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
     * Returns the value of this Byte as a jbyte.
     *
     * @return  the jbyte value represented by this object.
     */
	virtual jbyte byteValue();

    /**
     * Returns the value of this Byte as a jdouble.
     *
     * @return  the jbyte value represented by this object is converted 
	 *          to type jdouble and the result of the conversion is returned.
     */
	virtual jdouble doubleValue();

    /**
     * Returns the value of this Byte as a jfloat.
     *
     * @return  the jbyte value represented by this object is converted 
	 *          to type jfloat and the result of the conversion is returned.
     */
	virtual jfloat floatValue();

    /**
     * Returns the value of this Byte as a jint.
     *
     * @return  the jbyte value represented by this object is converted 
	 *          to type jint and the result of the conversion is returned.
     */
	virtual jint intValue();

    /**
     * Returns the value of this Byte as a jlong.
     *
     * @return  the jbyte value represented by this object is converted 
	 *          to type jlong and the result of the conversion is returned.
     */
	virtual jlong longValue();

    /**
     * Returns the value of this Byte as a jshort.
     *
     * @return  the jbyte value represented by this object is converted 
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
	 * Assuming the specified String represents a byte, returns a new 
	 *  Byte object initialized to that value. Throws an exception if the 
	 *  String cannot be parsed as a byte. The radix is assumed to be 10.
	 *
	 * @param   s the String containing the integer.
	 * @return  the Byte instance representing the parsed byte value
	 */
	static ByteRef		valueOf(StringRef s);

};

} } // namespace java::lang

#endif // _JAVA_LANG_BOOLEAN_H_
