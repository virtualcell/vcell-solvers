#ifndef _JAVA_LANG_NUMBER_H_
#define _JAVA_LANG_NUMBER_H_
/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

/*
 * The abstract class Number is the superclass of classes Byte, Double, Float, Integer, Long, and Short. 
 * Subclasses of Number must provide methods to convert the represented numeric value to byte, double, float
 *, int, long, and short.
 */

#include <java/lang/package_decls.h>
#include <java/lang/Object.h>

using java::lang::Object;

namespace java { namespace lang {

class SMQJ_API Number : public Object
{
public:
    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
	virtual ~Number();

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
 * Number
 *			
 * All methods pure virtual
 *
 */
public:
    /**
     * Returns the value of this Number as a jbyte.
     *
     * @return  the jbyte value represented by this object.
     */
	virtual  jbyte byteValue() = 0;

    /**
     * Returns the value of this Number as a jdouble.
     *
     * @return  the jdouble value represented by this object.
     */
	virtual  jdouble doubleValue() = 0;

    /**
     * Returns the value of this Number as a jfloat.
     *
     * @return  the jfloat value represented by this object.
     */
	virtual  jfloat floatValue() = 0;

    /**
     * Returns the value of this Number as a jint.
     *
     * @return  the jint value represented by this object.
     */
	virtual  jint intValue() = 0;

    /**
     * Returns the value of this Number as a jlong.
     *
     * @return  the jlong value represented by this object.
     */
	virtual  jlong longValue() = 0;

    /**
     * Returns the value of this Number as a jshort.
     *
     * @return  the jshort value represented by this object.
     */
	virtual  jshort shortValue() = 0;

};

} } // namespace java::lang

#endif // _JAVA_LANG_LONG_H_
