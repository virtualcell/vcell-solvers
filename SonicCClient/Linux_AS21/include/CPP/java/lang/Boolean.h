#ifndef _JAVA_LANG_BOOLEAN_H_
#define _JAVA_LANG_BOOLEAN_H_
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

using java::lang::Object;

namespace java { namespace lang {

/**
 * Creates a new <code>Boolean</code> object that initially represents the 
 *   value passed in.
 *
 * @param  b   the initial value of the Boolean.
 * @return the newly created Boolean object.
 */
SMQJ_API BooleanRef createBoolean(jboolean b);

class SMQJ_API Boolean : public Object
{
public:
    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
	virtual ~Boolean();

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
     * Returns the value of this <code>Boolean</code> object as a boolean primitive.
     *
     * @return  the primitive <code>boolean</code> value of this object.
     */
	jboolean			booleanValue();

    /**
     * Returns a String object representing this Boolean's value. 
	 *  If this object represents the value true, a string equal to "true" 
	 *  is returned. Otherwise, a string equal to "false" is returned.
     *
     * @return  a string representation of this object.
     */
	StringRef			toString();

    /**
     * Returns true if and only if the argument is not null and is a Boolean 
	 *  object that represents the same boolean value as this object.
	 *
     * @param   obj the object to compare with.
     * @return  jtrue if the Boolean objects represent the same value; jfalse otherwise.
     */
	jboolean			equals(ObjectRef obj) const;

/*
 * Static methods
 */
public:
	/**
	 * Returns the boolean value represented by the specified String.
	 *   A new Boolean object is constructed. This Boolean represents 
	 *   the value true if the string argument is not null and is 
	 *   equal, ignoring case, to the string "true".
	 *
	 * @param   s a string
	 * @return  the Boolean value represented by the string.
	 */
	static BooleanRef	valueOf(StringRef s);

};

}} // namespace java::lang

#endif // _JAVA_LANG_BOOLEAN_H_
