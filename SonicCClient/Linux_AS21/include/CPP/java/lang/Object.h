#ifndef _JAVA_LANG_OBJECT_H_
#define _JAVA_LANG_OBJECT_H_
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

#include <java_decls.h>
#include <objcounters.h>

namespace java { namespace lang { 

  class ObjectRef;
  class StringRef;
  class String;
  class ThrowableRef;

  /**
   * Class <code>Object</code> is the root of the class hierarchy. 
   * Every class has <code>Object</code> as a superclass. All objects, 
   * including arrays, implement the methods of this class. 
   */
  class SMQJ_API Object
  {
  public:
    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
    virtual ~Object();

    // for ref counting
    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
    virtual long    addRef();

    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
    virtual long    release();

    /**
     * For Sonic Software Internal use only: DO NOT USE.
     * Debug utility that returns the address of the ref count.
     * Used to track reference count during execution.
     */
    virtual long    *getRefCnt();

    // run-time type stuff
    /**
     * Returns the type-code for this class' classtype.
     *
     * @return     the type-code for this class.
     */
    static int      type();

    /**
     * Returns the type-code for this object's classtype.
     *
     * @return     the type-code for this object.
     */
    virtual int     getType() const;

    /**
     * Indicates whether this object is an instance of the given classtype. 
     * An object is an instance of the given classtype if it is exactly
     * that classtype or derives from that classtype.
     *
     * @param   classtype   the type-code for the classtype with which to compare.
     * @return  <code>jtrue</code> if this object is the same as or derived from
     *          the given classtype;<code>jfalse</code> otherwise.
     */
    virtual bool    instanceof(int classtype) const;

    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
    virtual jint hashCode();

    /**
     * Indicates whether some other object is "equal to" this one.
     * <p>
     * The <code>equals</code> method implements an equivalence relation: 
     * <ul>
     * <li>It is <i>reflexive</i>: for any reference value <code>x</code>, 
     *     <code>x.equals(x)</code> should return <code>jtrue</code>. 
     * <li>It is <i>symmetric</i>: for any reference values <code>x</code> and 
     *     <code>y</code>, <code>x.equals(y)</code> should return 
     *     <code>jtrue</code> if and only if <code>y.equals(x)</code> returns 
     *     <code>jtrue</code>. 
     * <li>It is <i>transitive</i>: for any reference values <code>x</code>, 
     *     <code>y</code>, and <code>z</code>, if <code>x.equals(y)</code>
     *     returns  <code>jtrue</code> and <code>y.equals(z)</code> returns 
     *     <code>jtrue</code>, then <code>x.equals(z)</code> should return 
     *     <code>jtrue</code>. 
     * <li>It is <i>consistent</i>: for any reference values <code>x</code> 
     *     and <code>y</code>, multiple invocations of <tt>x.equals(y)</tt>
     *     consistently return <code>jtrue</code> or consistently return 
     *     <code>jfalse</code>, provided no information used in
     *     <code>equals</code> comparisons on the object is modified.
     * <li>For any non-null reference value <code>x</code>, 
     *     <code>x.equals(null)</code> should return <code>jfalse</code>.
     * </ul>
     * <p>
     * The <tt>equals</tt> method for class <code>Object</code> implements 
     * the most discriminating possible equivalence relation on objects; 
     * that is, for any reference values <code>x</code> and <code>y</code>, 
     * this method returns <code>jtrue</code> if and only if <code>x</code> and 
     * <code>y</code> refer to the same object (<code>x==y</code> has the 
     * value <code>jtrue</code>). 
     *
     * @param   obj   the reference object with which to compare.
     * @return  <code>jtrue</code> if this object is the same as the obj
     *          argument; <code>jfalse</code> otherwise.
     */
    virtual jboolean equals(ObjectRef obj) const;

    /**
     * Creates and returns a copy of this object.  The precise meaning 
     * of "copy" may depend on the class of the object. The general 
     * intent is that, for any object <tt>x</tt>, the expression:
     * <blockquote>
     * <pre>
     * x->clone() != x</pre></blockquote>
     * will be <tt>jtrue<tt>, and that the expression:
     * <blockquote>
     * <pre>
     * x->clone()->getType() == x->getType()</pre></blockquote>
     * will be <tt>jtrue</tt>, but these are not absolute requirements. 
     * While it is typically the case that:
     * <blockquote>
     * <pre>
     * x->clone()->equals(x)</pre></blockquote>
     * will be <tt>jtrue</tt>, this is not an absolute requirement. 
     * Copying an object will typically entail creating a new instance of 
     * its class, but it also may require copying of internal data 
     * structures as well.  No constructors are called.
     * <p>
     * The class <tt>Object</tt> does not itself implement the interface 
     * <tt>Cloneable</tt>, so calling the <tt>clone</tt> method on an object 
     * whose class is <tt>Object</tt> will result in throwing an
     * exception at run time. 
     *
     * @return     a clone of this instance.
     * @exception  CloneNotSupportedExceptionRef  if the object's class does not
     *               support the <code>Cloneable</code> interface. Subclasses
     *               that override the <code>clone</code> method can also
     *               throw this exception to indicate that an instance cannot
     *               be cloned.
     */
    virtual Object * clone() /* throw (CloneNotSupportedException) */;

    /**
     * Returns a string representation of the object. In general, the 
     * <code>toString</code> method returns a string that 
     * "textually represents" this object. The result should 
     * be a concise but informative representation that is easy for a 
     * person to read.
     * It is recommendedthat all subclasses override this method.
     * <p>
     *
     * @return  a string representation of the object.
     */
    virtual StringRef toString();

    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
    virtual int operator==( Object* p_) const; 

    // this is here so Ref classes can throw this w/o forming circular dependencies
    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
    static void     throwClassCastException();



  };

} /* namespace lang */ } /* namespace java*/


using java::lang::Object;

#endif // _JAVA_LANG_OBJECT_H_
