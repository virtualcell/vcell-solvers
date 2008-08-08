#ifndef _JAVA_UTIL_DICTIONARY_H_
#define _JAVA_UTIL_DICTIONARY_H_
/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#include <java/util/package_decls.h>

namespace java { namespace util { 

  /**
   * The <code>Dictionary</code> class is the abstract parent of any 
   * class, such as <code>Hashtable</code>, which maps keys to values. 
   * Every key and every value is an object. In any one <tt>Dictionary</tt> 
   * object, every key is associated with at most one value. Given a 
   * <tt>Dictionary</tt> and a key, the associated element can be looked up. 
   * Any non-<code>null</code> object can be used as a key and as a value.
   * <p>
   * As a rule, the <code>equals</code> method should be used by 
   * implementations of this class to decide if two keys are the same. 
   * <p>
   */
  class SMQJ_API Dictionary : public java::lang::Object
  {
  public:
    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
    virtual ~Dictionary();

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
    virtual bool instanceof(int classtype) const;

    /**
     * Returns an enumeration of the values in this dictionary. The general 
     * contract for the <tt>elements</tt> method is that an 
     * <tt>Enumeration</tt> is returned that will generate all the elements 
     * contained in entries in this dictionary.
     *
     * @return  an enumeration of the values in this dictionary.
     */
    virtual /* synchronized */ EnumerationRef elements() = 0;

    /**
     * Returns the value to which the key is mapped in this dictionary. 
     * The general contract for the <tt>isEmpty</tt> method is that if this 
     * dictionary contains an entry for the specified key, the associated 
     * value is returned; otherwise, <tt>null</tt> is returned. 
     *
     * @return  the value to which the key is mapped in this dictionary;
     * @param   key   a key in this dictionary.
     *          <code>null</code> if the key is not mapped to any value in
     *          this dictionary.
     * @exception NullPointerException if the <tt>key</tt> is <tt>null</tt>.
     */
    virtual /* synchronized */ Object * get(Object * key) = 0;

    /**
     * Tests if this dictionary maps no keys to value. The general contract 
     * for the <tt>isEmpty</tt> method is that the result is true if and only 
     * if this dictionary contains no entries. 
     *
     * @return  <code>jtrue</code> if this dictionary maps no keys to values;
     *          <code>jfalse</code> otherwise.
     */
    virtual jboolean isEmpty() = 0;

    /**
     * Returns an enumeration of the keys in this dictionary. The general 
     * contract for the keys method is that an <tt>Enumeration</tt> object 
     * is returned that will generate all the keys for which this dictionary 
     * contains entries. 
     *
     * @return  an enumeration of the keys in this dictionary.
     */
    virtual /* synchronized */ EnumerationRef keys() = 0;

    /**
     * Maps the specified <code>key</code> to the specified 
     * <code>value</code> in this dictionary. Neither the key nor the 
     * value can be <code>null</code>.
     * <p>
     * If this dictionary already contains an entry for the specified 
     * <tt>key</tt>, the value already in this dictionary for that 
     * <tt>key</tt> is returned, after modifying the entry to contain the
     *  new element. <p>If this dictionary does not already have an entry 
     *  for the specified <tt>key</tt>, an entry is created for the 
     *  specified <tt>key</tt> and <tt>value</tt>, and <tt>null</tt> is 
     *  returned.
     * <p>
     * The <code>value</code> can be retrieved by calling the 
     * <code>get</code> method with a <code>key</code> that is equal to 
     * the original <code>key</code>. 
     *
     * @param      key     the hashtable key.
     * @param      value   the value.
     * @return     the previous value to which the <code>key</code> was mapped
     *             in this dictionary, or <code>null</code> if the key did not
     *             have a previous mapping.
     * @exception  NullPointerException  if the <code>key</code> or
     *               <code>value</code> is <code>null</code>.
     */
    virtual /* synchronized */ Object * put(Object * key, Object * value) = 0;

    /**
     * Removes the <code>key</code> (and its corresponding 
     * <code>value</code>) from this dictionary. This method does nothing 
     * if the <code>key</code> is not in this dictionary. 
     *
     * @param   key   the key that needs to be removed.
     * @return  the value to which the <code>key</code> had been mapped in this
     *          dictionary, or <code>null</code> if the key did not have a
     *          mapping.
     * @exception NullPointerException if <tt>key</tt> is <tt>null</tt>.
     */
    virtual /* synchronized */ ObjectRef remove(Object * key) = 0;

    /**
     * Returns the number of entries (dinstint keys) in this dictionary.
     *
     * @return  the number of keys in this dictionary.
     */
    virtual jint size() = 0;
  };

}} // namespace java::util


#endif // _JAVA_UTIL_DICTIONARY_H_
