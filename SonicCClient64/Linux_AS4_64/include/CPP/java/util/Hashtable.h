/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#ifndef _JAVA_UTIL_HASHTABLE_H_
#define _JAVA_UTIL_HASHTABLE_H_

#ifndef HPUX
#pragma warning(disable : 4251)
#endif

#include <java/util/Dictionary.h>


namespace java { namespace util {

  /**
   * This class implements a hashtable, which maps keys to values. Any
   * non-<code>null</code> object can be used as a key or as a value. <p>
   *
   * An instance of <code>Hashtable</code> has two parameters that affect its
   * performance: <i>initial capacity</i> and <i>load factor</i>.  The
   * <i>capacity</i> is the number of <i>buckets</i> in the hash table, and the
   * <i>initial capacity</i> is simply the capacity at the time the hash table
   * is created.  Note that the hash table is <i>open</i>: in the case a "hash
   * collision", a single bucket stores multiple entries, which must be searched
   * sequentially.  The <i>load factor</i> is a measure of how full the hash
   * table is allowed to get before its capacity is automatically increased.
   * When the number of entries in the hashtable exceeds the product of the load
   * factor and the current capacity, the capacity is increased by calling the
   * <code>rehash</code> method.<p>
   *
   * Generally, the default load factor (.75) offers a good tradeoff between
   * time and space costs.  Higher values decrease the space overhead but
   * increase the time cost to look up an entry (which is reflected in most
   * <tt>Hashtable</tt> operations, including <tt>get</tt> and <tt>put</tt>).<p>
   *
   * The initial capacity controls a tradeoff between wasted space and the
   * need for <code>rehash</code> operations, which are time-consuming.
   * No <code>rehash</code> operations will <i>ever</i> occur if the initial
   * capacity is greater than the maximum number of entries the
   * <tt>Hashtable</tt> will contain divided by its load factor.  However,
   * setting the initial capacity too high can waste space.<p>
   *
   * If many entries are to be made into a <code>Hashtable</code>,
   * creating it with a sufficiently large capacity may allow the
   * entries to be inserted more efficiently than letting it perform
   * automatic rehashing as needed to grow the table. <p>
   *
   * This example creates a hashtable of numbers. It uses the names of
   * the numbers as keys:
   * <p><blockquote><pre>
   *     HashtableRef numbers = createHashtable();
   *     numbers->put(createString("one"), createString("1"));
   *     numbers->put(createString("two"), createString("2"));
   *     numbers->put(createString("three"), createString("3"));
   * </pre></blockquote>
   * <p>
   * To retrieve a number, use the following code:
   * <p><blockquote><pre>
   *     StringRef n = StringRef::cast(numbers->get("two"));
   *     if (n != null) {
   *         System.out.println("two = " + n);
   *     }
   * </pre></blockquote>
   * <p>
   */
  class SMQJ_API Hashtable : public java::util::Dictionary
  {
  public:
    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
    virtual ~Hashtable();

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
     * Clears this hashtable so that it contains no keys.
     */
    virtual /* synchronized */ void clear();

    /**
     * Creates a shallow copy of this hashtable. All the structure of the
     * hashtable itself is copied, but the keys and values are not cloned.
     * This is a relatively expensive operation.
     *
     * @return  a clone of the hashtable.
     */
#ifdef WIN32 // Windows compiler bug workaround
    virtual /* synchronized */ Object * clone();
#else
    virtual /* synchronized */ Hashtable * clone();
#endif

    /**
     * Tests if some key maps into the specified value in this hashtable.
     * This operation is more expensive than the <code>containsKey</code>
     * method.<p>
     *
     * Note that this method is identical in functionality to containsValue,
     * (which is part of the Map interface in the collections framework).
     *
     * @param      value   a value to search for.
     * @return     <code>jtrue</code> if and only if some key maps to the
     *             <code>value</code> argument in this hashtable as
     *             determined by the <tt>equals</tt> method;
     *             <code>jfalse</code> otherwise.
     * @exception  NullPointerException  if the value is <code>null</code>.
     */
    virtual /* synchronized */ jboolean contains(Object * value);

    /**
     * Tests if the specified object is a key in this hashtable.
     *
     * @param   key   possible key.
     * @return  <code>jtrue</code> if and only if the specified object
     *          is a key in this hashtable, as determined by the
     *          <tt>equals</tt> method; <code>jfalse</code> otherwise.
     */
    virtual /* synchronized */ jboolean containsKey(Object * key);

    /**
     * Returns an enumeration of the values in this hashtable.
     * Use the Enumeration methods on the returned object to fetch the elements
     * sequentially.
     *
     * @return  an enumeration of the values in this hashtable.
     */
    virtual /* synchronized */ EnumerationRef elements();

    /**
     * Returns the value to which the specified key is mapped in this hashtable.
     *
     * @param   key   a key in the hashtable.
     * @return  the value to which the key is mapped in this hashtable;
     *          <code>null</code> if the key is not mapped to any value in
     *          this hashtable.
     */
    virtual /* synchronized */ Object * get(Object * key);

    /**
     * Tests if this hashtable maps no keys to values.
     *
     * @return  <code>true</code> if this hashtable maps no keys to values;
     *          <code>false</code> otherwise.
     */
    virtual jboolean isEmpty();

    /**
     * Returns an enumeration of the keys in this hashtable.
     *
     * @return  an enumeration of the keys in this hashtable.
     */
    virtual /* synchronized */ EnumerationRef keys();

    /**
     * Maps the specified <code>key</code> to the specified
     * <code>value</code> in this hashtable. Neither the key nor the
     * value can be <code>null</code>. <p>
     *
     * The value can be retrieved by calling the <code>get</code> method
     * with a key that is equal to the original key.
     *
     * @param      key     the hashtable key.
     * @param      value   the value.
     * @return     the previous value of the specified key in this hashtable,
     *             or <code>null</code> if it did not have one.
     * @exception  NullPointerException  if the key or value is
     *               <code>null</code>.
     */
    virtual /* synchronized */ Object * put(Object * key, Object * value);

    /**
     * Removes the key (and its corresponding value) from this
     * hashtable. This method does nothing if the key is not in the hashtable.
     *
     * @param   key   the key that needs to be removed.
     * @return  the value to which the key had been mapped in this hashtable,
     *          or <code>null</code> if the key did not have a mapping.
     */
    virtual /* synchronized */ ObjectRef remove(Object * key);

    /**
     * Returns the number of keys in this hashtable.
     *
     * @return  the number of keys in this hashtable.
     */
    virtual jint size();

    /**
     * Returns a string representation of this <tt>Hashtable</tt> object
     * in the form of a set of entries, enclosed in braces and separated
     * by the ASCII characters "<tt>,&nbsp;</tt>" (comma and space). Each
     * entry is rendered as the key, an equals sign <tt>=</tt>, and the
     * associated element, where the <tt>toString</tt> method is used to
     * convert the key and element to strings. <p>Overrides to
     * <tt>toString</tt> method of <tt>Object</tt>.
     *
     * @return  a string representation of this hashtable.
     */
    virtual /* synchronized */ StringRef toString();

    private: void JUHreserved0();
    private: void JUHreserved1();
    private: void JUHreserved2();

  };

}} // namespace java::util

#endif // _JAVA_UTIL_HASHTABLE_H_
