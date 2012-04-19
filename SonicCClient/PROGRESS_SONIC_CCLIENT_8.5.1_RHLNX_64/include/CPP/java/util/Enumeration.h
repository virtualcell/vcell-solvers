#ifndef _JAVA_UTIL_ENUMERATION_H_
#define _JAVA_UTIL_ENUMERATION_H_
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
   * An object that implements the Enumeration interface generates a
   * series of elements, one at a time. Successive calls to the
   * <code>nextElement</code> method return successive elements of the
   * series.
   * <p>
   * For example, to print all elements of a Hashtable <i>h</i>:
   * <blockquote><pre>
   *     for (EnumerationRef e = h->elements() ; e->hasMoreElements() ;) {
   *         cout << e->nextElement()->toString() << endl;<br>
   *     }
   * </pre></blockquote>
   * <p>
   * Methods are provided to enumerate through the keys of a hashtable, 
   * and the values in a hashtable.
   */
  class SMQJ_API Enumeration : public  Object
  {
  public:
    /**
     * For Sonic Software Internal use only: DO NOT USE.
     */
    virtual ~Enumeration ();

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
     * @return  <code>jtrue</code> if this object is the same as or derived from
     *          the given classtype;<code>jfalse</code> otherwise.
     */
    virtual bool	instanceof(int classtype) const;

    /**
     * Tests if this enumeration contains more elements.
     *
     * @return  <code>jtrue</code> if and only if this enumeration object
     *           contains at least one more element to provide;
     *          <code>jfalse</code> otherwise.
     */
    virtual jboolean hasMoreElements(void);

    /**
     * Returns the next element of this enumeration if this enumeration
     * object has at least one more element to provide.
     *
     * @return     the next element of this enumeration.
     * @exception  NoSuchElementException  if no more elements exist.
     */
    virtual ObjectRef nextElement(void);                                                                                                                                 

  };

} } // namespace java::util

#endif // _JAVA_LANG_ENUMERATION_H_
