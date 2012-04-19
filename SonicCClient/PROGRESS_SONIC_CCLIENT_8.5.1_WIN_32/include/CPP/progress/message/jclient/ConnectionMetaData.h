#ifndef _PROGRESS_MESSAGE_JCLIENT_CONNECTIONMETADATA_H_
#define _PROGRESS_MESSAGE_JCLIENT_CONNECTIONMETADATA_H_
/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#include <java/lang/package.h>
#include <java/util/package_decls.h>
#include <progress/message/jclient/package_decls.h>

using java::util::EnumerationRef;

namespace progress { namespace message { namespace jclient {

class SMQ_API ConnectionMetaData : public java::lang::Object
{
public:
	virtual ~ConnectionMetaData();
	/**
	 * Returns the int corresponding to the ConnectionMetaData type.
	 *
	 * @return the int corresponding to the ConnectionMetaData type
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
    /** Get the JMS version.
      *
      * @return the JMS version.
      *
      * @exception JMSException if some internal error occurs in
      *                         JMS implementation during the meta-data
      *                         retrieval.
      */
	virtual const StringRef getJMSVersion() const;
    /** Get the JMS major version number.
      *
      * @return the JMS major version number.
      *
      * @exception JMSException if some internal error occurs in
      *                         JMS implementation during the meta-data
      *                         retrieval.
      */
	virtual jint getJMSMajorVersion() const;
    /** Get the JMS minor version number.
      *
      * @return the JMS minor version number.
      *
      * @exception JMSException if some internal error occurs in
      *                         JMS implementation during the meta-data
      *                         retrieval.
      */
	virtual jint getJMSMinorVersion() const;
    /** Get the JMS provider name.
      *
      * @return the JMS provider name.
      *
      * @exception JMSException if some internal error occurs in
      *                         JMS implementation during the meta-data
      *                         retrieval.
      */
	virtual const StringRef getJMSProviderName() const;
    /** Get the JMS provider version.
      *
      * @return the JMS provider version.
      *
      * @exception JMSException if some internal error occurs in
      *                         JMS implementation during the meta-data
      *                         retrieval.
      */
	virtual const StringRef getProviderVersion() const;
    /** Get the JMS provider major version number.
      *
      * @return the JMS provider major version number.
      *
      * @exception JMSException if some internal error occurs in
      *                         JMS implementation during the meta-data
      *                         retrieval.
      */
	virtual jint getProviderMajorVersion() const;
    /** Get the JMS provider minor version number.
      *
      * @return the JMS provider minor version number.
      *
      * @exception JMSException if some internal error occurs in
      *                         JMS implementation during the meta-data
      *                         retrieval.
      */
	virtual jint getProviderMinorVersion() const;
    /** Get an enumeration of JMSX Property Names.
      *  
      * @return an Enumeration of JMSX PropertyNames.
      *  
      * @exception JMSException if some internal error occurs in
      *                         JMS implementation during the property
      *                         names retrieval.
      */
	virtual EnumerationRef getJMSXPropertyNames() const;
};

}}} // namespace progress::message::jclient


#endif // _PROGRESS_MESSAGE_JCLIENT_CONNECTIONMETADATA_H_

