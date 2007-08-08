#ifndef _PROGRESS_MESSAGE_JCLIENT_JMSEXCEPTION_H_
#define _PROGRESS_MESSAGE_JCLIENT_JMSEXCEPTION_H_

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

#include <java/lang/package.h>
#include <progress/message/jclient/package_decls.h>

namespace progress { namespace message { namespace jclient {

class SMQ_API JMSException : public java::lang::Exception
{
public:
	virtual ~JMSException();

	/**
	 * Returns the int corresponding to the JMSException type.
	 *
	 * @return the int corresponding to the JMSException type
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
	 * Get the vendor specific error code.
	 *
	 * @return a string specifying the vendor specific error code
	 */
	virtual const StringRef getErrorCode();
	/**
	 * Get the exception linked to this one.
	 *
	 * @return the linked Exception, null if none
	 */
	virtual ExceptionRef getLinkedException();
	/**
	 * Add a linked Exception.
	 *
	 * @param e - the linked Exception
	 */
	virtual void setLinkedException(ExceptionRef e);
};

}}} // namespace


#endif // _PROGRESS_MESSAGE_JCLIENT_JMSEXCEPTION_H_
