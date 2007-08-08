#ifndef _PROGRESS_MESSAGE_JCLIENT_BROKERNAME_H_
#define _PROGRESS_MESSAGE_JCLIENT_BROKERNAME_H_
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

SMQ_API BrokerNameRef createBrokerName(StringRef brokerURL) /*:>  throws JMSException */;
SMQ_API BrokerNameRef createBrokerName(
	StringRef brokerHostName, 
	jint brokerPort, 
	StringRef brokerProtocol) /*:>  throws JMSException */;

/** A help Class for parsing and constructing a JMS brokerURL string. */
class SMQ_API BrokerName : public java::lang::Object
{


public:
	virtual ~BrokerName();
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
	 * Returns the int corresponding to the BrokerName type.
	 *
	 * @return the int corresponding to the BrokerName type
	 */
	static  int type();
	/**
	 * Retrieve the URL for a BrokerName object.
	 *
	 * @return String containing Broker URL.
	 */
	virtual const StringRef getBrokerURL();
	/**
	 * Retrieve the hostname of a BrokerName object.
	 *
	 * @return String object containing the hostname.
	 */
	virtual const StringRef getBrokerHostName();
	/**
	 * Retrieve the protocol for a BrokerName object.
	 *
	 * @return String object containing the protocol.
	 */
	virtual const StringRef getBrokerProtocol();
	/**
	 * Retrieve the port number of a BrokerName object.
	 *
	 * @return int containing the port number.
	 */
	virtual       jint      getBrokerPort() const;

private:
	void reserved();
};

}}} //:>  namespace progress::message::jclient


#endif // _PROGRESS_MESSAGE_JCLIENT_BROKERNAME_H_
