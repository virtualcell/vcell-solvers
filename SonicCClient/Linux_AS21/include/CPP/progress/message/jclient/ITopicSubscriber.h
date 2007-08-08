#ifndef _PROGRESS_MESSAGE_JCLIENT_ITOPICSUBSCRIBER_H_
#define _PROGRESS_MESSAGE_JCLIENT_ITOPICSUBSCRIBER_H_
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

#include <progress/message/jclient/MessageConsumer.h>

namespace progress { namespace message { namespace jclient {

/**
 * JMS TopicSubscriber interface implemented as an abstract base class 
 */
class SMQ_API ITopicSubscriber : public MessageConsumer
{
public:
	virtual ~ITopicSubscriber();

	/**
	 * Returns the int corresponding to the ITopicSubscriber type.
	 *
	 * @return the int corresponding to the ITopicSubscriber type
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

	/** Get the topic associated with this subscriber.
	  *  
	  * @return this subscriber's topic
	  *  
	  * @exception JMSException if JMS fails to get topic for
	  *						this topic subscriber
	  *						due to some internal error.
	  */ 
	virtual TopicRef getTopic() = 0;
	/** Get the NoLocal attribute for this TopicSubscriber or DurableSubscriber.
	  * The default value for this attribute is false.
	  *  
	  * @return true if locally published messages are being inhibited.
	  *  
	  * @exception JMSException if JMS fails to get noLocal attribute for
	  *						this topic subscriber
	  *						due to some internal error.
	  */ 
	virtual jboolean getNoLocal() = 0;

    /** Allow a provider to release resources allocated for a topic subscriber.
      *  
      * @exception JMSException if JMS fails to close the consumer
      *                         due to some error.
      */ 
    virtual void close() = 0;
};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_ITOPICSUBSCRIBER_H_
