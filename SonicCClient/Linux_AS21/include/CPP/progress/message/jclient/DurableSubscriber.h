#ifndef _PROGRESS_MESSAGE_JCLIENT_DURABLESUBSCRIBER_H_
#define _PROGRESS_MESSAGE_JCLIENT_DURABLESUBSCRIBER_H_
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
#include <progress/message/jclient/ITopicSubscriber.h>


namespace progress { namespace message { namespace jclient {


class SMQ_API DurableSubscriber : public ITopicSubscriber
{
public:
    virtual ~DurableSubscriber();

    /**
     * Returns the int corresponding to the DurableSubscriber type.
     *
     * @return the int corresponding to the DurableSubscriber type
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
     * Closes the durable subscriber.
     *
     * Since the C-Client may allocate some resources on behalf of a message consumer,
     * clients should close them when they are not needed.
     *
     * @exception JMSException if JMS fails to close the consumer
     *                         due to some error.
     */ 
    virtual void close();

    /** Get the topic associated with this subscriber.
      *  
      * @return this subscriber's topic
      *  
      * @exception JMSException if JMS fails to get topic for
      *                     this topic subscriber
      *                     due to some internal error.
      */ 
    virtual TopicRef getTopic();

    /** Get the NoLocal attribute for this TopicSubscriber.
      * The default value for this attribute is false.
      *  
      * @return set to true if locally published messages are being inhibited.
      *  
      * @exception JMSException if JMS fails to get noLocal attribute for
      *                     this topic subscriber
      *                     due to some internal error.
      */ 
    virtual jboolean getNoLocal();
   
    private: virtual void reservedv1();
    private: virtual void reservedv2();
    private: virtual void reservedv3();
    private: virtual void reservedv4();
    private: virtual void reservedv7();
    private: virtual void reservedv8();
    private: void PMJDSUBreserved0();
};


}}} // namespace

#endif // _PROGRESS_MESSAGE_JCLIENT_DURABLESUBSCRIBER_H_
