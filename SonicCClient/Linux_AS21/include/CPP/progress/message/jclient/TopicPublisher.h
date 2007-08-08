#ifndef _PROGRESS_MESSAGE_JCLIENT_TOPICPUBLISHER_H_
#define _PROGRESS_MESSAGE_JCLIENT_TOPICPUBLISHER_H_
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

#include <progress/message/jclient/MessageProducer.h>

namespace progress { namespace message { namespace jclient {


class SMQ_API TopicPublisher : public MessageProducer
{
public:
	virtual ~TopicPublisher();

	/**
	 * Returns the int corresponding to the TopicPublisher type.
	 *
	 * @return the int corresponding to the TopicPublisher type
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

    /** Get the topic associated with this publisher.
      *
      * @return this publisher's topic
      *  
      * @exception JMSException if JMS fails to get topic for
      *                         this topic publisher
      *                         due to some internal error.
      */
    virtual TopicRef getTopic();
 
    /** Publish a Message to the topic
      * Use the topics default delivery mode, timeToLive and priority. 
      *
      * @param message the message to publish
      *
      * @exception JMSException if JMS fails to publish the message
      *                         due to some internal error.
      * @exception InvalidDestinationException if a client uses
      *                         this method with a Topic Publisher with
      *                         an invalid topic.
      */
    virtual void publish(MessageRef message);

    /** Publish a Message to the topic specifying delivery mode, priority 
      * and time to live to the topic.
      *
      * @param message the message to publish
      * @param deliveryMode the delivery mode to use
      * @param priority the priority for this message
      * @param timeToLive the message's lifetime (in milliseconds).
      *
      * @exception JMSException if JMS fails to publish the message
      *                         due to some internal error.
      * @exception javax::jms::IllegalStateException if DISCARDABLE is set on a
      *                         transacted session.
      * @exception InvalidDestinationException if a client uses
      *                         this method with a Topic Publisher with
      *                         an invalid topic.
      */
    virtual void publish(
		MessageRef message, 
        jint deliveryMode, 
        jint priority,
        jlong timeToLive);

    /** Publish a Message to a topic for an unidentified message producer.	
      * Use the topics default delivery mode, timeToLive and priority.
      *  
      * <P>Typically a JMS message producer is assigned a topic at creation 
      * time; however, JMS also supports unidentified message producers 
      * which require that the topic be supplied on every message publish.
      *
      * @param topic the topic to publish this message to
      * @param message the message to send
      *  
      * @exception JMSException if JMS fails to publish the message
      *                         due to some internal error.
      * @exception InvalidDestinationException if a client uses
      *                         this method with an invalid topic.
      */ 
    virtual void publish(
		TopicRef topic, 
		MessageRef message);

    /** Publish a Message to a topic for an unidentified message producer,
      * specifying delivery mode, priority and time to live.
      *  
      * <P>Typically a JMS message producer is assigned a topic at creation
      * time; however, JMS also supports unidentified message producers
      * which require that the topic be supplied on every message publish.
      *
      * @param topic the topic to publish this message to
      * @param message the message to send
      * @param deliveryMode the delivery mode to use
      * @param priority the priority for this message
      * @param timeToLive the message's lifetime (in milliseconds).
      *  
      * @exception JMSException if JMS fails to publish the message
      *                         due to some internal error.
      * @exception javax::jms::IllegalStateException if DISCARDABLE is set on a
      *                         transacted session.
      * @exception InvalidDestinationException if a client uses
      *                         this method with an invalid topic.
      */ 
    virtual void publish(
		TopicRef topic, 
        MessageRef message, 
        jint deliveryMode, 
        jint priority,
	    jlong timeToLive);
    
    /** 
      * Closes the topic publisher.
      *
      * Since the C-Client may allocate some resources on behalf of a message producer,
      * clients should close them when they are not needed.
      *
      * @exception JMSException if JMS fails to close the producer
      *                         due to some error.
      */ 
    virtual void close();

    /** Set the producer's default delivery mode.
      *  
      * <P>Delivery mode is set to PERSISTENT by default.
      *
      * @param deliveryMode the message delivery mode for this message
      * producer.
      *  
      * @exception JMSException if JMS fails to set delivery mode
      *                         due to some internal error.          
      * @exception javax::jms::IllegalStateException if DISCARDABLE is set on a
      *                         transacted session.
      */ 
    virtual void setDeliveryMode(jint deliveryMode);
    
    private: void PMJTPreserved0();
};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_TOPICPUBLISHER_H_
