#ifndef _PROGRESS_MESSAGE_JCLIENT_TOPICSESSION_H_
#define _PROGRESS_MESSAGE_JCLIENT_TOPICSESSION_H_
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

#include <progress/message/jclient/package_decls.h>
#include <progress/message/jclient/Session.h>
#include <java/util/package_decls.h>


namespace progress { namespace message { namespace jclient {


class SMQ_API TopicSession : public Session
{
public:
    virtual ~TopicSession();

    /**
     * Returns the int corresponding to the TopicSession type.
     *
     * @return the int corresponding to the TopicSession type
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
    
    /** Create a Topic given a Topic name.
      *
      * <P>This facility is provided for the rare cases where clients need to
      * dynamically manipulate topic identity. This allows the creation of a
      * topic with a provider specific name. Clients that depend on this
      * ability are not portable.
      *
      * @param topicName the name of this topic
      *
      * @return a Topic with the given name.
      *
      * @exception JMSException if a session fails to create a topic
      *                         due to some JMS error.
      */
    virtual TopicRef createTopic(StringRef topicName);
    
    /** Disables or reenables publish flow control.
      *
      * @param jboolean disabled true to disable flow control
      */
    virtual void setFlowControlDisabled( jboolean disabled );

    /** Create a non-durable Subscriber to the specified topic.
      *  
      * <P>A client uses a TopicSubscriber for receiving messages that have 
      * been published to a topic.
      *
      * <P>Regular TopicSubscriber's are not durable. They only receive 
      * messages that are published while they are active.
      *
      * <P>In some cases, a connection may both publish and subscribe to a 
      * topic. The subscriber NoLocal attribute allows a subscriber to 
      * inhibit the delivery of messages published by its own connection.
      * The default value for this attribute is false.
      *
      * @param topic the topic to subscribe to
      *  
      * @exception JMSException if a session fails to create a subscriber
      *                         due to some JMS error.
      * @exception javax::jms::InvalidDestinationException if invalid Topic specified.
      */ 
    virtual TopicSubscriberRef createSubscriber(TopicRef topic);

    /** Create a non-durable Subscriber to the specified topic.
      *
      * <P>A client uses a TopicSubscriber for receiving messages that have 
      * been published to a topic.
      *  
      * <P>Regular TopicSubscriber's are not durable. They only receive 
      * messages that are published while they are active.
      *
      * <P>Messages filtered out by a subscriber's message selector will 
      * never be delivered to the subscriber. From the subscriber's 
      * perspective they simply don't exist.
      *
      * <P>In some cases, a connection may both publish and subscribe to a 
      * topic. The subscriber NoLocal attribute allows a subscriber to 
      * inhibit the delivery of messages published by its own connection.
      *
      * @param topic the topic to subscribe to
      * @param messageSelector only messages with properties matching the
      *     message selector expression are delivered. This value may be null.
      * @param noLocal if set, inhibits the delivery of messages published
      *     by its own connection.
      * 
      * @exception JMSException if a session fails to create a subscriber
      *                         due to some JMS error or invalid selector.
      * @exception javax::jms::InvalidDestinationException if invalid Topic specified.
      * @exception javax::jms::InvalidSelectorException if the message selector is invalid.
      */
    virtual TopicSubscriberRef createSubscriber(
            TopicRef topic, 
            StringRef messageSelector, 
            jboolean noLocal);

    /** Create a durable Subscriber to the specified topic.
      *  
      * <P>If a client needs to receive all the messages published on a 
      * topic including the ones published while the subscriber is inactive,
      * it uses a durable TopicSubscriber. JMS retains a record of this 
      * durable subscription and insures that all messages from the topic's 
      * publishers are retained until they are either acknowledged by this 
      * durable subscriber or they have expired.
      *
      * <P>Sessions with durable subscribers must always provide the same 
      * client identifier. In addition, each client must specify a name which 
      * uniquely identifies (within client identifier) each durable 
      * subscription it creates. Only one session at a time can have a 
      * TopicSubscriber for a particular durable subscription.
      *
      * <P>A client can change an existing durable subscription by creating 
      * a durable TopicSubscriber with the same name and a new topic and/or 
      * message selector. Changing a durable subscriber is equivalent to 
      * deleting and recreating it
      *
      * @param topic the topic to subscribe to
      * @param name the name used to identify this subscription.
      *  
      * @exception JMSException if a session fails to create a subscriber
      *                         due to some JMS error.
      * @exception javax::jms::InvalidDestinationException if invalid Topic specified.
      */ 
    virtual DurableSubscriberRef createDurableSubscriber(TopicRef topic, StringRef name);

    /** Create a durable Subscriber to the specified topic.
      *  
      * <P>If a client needs to receive all the messages published on a 
      * topic including the ones published while the subscriber is inactive,
      * it uses a durable TopicSubscriber. JMS retains a record of this 
      * durable subscription and insures that all messages from the topic's 
      * publishers are retained until they are either acknowledged by this 
      * durable subscriber or they have expired.
      *
      * <P>Sessions with durable subscribers must always provide the same
      * client identifier. In addition, each client must specify a name which
      * uniquely identifies (within client identifier) each durable
      * subscription it creates. Only one session at a time can have a
      * TopicSubscriber for a particular durable subscription.
      *  
      * <P>A client can change an existing durable subscription by creating 
      * a durable TopicSubscriber with the same name and a new topic and/or 
      * message selector. Changing a durable subscriber is equivalent to 
      * deleting and recreating it
      *
      * @param topic the topic to subscribe to
      * @param name the name used to identify this subscription.
      * @param messageSelector only messages with properties matching the
      *     message selector expression are delivered. This value may be null.
      * @param noLocal if set, inhibits the delivery of messages published
      *     by its own connection.
      *  
      * @exception JMSException if a session fails to create a subscriber
      *                         due to some JMS error or invalid selector.
      * @exception javax::jms::InvalidDestinationException if invalid Topic specified.
      * @exception javax::jms::InvalidSelectorException if the message selector is invalid.
      */ 
    virtual DurableSubscriberRef createDurableSubscriber(
            TopicRef topic,
            StringRef name, 
            StringRef messageSelector,
            jboolean noLocal);

    /** Create a durable Subscriber to the specified topic.
      *  
      * <P>If a client needs to receive all the messages published on a 
      * topic including the ones published while the subscriber is inactive,
      * it uses a durable TopicSubscriber. JMS retains a record of this 
      * durable subscription and insures that all messages from the topic's 
      * publishers are retained until they are either acknowledged by this 
      * durable subscriber or they have expired.
      *
      * <P>Sessions with durable subscribers must always provide the same
      * client identifier. In addition, each client must specify a name which
      * uniquely identifies (within client identifier) each durable
      * subscription it creates. Only one session at a time can have a
      * TopicSubscriber for a particular durable subscription.
      *  
      * <P>A client can change an existing durable subscription by creating 
      * a durable TopicSubscriber with the same name and a new topic and/or 
      * message selector. Changing a durable subscriber is equivalent to 
      * deleting and recreating it
      *
      * @param topic the topic to subscribe to
      * @param name the name used to identify this subscription.
      * @param messageSelector only messages with properties matching the
      *     message selector expression are delivered. This value may be null.
      * @param noLocal if set, inhibits the delivery of messages published
      *     by its own connection.
      * @param timeToLive time in milliseconds that a DurableSubscriber
      *                   will live for after it has disconnected.
      * NOTE: A timeToLive value <= 0 will result in NOT setting a disconnected timeToLive.
      *
      * @exception JMSException if a session fails to create a subscriber
      *                         due to some JMS error or invalid selector.
      * @exception javax::jms::InvalidDestinationException if invalid Topic specified.
      * @exception javax::jms::InvalidSelectorException if the message selector is invalid.
      */ 
    virtual DurableSubscriberRef createDurableSubscriber(
            TopicRef topic,
            StringRef name, 
            StringRef messageSelector,
            jboolean noLocal,
            jlong timeToLive);

    /** Create a Publisher for the specified topic.
      *
      * <P>A client uses a TopicPublisher for publishing messages on a topic.
      * Each time a client creates a TopicPublisher on a topic, it defines a 
      * new sequence of messages that have no ordering relationship with the 
      * messages it has previously sent.
      *
      * @param topic the topic to publish to, or null if this is an 
      * unidentifed producer.
      *
      * @exception JMSException if a session fails to create a publisher
      *                         due to some JMS error.
      * @exception javax::jms::InvalidDestinationException if invalid Topic specified.
      */
    virtual TopicPublisherRef createPublisher(TopicRef topic);

    /** Create a temporary topic. It's lifetime will be that of the 
      * TopicConnection unless deleted earlier.
      *
      * @return a temporary topic.
      *
      * @exception JMSException if a session fails to create a temporary
      *                         topic due to some JMS error.
      */
    virtual TemporaryTopicRef createTemporaryTopic();

    /** Closes all subscribers and publishers active in this session.
      *
      * Since the C-Client may allocate some resources on behalf of a Session,
      * clients should close them when they are not needed.
      *
      * Note: closing a connection also closes all underlying sessions.
      *
      * @exception JMSException if JMS implementation fails to close a
      *                         Session due to some internal error.
      */
    virtual void close();
    
    /** Unsubscribe a durable subscription that has been created by a client.
      *  
      * <P>This deletes the state being maintained on behalf of the 
      * subscriber by its provider.
      *
      * @param name the name used to identify this subscription.
      *  
      * @exception JMSException if JMS fails to unsubscribe to durable
      *                         subscription due to some JMS error.
      * @exception javax::jms::InvalidDestinationException if invalid Topic specified.
      */
    virtual void unsubscribe(StringRef name);

    private: virtual void reservedv1();
    private: virtual void reservedv2();
    private: virtual void reservedv3();
    private: void PMJTSESSreserved0();
    private: void PMJTSESSreserved1();
    private: void PMJTSESSreserved2();
    private: void PMJTSESSreserved3();
    private: void PMJTSESSreserved4();
    private: void PMJTSESSreserved5();
    private: void PMJTSESSreserved6();
    private: void PMJTSESSreserved7();
    private: void PMJTSESSreserved8();
    private: void PMJTSESSreserved9();
    private: void PMJTSESSreserved10();
    private: void PMJTSESSreserved11();
    private: void PMJTSESSreserved12();
    private: void PMJTSESSreserved13();
};


}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_TOPICSESSION_H_
