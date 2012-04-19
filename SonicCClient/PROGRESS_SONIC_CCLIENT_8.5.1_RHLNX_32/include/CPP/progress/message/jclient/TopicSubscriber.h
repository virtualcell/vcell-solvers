/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#ifndef _PROGRESS_MESSAGE_JCLIENT_TOPICSUBSCRIBER_H_
#define _PROGRESS_MESSAGE_JCLIENT_TOPICSUBSCRIBER_H_

#include <java/lang/package.h>
#include <progress/message/jclient/ITopicSubscriber.h>


namespace progress { namespace message { namespace jclient {


class SMQ_API TopicSubscriber : public ITopicSubscriber
{
public:
    virtual ~TopicSubscriber();

    /**
     * Returns the int corresponding to the TopicSubscriber type.
     *
     * @return the int corresponding to the TopicSubscriber type
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
     * Closes the topic subscriber.
     *
     * Since the C-Client may allocate some resources on behalf of a message consumer,
     * clients should close them when they are not needed.
     *
     * @exception JMSException if JMS fails to close the consumer
     *                         due to some error.
     */
    virtual void close();

    /**
     * Get the topic associated with this subscriber.
     *
     * @return this subscriber's topic
     *
     * @exception JMSException if JMS fails to get topic for
     *                     this topic subscriber
     *                     due to some internal error.
     */
    virtual TopicRef getTopic();

    /**
     *  Get the NoLocal attribute for this TopicSubscriber.
     * The default value for this attribute is false.
     *
     * @return true if locally published messages are being inhibited.
     *
     * @exception JMSException if JMS fails to get noLocal attribute for
     *                     this topic subscriber
     *                     due to some internal error.
     */
    virtual jboolean getNoLocal();

    /**
     * Receive the next message produced for this message consumer.
     *
     * <P>This call blocks indefinitely until a message is produced.
     *
     * <P>If this receive is done within a transaction, the message
     * remains on the consumer until the transaction commits.
     *
     * @return the next message produced for this message consumer,
     *         or null, if the connection closes while waiting
     *
     * @exception JMSException if JMS fails to receive the next
     *                     message due to some error.
     */
    virtual MessageRef receive();

    /**
     * Receive the next message that arrives within the specified
     * timeout interval.
     *
     * <P>This call blocks until either a message arrives or the
     * timeout expires.
     *
     * @param timeout the timeout value (in milliseconds)
     *
     * @return the next message produced for this message consumer,
     *         or null, if the connection closes while waiting, or if
     *         the wait times out.
     *
     * @exception JMSException if JMS fails to receive the next
     *                     message due to some error.
     */
    virtual MessageRef receive(jlong timeout);

    /**
     * Receive the next message if one is immediately available.
     *
     * @return the next message produced for this message consumer, or
     * null if one is not available.
     *
     * @exception JMSException if JMS fails to receive the next
     *                     message due to some error.
     */
    virtual MessageRef receiveNoWait();

    private: virtual void reservedv1();
    private: virtual void reservedv2();
    private: virtual void reservedv7();
    private: virtual void reservedv8();
};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_TOPICSUBSCRIBER_H_
