/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#ifndef _PROGRESS_MESSAGE_JCLIENT_QUEUERECEIVER_H_
#define _PROGRESS_MESSAGE_JCLIENT_QUEUERECEIVER_H_

#include <progress/message/jclient/MessageConsumer.h>
#include <progress/message/jclient/package.h>


namespace progress { namespace message { namespace jclient {


/** A client uses a QueueReceiver for receiving messages that have been
 * delivered to a queue.
 *
 * <P>Although is possible to have two Sessions with a QueueReceiver for
 * the same queue, JMS does not define how messages are distributed between
 * the QueueReceivers.
 */

class SMQ_API QueueReceiver : public MessageConsumer
{
public :
    virtual ~QueueReceiver();

    /**
     * Returns the int corresponding to the QueueReceiver type.
     *
     * @return the int corresponding to the QueueReceiver type
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
     * Closes the queue receiver.
     *
     * Since the C-Client may allocate some resources on behalf of a message consumer,
     * clients should close them when they are not needed.
     *
     * @exception JMSException if JMS fails to close the consumer
     *                         due to some error.
     */
    virtual void close();

    /**
     * Get the queue associated with this queue receiver.
     *
     * @return the queue
     *
     * @exception JMSException if JMS fails to get queue for
     *                         this queue receiver
     *                         due to some internal error.
     */
    virtual progress::message::jclient::QueueRef getQueue();


    /**
     * Receive the next message produced for this message consumer.
     *
     * <P>This call blocks indefinitely until a message is produced.
     *
     * <P>If this receive is done within a transaction, the message
     * remains on the consumer until the transaction commits.
     *
     * @exception JMSException if JMS fails to receive the next
     *                     message due to some error.
     *
     * @return the next message produced for this message consumer,
     *         or null, if the connection closes while waiting
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
     * @exception JMSException if JMS fails to receive the next
     *                     message due to some error.
     * @return the next message produced for this message consumer,
     *         or null, if the connection closes while waiting, or if
     *         the wait times out.
     */
    virtual MessageRef  receive(jlong timeOut);

    /**
     * Receive the next message if one is immediately available.
     *
     * @exception JMSException if JMS fails to receive the next
     *                     message due to some error.
     * @return the next message produced for this message consumer, or
     * null if one is not available.
     */
    virtual MessageRef receiveNoWait();

    /**
     * Set the QueueReceiver's MessageListener.  If the connection is
     * started, this will also start the process of getting messages.
     *
     * This method is obsoleted by the class based listener setMessageListenerObj.
     * It is retained for backward compatibility.
     *
     * @param messageListener the messages are delivered to this listener function
     *
     * @exception JMSException if JMS fails to set message
     *                         listener due to some JMS error
     */
    virtual void setMessageListener(pfnMessageListener listener);

    /**
     * Set the QueueReceiver's MessageListener.  If the connection is
     * started, this will also start the process of getting messages.
     *
     * @param messageListener the messages are delivered to this listener object
     *
     * @exception JMSException if JMS fails to set message
     *                         listener due to some JMS error
     */
    virtual void setMessageListenerObj(MessageListener *listener);


    /**
     * Set the prefetch count for the QueueReceiver.  When this value
     * is greater than one, the broker will be able to send multiple
     * messages as part of a single QueueReceiver request. This can
     * improve performance.
     *
     * <p>Note that this is a Progress SonicMQ extention not found in the
     * standard {@link javax.jms.QueueReceiver} interface.
     *
     * @param count The number of messages to prefetch.
     * @exception javax.jms.JMSException if an invalid value is set.
     * @see #setPrefetchThreshold(int)
     * @see #getPrefetchCount()
     */
    virtual void setPrefetchCount(jint count);

    /**
     * Get the prefetch count for the QueueReceiver. When this value
     * is greater than one, the broker will be able to send multiple
     * messages as part of a single QueueReceiver request.
     * @return The number of messages to prefetch.
     * @see #setPrefetchCount()
     */
    virtual jint getPrefetchCount();

    /**
     * Set the prefetch threshold for the QueueReceiver.  When the
     * number of messages waiting to be processed by the QueueReceiver
     * falls to, or below, this number, a new batch of messages will be fetched.
     * This number cannot exceed the prefetch count.
     *
     * <p>Setting this to a value greater than zero allows the QueueReceiver
     * to always have messages available for processing locally without
     * waiting for a broker interaction.  This improves performance.
     *
     * <p>For example, a threshold value of two and a prefetch count of five
     * will cause the QueueReceiver to fetch batches of five messages from the
     * broker whenever the number of messages locally waiting for processing
     * drops below two.
     *
     * <p>Note that this is a Progress SonicMQ extention not found in the
     * standard {@link javax.jms.QueueReceiver} interface.
     *
     * @param threshold The threshold value for prefetching messages.
     * @exception javax.jms.JMSException if an invalid value is set.
     * @see #setPrefetchCount(int)
     * @see #getPrefetchThreshold()
     */
    virtual void setPrefetchThreshold(jint threshold);

    /**
     * Get the prefetch threshold for the QueueReceiver.  When the
     * number of messages waiting to be processed by the QueueReceiver
     * falls to, or below, this number, a new batch of messages will be fetched.
     * @return The threshold value for prefetching messages.
     * @see #setPrefetchCount(int)
     */
    virtual jint getPrefetchThreshold();

    private: virtual void reservedv1();
    private: virtual void reservedv2();
    private: virtual void reservedv5();
    private: virtual void reservedv6();
    private: virtual void reservedv7();
    private: virtual void reservedv8();
    private: void PMJQRreserved0();
    private: void PMJQRreserved1();
    private: void PMJQRreserved2();
    private: void PMJQRreserved3();
    private: void PMJQRreserved4();
    private: void PMJQRreserved5();
    private: void PMJQRreserved6();
    private: void PMJQRreserved7();
    private: void PMJQRreserved8();
    private: void PMJQRreserved9();
    private: void PMJQRreserved10();
    private: void PMJQRreserved11();
    private: void PMJQRreserved12();
    private: void PMJQRreserved13();
    private: void PMJQRreserved14();
};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_QUEUERECEIVER_H_
