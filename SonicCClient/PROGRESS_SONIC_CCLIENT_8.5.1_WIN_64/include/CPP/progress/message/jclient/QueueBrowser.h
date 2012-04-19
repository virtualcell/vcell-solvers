/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#ifndef _PROGRESS_MESSAGE_JCLIENT_QUEUEBROWSER_H_
#define _PROGRESS_MESSAGE_JCLIENT_QUEUEBROWSER_H_

#include <progress/message/jclient/package_decls.h>
#include <java/util/Enumeration.h>


namespace progress { namespace message { namespace jclient {


/** A client uses a QueueBrowser to look at messages on a queue without
 * removing them.
 *
 * <P>The browse methods return a java.util.Enumeration that is used to
 * scan the queue's messages. It may be an enumeration of the entire content
 * of a queue or it may only contain the messages matching a message selector.
 *
 * <P>Messages may be arriving and expiring while the scan is done. JMS does
 * not require the content of an enumeration to be a static snapshot of queue
 * content. Whether these changes are visible or not depends on the JMS provider.
 *
 */
class SMQ_API QueueBrowser : public java::lang::Object //implements javax.jms.QueueBrowser
{
public:
    virtual ~QueueBrowser();

    /**
     * Returns the int corresponding to the QueueBrowser type.
     *
     * @return the int corresponding to the QueueBrowser type
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
     * Closes the queue browser.
     *
     * Since the C-Client may allocate some resources on behalf of a message consumer,
     * clients should close them when they are not needed.
     *
     * @exception JMSException if JMS fails to close the consumer
     *                         due to some error.
     */
    virtual void close();

    /**
     * Get the queue associated with this queue Browser.
     *
     * @return the queue
     *
     * @exception JMSException if JMS fails to get queue for
     *                         this queue Browser
     *                         due to some internal error.
     */
    virtual QueueRef getQueue();

    /**
     * Get this queue browser's message selector expression.
     *
     * @return this queue browser's message selector
     *
     * @exception JMSException if JMS fails to get the message
     *                         selector for this queue Browser
     *                         due to some internal error.
     */
    virtual StringRef getMessageSelector();

    /**
     * Get an enumeration for browsing the current queue
     * messages in the order they would be received.
     *
     * @return an enumeration for browsing the messages
     *
     * @exception JMSException if JMS fails to get the enumeration
     *                         for this queue Browser
     *                         due to some internal error.
     */
    virtual EnumerationRef getEnumeration();

    /**
     * Set the prefetch count for the QueueBrowser.  When this value
     * is greater than one, the broker will be able to send multiple
     * messages as part of a single QueueBrowser request. This can
     * improve performance.
     *
     * <p>Note that this is a Progress SonicMQ extention not found in the
     * standard {@link javax.jms.QueueBrowser} interface.
     *
     * @param count The number of messages to prefetch.
     * @exception javax.jms.JMSException if an invalid value is set.
     * @see #setPrefetchThreshold(int)
     * @see #getPrefetchCount()
     */
    void setPrefetchCount(jint count);
    
    /**
     * Get the prefetch count for the QueueBrowser.
     * @return The number of messages to prefetch.
     * @see #setPrefetchCount()
     */
    jint getPrefetchCount();

    /**
     * Set the prefetch threshold for the QueueBrowser.  When the
     * number of messages waiting to be processed by the QueueBrowser
     * falls to, or below, this number, a new batch of messages will be fetched.
     * This number cannot exceed the prefetch count.
     *
     * <p>Setting this to a value greater than zero allows the QueueBrowser
     * to always have messages available for processing locally without
     * waiting for a broker interaction.  This improves performance.
     *
     * <p>For example, a threshold value of two and a prefetch count of five
     * will cause the QueueBrowser to fetch batches of five messages from the
     * broker whenever the number of messages locally waiting for processing
     * drops below two.
     *
     * <p>Note that this is a Progress SonicMQ extention not found in the
     * standard {@link javax.jms.QueueBrowser} interface.
     *
     * @param threshold The threshold value for prefetching messages.
     * @exception javax.jms.JMSException if an invalid value is set.
     * @see #setPrefetchCount(int)
     * @see #getPrefetchThreshold()
     */
    void setPrefetchThreshold(jint threshold);

    /**
     * Get the prefetch threshold for the QueueBrowser.  When the
     * number of messages waiting to be processed by the QueueBrowser
     * falls to, or below, this number, a new batch of messages will be fetched.
     * @return The threshold value for prefetching messages.
     * @see #setPrefetchCount(int)
     */
    jint getPrefetchThreshold();
    
    private: void PMJQBreserved0();
    private: void PMJQBreserved1();
    private: void PMJQBreserved2();
    private: void PMJQBreserved3();
    private: void PMJQBreserved4();
    private: void PMJQBreserved5();
    private: void PMJQBreserved6();
    private: void PMJQBreserved7();
    private: void PMJQBreserved8();
    private: void PMJQBreserved9();
    private: void PMJQBreserved10();
    private: void PMJQBreserved11();
    private: void PMJQBreserved12();
};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_QUEUEBROWSER_H_
