#ifndef _PROGRESS_MESSAGE_JCLIENT_QUEUESESSION_H_
#define _PROGRESS_MESSAGE_JCLIENT_QUEUESESSION_H_
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


namespace progress { namespace message { namespace jclient {

/** A QueueSession provides methods for creating QueueReceiver's, 
  * QueueSender's, QueueBrowser's and TemporaryQueues.
  *
  * <P>If there are messages that have been received but not acknowledged 
  * when a QueueSession terminates, these messages will be retained and 
  * redelivered when a consumer next accesses the queue.
  *
  * @version     1.0  6-Jul-1998
  * @author      Giovanni Boschi
  *
  * @see         javax.jms.QueueSession
  * @see         progress.message.jclient.Session
  */
class SMQ_API QueueSession : public Session
{

public:
    virtual ~QueueSession();

    /**
     * Returns the int corresponding to the QueueSession type.
     *
     * @return the int corresponding to the QueueSession type
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

    /** Create a Queue given a Queue name.
      *
      * <P>This facility is provided for the rare cases where clients need to
      * dynamically manipulate queue identity. This allows the creation of a
      * queue with a provider specific name. Clients that depend on this
      * ability are not portable.
      *
      * @param queueName the name of this queue
      *
      * @return a Queue with the given name.
      *
      * @exception JMSException if a session fails to create a queue
      *                         due to some JMS error.
      */ 
    virtual QueueRef createQueue(StringRef queueName);

    /** Disables or reenables publish flow control.
      *
      * @param jboolean disabled true to disable flow control
      */
    virtual void setFlowControlDisabled( jboolean disabled );

    /** Create a QueueReceiver to receive messages from the specified queue.
      *
      * @param queue the queue to access
      *
      * @exception JMSException if a session fails to create a receiver
      *                         due to some JMS error.
      * @exception javax.jms.InvalidDestinationException if invalid Queue specified.
      */
    virtual QueueReceiverRef createReceiver(QueueRef queue);

    /** Create a QueueReceiver to receive messages from the specified queue.
      *  
      * @param queue the queue to access
      * @param messageSelector only messages with properties matching the
      * message selector expression are delivered
      *  
      * @exception JMSException if a session fails to create a receiver
      *                         due to some JMS error.
      * @exception javax.jms.InvalidDestinationException if invalid Queue specified.
      * @exception javax.jms.InvalidSelectorException if the message selector is invalid.
      */ 
    virtual QueueReceiverRef createReceiver(QueueRef queue, StringRef messageSelector);

    /** Create a QueueSender to send messages to the specified queue.
      *
      * @param queue the queue to access, or null if this is an unidentifed
      * producer.
      *
      * @exception JMSException if a session fails to create a sender
      *                         due to some JMS error.
      * @exception javax.jms.InvalidDestinationException if invalid Queue specified.
      */
    virtual QueueSenderRef createSender(QueueRef queue);

    /** Create a QueueBrowser to peek at the messages on the specified queue.
      *
      * @param queue the queue to access
      *
      * @exception JMSException if a session fails to create a browser
      *                         due to some JMS error.
      * @exception javax.jms.InvalidDestinationException if invalid Queue specified.
      */
    virtual QueueBrowserRef createBrowser(QueueRef queue);

    /** Create a QueueBrowser to peek at the messages on the specified queue.
      *  
      * @param queue the queue to access
      * @param messageSelector only messages with properties matching the
      * message selector expression are delivered
      *  
      * @exception JMSException if a session fails to create a browser
      *                         due to some JMS error.
      * @exception javax.jms.InvalidDestinationException if invalid Queue specified.
      * @exception javax.jms.InvalidSelectorException if the message selector is invalid.
      */ 
    virtual QueueBrowserRef createBrowser(QueueRef queue,StringRef messageSelector);

    /** Create a temporary queue. It's lifetime will be that of the 
      * QueueConnection unless deleted earlier.
      *
      * @return a temporary queue.
      *
      * @exception JMSException if a session fails to create a Temporary Queue
      *                         due to some JMS error.
      */
    virtual TemporaryQueueRef createTemporaryQueue();
    
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

    private: virtual void reservedv1();
    private: virtual void reservedv2();
    private: virtual void reservedv3();
    private: void PMJQSESSreserved0();
    private: void PMJQSESSreserved1();
    private: void PMJQSESSreserved2();
    private: void PMJQSESSreserved3();
    private: void PMJQSESSreserved4();
    private: void PMJQSESSreserved5();
    private: void PMJQSESSreserved6();
    private: void PMJQSESSreserved7();
};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_QUEUESESSION_H_
