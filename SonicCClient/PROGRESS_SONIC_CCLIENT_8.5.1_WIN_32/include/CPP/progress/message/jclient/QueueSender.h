#ifndef _PROGRESS_MESSAGE_JCLIENT_QUEUESENDER_H_
#define _PROGRESS_MESSAGE_JCLIENT_QUEUESENDER_H_
/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#include <progress/message/jclient/MessageProducer.h>

namespace progress { namespace message { namespace jclient {


/**
  * A client uses a QueueSender to send messages to a queue. 
  *
  * <P>Normally the Queue is specified when a QueueSender is created 
  * and in this case, attempting to use the methods for an unidentified 
  * QueueSender will throws an UnsupportedOperationException. 
  *
  * <P>In the case that the QueueSender with an unidentified Queue is 
  * created, the methods that assume the Queue has been identified throw 
  * an UnsupportedOperationException. 
  */

class QueueSender : public MessageProducer //implements javax.jms.QueueSender
{
public:
	virtual ~QueueSender();

	/**
	 * Returns the int corresponding to the QueueSender type.
	 *
	 * @return the int corresponding to the QueueSender type
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

    /** Get the queue associated with this sender.
      *
      * @return this sender's queue
      *  
      * @exception JMSException if JMS fails to get queue for
      *                         this queue sender
      *                         due to some internal error.
      */
	virtual QueueRef getQueue();

    /** Set the sender's default priority.
      *  
      * <P>Priority is set to 4, by default.
      *
      * @param priority the message priority for this message producer.
      *  
      * @exception JMSException if JMS fails to set priority
      *                         due to some internal error.
      */ 
    virtual void setPriority(jint priority);

    /** Set the producer's default delivery mode.
      *  
      * <P>Delivery mode is set to PERSISTENT by default.
      *
      * @param deliveryMode the message delivery mode for this message
      * producer.
      *  
      * @exception JMSException if JMS fails to set delivery mode
      *                         due to some internal error.          
      * @exception javax.jms.IllegalStateException if the delivery mode is set
      *                         to DISCARDABLE.
      */ 
    virtual void setDeliveryMode(jint deliveryMode);
 
    /** Send a Message to the queue
      * Use the queues default delivery mode, timeToLive and priority. 
      *
      * @param message the message to send
      *
      * @exception JMSException if JMS fails to send the message
      *                         due to some internal error.
      * @exception javax.jms.MessageFormatException if invalid message specified
      * @exception InvalidDestinationException if a client uses
      *                         this method with a Queue Sender with
      *                         an invalid queue.
      */
    virtual void send(MessageRef message);

    /** Send a Message to the queue specifying delivery mode, priority 
      * and time to live to the queue.
      *
      * @param message the message to send
      * @param deliveryMode the delivery mode to use
      * @param priority the priority for this message
      * @param timeToLive the message's lifetime (in milliseconds).
      *
      * @exception JMSException if JMS fails to send the message
      *                         due to some internal error.
      * @exception javax.jms.MessageFormatException if invalid message specified
      * @exception InvalidDestinationException if a client uses
      *                         this method with a Queue Sender with
      *                         an invalid queue.
      * @exception javax.jms.IllegalStateException if the delivery mode is set
      *                         to DISCARDABLE.
      */
    virtual void send(
		MessageRef message, 
        jint deliveryMode, 
	    jint priority,
	    jlong timeToLive);

    /** Send a Message to a queue for an unidentified message producer.	
      * Use the queues default delivery mode, timeToLive and priority.
      *  
      * <P>Typically a JMS message producer is assigned a queue at creation 
      * time; however, JMS also supports unidentified message producers 
      * which require that the queue be supplied on every message send.
      *
      * @param queue the queue to send this message to
      * @param message the message to send
      *  
      * @exception JMSException if JMS fails to send the message
      *                         due to some internal error.
      * @exception javax.jms.MessageFormatException if invalid message specified
      * @exception InvalidDestinationException if a client uses
      *                         this method with an invalid queue.
      */ 
    virtual void send(QueueRef queue, MessageRef message); 

    /** Send a Message to a queue for an unidentified message producer,
      * specifying delivery mode, priority and time to live.
      *  
      * <P>Typically a JMS message producer is assigned a queue at creation
      * time; however, JMS also supports unidentified message producers
      * which require that the queue be supplied on every message send.
      *
      * @param queue the queue to send this message to
      * @param message the message to send
      * @param deliveryMode the delivery mode to use
      * @param priority the priority for this message
      * @param timeToLive the message's lifetime (in milliseconds).
      *  
      * @exception JMSException if JMS fails to send the message
      *                         due to some internal error.
      * @exception javax.jms.MessageFormatException if invalid message specified
      * @exception InvalidDestinationException if a client uses
      *                         this method with an invalid Queue.
      * @exception javax.jms.IllegalStateException if the delivery mode is set
      *                         to DISCARDABLE.
      */ 
    virtual void send(
		QueueRef queue, 
        MessageRef message, 
        jint deliveryMode, 
        jint priority,
        jlong timeToLive);
    
    /** Remove sender associated with this Queue.
      *
      * Since the C-Client may allocate some resources on behalf of a message producer,
      * clients should close them when they are not needed.
      *
      * @exception JMSException if JMS fails to close remove sender 
      *                         due to some internal error.      
      */ 
    virtual void close();

    private: void PMJQSENDreserved0();
    private: void PMJQSENDreserved1();
};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_QUEUESENDER_H_
