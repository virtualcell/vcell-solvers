#ifndef _PROGRESS_MESSAGE_JCLIENT_MESSAGECONSUMER_H_
#define _PROGRESS_MESSAGE_JCLIENT_MESSAGECONSUMER_H_
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
#include <progress/message/jclient/MessageListener.h>


namespace progress { namespace message { namespace jclient {


/** The parent interface for all message consumers.
  *
  * <P>A client uses a message consumer to receive messages from a Destination.
  * It is created by passing a Destination to a create message consumer method
  * supplied by a Session.
  *
  * <P>A message consumer can be created with a message selector. This allows
  * the client to restrict the messages delivered to the message consumer to
  * those that match the selector.
  *
  * <P>Although a session allows the creation of multiple message consumer's
  * per Destination, it will only deliver each message for a Destination to one
  * message consumer. If more than one message consumer could receive it, the
  * session randomly selects one to deliver it to.
  *
  * <P>A client may either synchronously receive a message consumer's messages
  * or have the consumer asynchronously deliver them as they arrive.
  *
  * <P>A client can request the next message from a message consumer using one
  * of its receive methods. There are several variations of receive that allow a
  * client to poll or wait for the next message.
  *
  * <P>A client can register a MessageListener object with a message consumer.
  * As messages arrive at the message consumer, it delivers them by calling the
  * MessageListener's onMessage method.
  *
  * <P>It is a client programming error for a MessageListener to throw an
  * exception.
  */
class SMQ_API MessageConsumer : public java::lang::Object
{
public:
    virtual ~MessageConsumer();

    /**
     * Returns the int corresponding to the MessageConsumer type.
     *
     * @return the int corresponding to the MessageConsumer type
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

    /** Get this message consumer's message selector expression.
      *
      * @return this message consumer's message selector
      *
      * @exception JMSException if JMS fails to get message
      *                         selector due to some JMS error
      */
    virtual StringRef getMessageSelector();

    /** Get the message consumer's MessageListener.
      *
      * This method is obsoleted by the class based listener getMessageListenerObj.
      * It is retained for backward compatibility.
      *
      * @return the listener function for the message consumer, or null if this isn't
      * one set.
      *
      * @exception JMSException if JMS fails to get message
      *                         listener due to some JMS error
      */
    virtual pfnMessageListener getMessageListener();

    /** Get the message consumer's MessageListener.
      *
      * @return the listener object for the message consumer, or null if this isn't
      * one set.
      *
      * @exception JMSException if JMS fails to get message
      *                         listener due to some JMS error
      */
    virtual MessageListener *getMessageListenerObj();

    /** Set the message consumer's MessageListener function.  This form takes
      * a callback function signature as for a C++ application.
      *
      * This method is obsoleted by the class based listener getMessageListenerObj.
      * It is retained for backward compatibility.
      *
      * @param messageListener the messages are delivered to this listener
      *
      * @exception JMSException if JMS fails to set message
      *                         listener due to some JMS error
      */
    virtual void setMessageListener(pfnMessageListener listener);

    /** Set the message consumer's MessageListener function.  This form takes
      * a MessageListener signature as for a C++ application.
      *
      * @param MessageListener the messages are delivered to this listener
      *
      * @exception JMSException if JMS fails to set message
      *                         listener due to some JMS error
      */
    virtual void setMessageListenerObj(MessageListener *listener);

    /** Receive the next message produced for this message consumer.
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

    /** Receive the next message that arrives within the specified
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
    virtual MessageRef receive(jlong timeout);

    /** Receive the next message if one is immediately available.
      *
      * @exception JMSException if JMS fails to receive the next
      *                     message due to some error.
      * @return the next message produced for this message consumer, or
      * null if one is not available.
      */
    virtual MessageRef receiveNoWait();

    /** 
     * Closes the message consumer.
     *
     * Since the C-Client may allocate some resources on behalf of a message consumer,
     * clients should close them when they are not needed.
     *
     * @exception JMSException if JMS fails to close the consumer
     *                         due to some error.
     */ 
    virtual void close();

    private: virtual void reservedv1();
    private: virtual void reservedv2();
    private: virtual void reservedv3();
    private: virtual void reservedv4();
    private: virtual void reservedv5();
    private: virtual void reservedv6();
    private: virtual void reservedv7();
    private: virtual void reservedv8();
    private: void PMJMCreserved0();
    private: void PMJMCreserved1();
    private: void PMJMCreserved2();
    private: void PMJMCreserved3();
    private: void PMJMCreserved4();
    private: void PMJMCreserved5();
    private: void PMJMCreserved6();
    private: void PMJMCreserved7();
    private: void PMJMCreserved8();
    private: void PMJMCreserved9();
    private: void PMJMCreserved10();
    private: void PMJMCreserved11();
};


}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_MESSAGECONSUMER_H_
