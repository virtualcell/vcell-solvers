#ifndef _PROGRESS_MESSAGE_JCLIENT_SESSION_H_
#define _PROGRESS_MESSAGE_JCLIENT_SESSION_H_
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
#include <progress/message/jclient/package_decls.h>
#include <progress/message/jclient/MessageListener.h>
#include <progress/message/jclient/StreamMessage.h>


namespace progress { namespace message { namespace jclient {

class SMQ_API Session : public java::lang::Object
{
public:
    virtual ~Session();

    /**
     * Returns the int corresponding to the Session type.
     *
     * @return the int corresponding to the Session type
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

    /** Create a BytesMessage. A BytesMessage is used to send a message
      * containing a stream of uninterpreted bytes.
      *
      * @exception IllegalStateException if session is closing.
      * @exception JMSException if JMS fails to create this message
      *                         due to some internal error.
      */
    virtual BytesMessageRef createBytesMessage();

    /** Create a BytesMessage. A BytesMessage is used to send a message
      * containing a stream of uninterpreted bytes.
      *
	  * @param preAllocatedSize, a preallocated size for the Bytes Message
	  * @param growthSize, the size by which the bytes message with grow when
      * the preAllocatedSize is reached.
  	  * 
      * @exception IllegalStateException if session is closing.
      * @exception JMSException if JMS fails to create this message
      *                         due to some internal error.
      */
    virtual BytesMessageRef createBytesMessage(int preAllocatedSize, int growthSize);

    /** Create a Message. The Message interface is the root interface of
      * all JMS messages. It holds all the standard message header
      * information. It can be sent when a message containing only header
      * information is sufficient.
      *
      * @exception IllegalStateException if session is closing.
      * @exception JMSException if JMS fails to create this message
      *                         due to some internal error.
      */
    virtual MessageRef createMessage();

    /** Create a TextMessage. A TextMessage is used to send a message
      * containing a String.
      *
      * @exception IllegalStateException if session is closing.
      * @exception JMSException if JMS fails to create this message
      *                         due to some internal error.
      */
    virtual TextMessageRef createTextMessage();
 
    /** Create a StreamMessage.
      *
      * @exception IllegalStateException if session is closing.
      * @exception JMSException if JMS fails to create this message
      *                         due to some internal error.
      */
    virtual StreamMessageRef createStreamMessage();
    
    /** Create an initialized TextMessage. A TextMessage is used to send
      * a message containing a String.
      *
      * @param text the text used to initialize this message.
      *
      * @exception IllegalStateException if session is closing.
      * @exception JMSException if JMS fails to create this message
      *                         due to some internal error.
      */
    virtual TextMessageRef createTextMessage(StringRef text);

    /** Is the session in transacted mode?
      *
      * @return true if in transacted mode
      *
      * @exception IllegalStateException if session is closing.
      * @exception JMSException if JMS fails to return the transaction
      *                         mode due to internal error in JMS Provider.
      */
    virtual jboolean getTransacted();

    /** Commit all messages done in this transaction and release any locks
      * currently held.
      *
      * @exception JMSException if JMS implementation fails to commit the
      *                         the transaction due to some internal error.
      * @exception TransactionRolledBackException  if the transaction
      *                         gets rolled back due to some internal error
      *                         during commit.
      * @exception IllegalStateException if commit() is called on a 
      *                         non-transacted session
      */
    virtual void commit(String * transactionId = null, jint lifespan = 0);  // synchronized

    /** Rollback any messages done in this transaction and releases any locks
      * currently held.
      *
      * @exception JMSException if JMS implementation fails to rollback the
      *                         the transaction due to some internal error.
      * @exception IllegalStateException if rollback() called in a non-transacted
      *                         session
      */
    virtual void rollback();    // synchronized
 
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

    /** Stop message delivery in this session, and restart sending messages
      * with the oldest unacknowledged message.
      *
      * <P>All consumers deliver messages in a serial order.
      * Acknowledging a received message automatically acknowledges all
      * messages that have been delivered to the client in the current session.
      *
      * <P>Restarting a session causes it to take the following actions:
      *
      * <UL>
      *   <LI>Stop message delivery
      *   <LI>Mark all messages that might have been delivered but not
      *       acknowledged as `redelivered'
      *   <LI>Restart the delivery sequence including all unacknowledged
      *       messages that had been previously delivered.
      *
      *          <P>Redelivered messages do not have to be delivered in
      *             exactly their original delivery order.
      * </UL>
      *
      * @exception JMSException if JMS implementation fails to stop message
      *                         delivery and restart message send due to
      *                         due to some internal error.
      * @exception IllegalStateException if recover() is called in a transacted
      *                         session or while session closing
      */
    virtual void recover();

    /** Sets the transaction batch size of this session.
      * The batch size specifies the amount of information to be batched on the 
      * client.  This is a performance optimization utilized on transacted sessions.
      * The size parameter is a measure of the message payloads only.
      *
      * @param size  The number of bytes to batch, specified as a jint.
      */
    virtual void setTxnBatchSize( jint size );

    /** Gets the session's transaction batch size
      *
      * @return The number of bytes to batch, specified as an int.
      */
    virtual jint getTxnBatchSize();

    /**
     * Specifies whether the batching of ACKs will be permitted in the session.
     * @param enabled
     * @throws JMSException
     */
    virtual void setAckBatchingEnabled( bool enabled );
    
    /** Gets whether ack batching is enabled in the session.
      *
      * @return Ack batching enabled.
      */
    virtual bool getAckBatchingEnabled();

    /** Gets the session acknowlegement mode.
      *
      * @return The sesion acknowledge mode.
      */
    virtual jint getAcknowledgeMode();
    
    private: virtual void reservedv1();
    private: virtual void reservedv2();
    private: virtual void reservedv3();
    private: void PMJSESSreserved0();
    private: void PMJSESSreserved1();
    private: void PMJSESSreserved2();
    private: void PMJSESSreserved3();
    private: void PMJSESSreserved4();
    private: void PMJSESSreserved5();
    private: void PMJSESSreserved6();
    private: void PMJSESSreserved7();
    private: void PMJSESSreserved8();
    private: void PMJSESSreserved9();
    private: void PMJSESSreserved10();
    private: void PMJSESSreserved11();
    private: void PMJSESSreserved12();
    private: void PMJSESSreserved13();
    private: void PMJSESSreserved14();
    private: void PMJSESSreserved15();
    private: void PMJSESSreserved16();
    private: void PMJSESSreserved17();
    private: void PMJSESSreserved18();
    private: void PMJSESSreserved19();
    private: void PMJSESSreserved20();
    private: void PMJSESSreserved21();
    private: void PMJSESSreserved22();
    private: void PMJSESSreserved23();
    private: void PMJSESSreserved24();
    private: void PMJSESSreserved25();
    private: void PMJSESSreserved26();
    private: void PMJSESSreserved27();
    private: void PMJSESSreserved28();
    private: void PMJSESSreserved29();
    private: void PMJSESSreserved30();
    private: void PMJSESSreserved31();
    private: void PMJSESSreserved32();
    private: void PMJSESSreserved33();
    private: void PMJSESSreserved34();
    private: void PMJSESSreserved35();
    private: void PMJSESSreserved36();
    private: void PMJSESSreserved37();
    private: void PMJSESSreserved38();
    private: void PMJSESSreserved39();
    private: void PMJSESSreserved40();
};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_SESSION_H_
