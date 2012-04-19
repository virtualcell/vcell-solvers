#ifndef _PROGRESS_MESSAGE_JCLIENT_MESSAGE_H_
#define _PROGRESS_MESSAGE_JCLIENT_MESSAGE_H_
/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#include <progress/message/jclient/package_decls.h>
#include <java/lang/package.h>
#include <java/util/package_decls.h>
using java::util::EnumerationRef;
using java::util::HashtableRef;

#if defined(WIN32) // Windows compiler bug workaround
#define CLONE_MESSAGE_RET_TYPE Object
#else
#define CLONE_MESSAGE_RET_TYPE Message
#endif



namespace progress { namespace message { namespace jclient {

SMQ_API Message * createMessage();

class SMQ_API Message : public java::lang::Object // implements javax.jms.Message, java.io.Serializable, Cloneable
{


public:
    virtual ~Message();

    /** Clone this Message
      *
      * @return clone of Message
      */
    virtual CLONE_MESSAGE_RET_TYPE * clone();

    /** Compares two Objects for equality.  Returns true if obj is an instance Message,
      * or subclass of Message, and their JMSMessageIDs are equal.
      *
      * <P>Use of this routine should be avoided if at all possible, as the behavior
      * is very different from other equals methods. Note that Message IDs are only
      * created when a message is sent.  using this method on a message that has not
      * yet been sent could lead to unexpected results.
      *
      * @param ojb - the reference object with which to compare.
      *
      * @return true if this object is the same as the obj argument, false otherwise.
      */
    virtual jboolean equals(ObjectRef obj) const;

    /**
     * Returns the int corresponding to the Message type.
     *
     * @return the int corresponding to the Message type
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

    /** Get the message ID.
      *
      * <P>The messageID header field contains a value that uniquely
      * identifies each message sent by a provider.
      *
      * <P>When a message is sent, messageID can be ignored. When
      * the send method returns it contains a provider-assigned value.
      *
      * <P>A JMSMessageID is a String value which should function as a
      * unique key for identifying messages in a historical repository.
      * The exact scope of uniqueness is provider defined. It should at
      * least cover all messages for a specific installation of a
      * provider where an installation is some connected set of message
      * routers.
      *
      * <P>All JMSMessageID values must start with the prefix `ID:'.
      * Uniqueness of message ID values across different providers is
      * not required.
      *
      * <P>Since message ID's take some effort to create and increase a
      * message's size, some JMS providers may be able to optimize message
      * overhead if they are given a hint that message ID is not used by
      * an application. JMS message Producers provide a hint to disable
      * message ID. When a client sets a Producer to disable message ID
      * they are saying that they do not depend on the value of message
      * ID for the messages it produces. These messages must either have
      * message ID set to null or, if the hint is ignored, messageID must
      * be set to its normal unique value.
      *
      * @return the message ID
      *
      * @exception JMSException if JMS fails to get the message Id
      *                         due to internal JMS error.
      */
    virtual StringRef getJMSMessageID() ;
    /** Set the message ID.
      *
      * <P>Providers set this field when a message is sent. This operation
      * can be used to change the value of a message that's been received.
      *
      * @param id the ID of the message
      *
      * @exception JMSException if JMS fails to set the message Id
      *                         due to internal JMS error.
      */
    virtual void setJMSMessageID(StringRef id) ;
    /** Get the message timestamp.
      *
      * <P>The JMSTimestamp header field contains the time a message was
      * handed off to a provider to be sent. It is not the time the
      * message was actually transmitted because the actual send may occur
      * later due to transactions or other client side queueing of messages.
      *
      * <P>When a message is sent, JMSTimestamp is ignored. When the send
      * method returns it contains a a time value somewhere in the interval
      * between the call and the return. It is in the format of a normal
      * Java millis time value.
      *
      * <P>Since timestamps take some effort to create and increase a
      * message's size, some JMS providers may be able to optimize message
      * overhead if they are given a hint that timestamp is not used by an
      * application. JMS message Producers provide a hint to disable
      * timestamps. When a client sets a producer to disable timestamps
      * they are saying that they do not depend on the value of timestamp
      * for the messages it produces. These messages must either have
      * timestamp set to null or, if the hint is ignored, timestamp must
      * be set to its normal value.
      *
      * @return the message timestamp
      *
      * @exception JMSException if JMS fails to get the Timestamp
      *                         due to internal JMS error.
      */
    virtual jlong getJMSTimestamp() ;
    /** Set the message timestamp.
      *
      * <P>Providers set this field when a message is sent. This operation
      * can be used to change the value of a message that's been received.
      *
      * @param timestamp the timestamp for this message
      *
      * @exception JMSException if JMS fails to set the timestamp
      *                         due to some internal JMS error.
      */
    virtual void setJMSTimestamp(jlong timestamp) ;
    /** Get the correlation ID as an array of bytes for the message.
      *
      * <P>The use of a byte[] value for JMSCorrelationID is non-portable.
      *
      * @return the correlation ID of a message as an array of bytes.
      *
      * @exception JMSException if JMS fails to get correlationId
      *                         due to some internal JMS error.
      */
    virtual jbyteArrayRef getJMSCorrelationIDAsBytes() ;
    /** Set the correlation ID as an array of bytes for the message.
      *
      * <P>If a provider supports the native concept of correlation id, a
      * JMS client may need to assign specific JMSCorrelationID values to
      * match those expected by non-JMS clients. JMS providers without native
      * correlation id values are not required to support this (and the
      * corresponding get) method; their implementation may throw
      * java.lang.UnsupportedOperationException).
      *
      * <P>The use of a byte[] value for JMSCorrelationID is non-portable.
      *
      * @param correlationID the correlation ID value as an array of bytes.
      *
      * @exception JMSException if JMS fails to set correlationId
      *                         due to some internal JMS error.
      */
    virtual void setJMSCorrelationIDAsBytes(jbyteArrayRef correlationID) ;
    /** Set the correlation ID for the message.
      *
      * <P>A client can use the JMSCorrelationID header field to link one
      * message with another. A typically use is to link a response message
      * with its request message.
      *
      * <P>JMSCorrelationID can hold one of the following:
      *    <UL>
      *      <LI>A provider-specific message ID
      *      <LI>An application-specific String
      *      <LI>A provider-native byte[] value.
      *    </UL>
      *
      * <P>Since each message sent by a JMS provider is assigned a message ID
      * value it is convenient to link messages via message ID. All message ID
      * values must start with the `ID:' prefix.
      *
      * <P>In some cases, an application (made up of several clients) needs to
      * use an application specific value for linking messages. For instance,
      * an application may use JMSCorrelationID to hold a value referencing
      * some external information. Application specified values must not start
      * with the `ID:' prefix; this is reserved for provider-generated message
      * ID values.
      *
      * <P>If a provider supports the native concept of correlation ID, a JMS
      * client may need to assign specific JMSCorrelationID values to match
      * those expected by non-JMS clients. A byte[] value is used for this
      * purpose. JMS providers without native correlation ID values are not
      * required to support byte[] values. The use of a byte[] value for
      * JMSCorrelationID is non-portable.
      *
      * @param correlationID the message ID of a message being referred to.
      *
      * @exception JMSException if JMS fails to set correlationId
      *                         due to some internal JMS error.
      */
    virtual void setJMSCorrelationID(StringRef correlationID) ;
    /** Get the correlation ID for the message.
      *
      * <P>This method is used to return correlation id values that are
      * either provider-specific message ID's or application-specific Strings.
      *
      * @return the correlation ID of a message as a String.
      *
      * @exception JMSException if JMS fails to get correlationId
      *                         due to some internal JMS error.
      */
    virtual StringRef getJMSCorrelationID() ;
    /** Get where a reply to this message should be sent.
      *
      * @return where to send a response to this message
      *
      * @exception JMSException if JMS fails to get ReplyTo Destination
      *                         due to some internal JMS error.
      */
    virtual DestinationRef getJMSReplyTo() ;
    /** Set where a reply to this message should be sent.
      *
      * <P>The replyTo header field contains the destination where a reply
      * to the current message should be sent. If it is null no reply is
      * expected. The destination may be either a Queue or a Topic.
      *
      * <P>Messages with a null replyTo value are called JMS datagrams.
      * Datagrams may be a notification of some change in the sender (i.e.
      * they signal a sender event) or they may just be some data the sender
      * thinks is of interest.
      *
      * Messages with a replyTo value are typically expecting a response.
      * A response may be optional, it is up to the client to decide. These
      * messages are called JMS requests. A message sent in response to a
      * request is called a reply.
      *
      * In some cases a client may wish to match up a request it sent earlier
      * with a reply it has just received. This can be done using the
      * correlationID.
      *
      * @param replyTo where to send a response to this message
      *
      * @exception JMSException if JMS fails to set ReplyTo Destination
      *                         due to some internal JMS error.
      */
    virtual void setJMSReplyTo(DestinationRef replyTo) ;
    /** Get the destination for this message.
      *
      * <P>The destination field contains the destination to which the
      * message is being sent.
      *
      * <P>When a message is sent this value is ignored. After completion
      * of the send method it holds the destination specified by the send.
      *
      * <P>When a message is received, its destination value must be
      * equivalent to the value assigned when it was sent.
      *
      * @return the destination of this message.
      *
      * @exception JMSException if JMS fails to get JMS Destination
      *                         due to some internal JMS error.
      */
    virtual DestinationRef getJMSDestination() ;
    /** Set the destination for this message.
      *
      * <P>Providers set this field when a message is sent. This operation
      * can be used to change the value of a message that's been received.
      *
      * @param destination the destination for this message.
      *
      * @exception JMSException if JMS fails to set JMS Destination
      *                         due to some internal JMS error.
      */
    virtual void setJMSDestination(DestinationRef destination) ;
    /** Get the delivery mode for this message.
      *
      * @return the delivery mode of this message.
      *
      * @exception JMSException if JMS fails to get JMS DeliveryMode
      *                         due to some internal JMS error.
      */
    virtual jint getJMSDeliveryMode() ;
    /** Set the delivery mode for this message.
      *
      * <P>Providers set this field when a message is sent. This operation
      * can be used to change the value of a message that's been received.
      *
      * @param deliveryMode the delivery mode for this message.
      *
      * @exception JMSException if JMS fails to set JMS DeliveryMode
      *                         due to some internal JMS error.
      */
    virtual void setJMSDeliveryMode(jint deliveryMode) ;
    /** Get an indication of whether this message is being redelivered.
      *
      * <P>If a client receives a message with the redelivered indicator set,
      * it is likely, but not guaranteed, that this message was delivered to
      * the client earlier but the client did not acknowledge its receipt at
      * that earlier time.
      *
      * @return set to true if this message is being redelivered.
      *
      * @exception JMSException if JMS fails to get JMS Redelivered flag
      *                         due to some internal JMS error.
      */
    virtual jboolean getJMSRedelivered() ;
    /** Set to indicate whether this message is being redelivered.
      *
      * <P>This field is set at the time the message is delivered. This
      * operation can be used to change the value of a message that's
      * been received.
      *
      * @param redelivered an indication of whether this message is being
      * redelivered.
      *
      * @exception JMSException if JMS fails to set JMS Redelivered flag
      *                         due to some internal JMS error.
      */
    virtual void setJMSRedelivered(jboolean redelivered) ;
    /** Get the message type.
      *
      * @return the message type
      *
      * @exception JMSException if JMS fails to get JMS message type
      *                         due to some internal JMS error.
      */
    virtual StringRef getJMSType() ;
    /** Set the message type.
      *
      * <P>Some JMS providers use a message repository that contains the
      * definition of messages sent by applications. The type header field
      * contains the name of a message's definition.
      *
      * <P>JMS does not define a standard message definition repository nor
      * does it define a naming policy for the definitions it contains. JMS
      * clients should use symbolic values for type that can be configured
      * at installation time to the values defined in the current providers
      * message repository.
      *
      * <P>JMS clients should assign a value to type whether the application
      * makes use of it or not. This insures that it is properly set for
      * those providers that require it.
      *
      * @param type the class of message
      *
      * @exception JMSException if JMS fails to set JMS message type
      *                         due to some internal JMS error.
      */
    virtual void setJMSType(StringRef type) ;
    /** Get the message's expiration value.
      *
      * <P>When a message is sent, expiration is left unassigned. After
      * completion of the send method, it holds the expiration time of the
      * message. This is the sum of the time-to-live value specified by the
      * client and the GMT at the time of the send.
      *
      * <P>If the time-to-live is specified as zero, expiration is set to
      * zero which indicates the message does not expire.
      *
      * <P>When a message's expiration time is reached, a provider should
      * discard it. JMS does not define any form of notification of message
      * expiration.
      *
      * <P>Clients should not receive messages that have expired; however,
      * JMS does not guarantee that this will not happen.
      *
      * @return the time the message expires. It is the sum of the
      * time-to-live value specified by the client, and the GMT at the
      * time of the send.
      *
      * @exception JMSException if JMS fails to get JMS message expiration
      *                         due to some internal JMS error.
      */
    virtual jlong getJMSExpiration() ;
    /** Set the message's expiration value.
      *
      * <P>Providers set this field when a message is sent. This operation
      * can be used to change the value of a message that's been received.
      *
      * @param expiration the message's expiration time
      *
      * @exception JMSException if JMS fails to set JMS message expiration
      *                         due to some internal JMS error.
      */
    virtual void setJMSExpiration(jlong expiration) ;
    /** Get the message priority.
      *
      * <P>JMS defines a ten level priority value with 0 as the lowest
      * priority and 9 as the highest. In addition, clients should consider
      * priorities 0-4 as gradations of normal priority and priorities 5-9
      * as gradations of expedited priority.
      *
      * <P>JMS does not require that a provider strictly implement priority
      * ordering of messages; however, it should do its best to deliver
      * expedited messages ahead of normal messages.
      *
      * @return the default message priority
      *
      * @exception JMSException if JMS fails to get JMS message priority
      *                         due to some internal JMS error.
      */
    virtual jint getJMSPriority() ;
    /** Set the priority for this message.
      *
      * <P>Providers set this field when a message is sent. This operation
      * can be used to change the value of a message that's been received.
      *
      * @param priority the priority of this message
      *
      * @exception JMSException if JMS fails to set JMS message priority
      *                         due to some internal JMS error.
      */
    virtual void setJMSPriority(jint priority) ;
    /** Clear a message's properties.
      *
      * @exception JMSException if JMS fails to clear JMS message
      *                         properties due to some internal JMS
      *                         error.
      */
    virtual void clearProperties() ;
    /** Check if a property value exists.
      *
      * @param name the name of the property to test
      *
      * @return true if the property does exist.
      *
      * @exception JMSException if JMS fails to  check if property
      *                         exists due to some internal JMS
      *                         error.
      */
    virtual jboolean propertyExists(StringRef name) ;
    /** Return the boolean property value with the given name.
      *
      * @param name the name of the boolean property
      *
      * @return the boolean property value with the given name.
      *
      * @exception JMSException if JMS fails to  get Property due to
      *                         some internal JMS error.
      * @exception MessageFormatException if this type conversion is invalid.
      * @exception java.lang.NullPointerException if property does not exist.
      */
    virtual jboolean getBooleanProperty(StringRef name) ;
    /** Return the byte property value with the given name.
      *
      * @param name the name of the byte property
      *
      * @return the byte property value with the given name.
      *
      * @exception JMSException if JMS fails to  get Property due to
      *                         some internal JMS error.
      * @exception MessageFormatException if this type conversion is invalid.
      * @exception java.lang.NullPointerException if property does not exist.
      */
    virtual jbyte getByteProperty(StringRef name) ;
    /** Return the short property value with the given name.
      *
      * @param name the name of the short property
      *
      * @return the short property value with the given name.
      *
      * @exception JMSException if JMS fails to  get Property due to
      *                         some internal JMS error.
      * @exception MessageFormatException if this type conversion is invalid.
      * @exception java.lang.NullPointerException if property does not exist.
      */
    virtual jshort getShortProperty(StringRef name) ;
    /** Return the integer property value with the given name.
      *
      * @param name the name of the integer property
      *
      * @return the integer property value with the given name.
      *
      * @exception JMSException if JMS fails to  get Property due to
      *                         some internal JMS error.
      * @exception MessageFormatException if this type conversion is invalid.
      * @exception java.lang.NullPointerException if property does not exist.
      */
    virtual jint getIntProperty(StringRef name) ;
    /** Return the long property value with the given name.
      *
      * @param name the name of the long property
      *
      * @return the long property value with the given name.
      *
      * @exception JMSException if JMS fails to  get Property due to
      *                         some internal JMS error.
      * @exception MessageFormatException if this type conversion is invalid.
      * @exception java.lang.NullPointerException if property does not exist.
      */
    virtual jlong getLongProperty(StringRef name) ;
    /** Return the float property value with the given name.
      *
      * @param name the name of the float property
      *
      * @return the float property value with the given name.
      *
      * @exception JMSException if JMS fails to  get Property due to
      *                         some internal JMS error.
      * @exception MessageFormatException if this type conversion is invalid.
      * @exception java.lang.NullPointerException if property does not exist.
      */
    virtual jfloat getFloatProperty(StringRef name) ;
    /** Return the double property value with the given name.
      *
      * @param name the name of the double property
      *
      * @return the double property value with the given name.
      *
      * @exception JMSException if JMS fails to  get Property due to
      *                         some internal JMS error.
      * @exception MessageFormatException if this type conversion is invalid.
      * @exception java.lang.NullPointerException if property does not exist.
      */
    virtual jdouble getDoubleProperty(StringRef name) ;
    /** Return the String property value with the given name.
      *
      * @param name the name of the String property
      *
      * @return the String property value with the given name. If there
      * is no property by this name, a null value is returned.
      *
      * @exception JMSException if JMS fails to  get Property due to
      *                         some internal JMS error.
      * @exception MessageFormatException if this type conversion is invalid.
      */
    virtual StringRef getStringProperty(StringRef name) ;
    /** Return the Java object property value with the given name.
      *
      * <P>Note that this method can be used to return in objectified format,
      * an object that had been stored as a property in the Message with the
      * equivalent <CODE>setObject</CODE> method call, or it's equivalent
      * primitive set<type> method.
      *
      * @param name the name of the Java object property
      *
      * @return the Java object property value with the given name, in
      * objectified format (ie. if it set as an int, then a Integer is
      * returned). If there is no property by this name, a null value
      * is returned.
      *
      * @exception JMSException if JMS fails to  get Property due to
      *                         some internal JMS error.
      */
    virtual ObjectRef getObjectProperty(StringRef name) ;
    /** Return an Enumeration of all the property names.
      *
      * @return an enumeration of all the names of property values.
      *
      * @exception JMSException if JMS fails to  get Property names due to
      *                         some internal JMS error.
      */
    virtual EnumerationRef getPropertyNames() ;
    /** Set a boolean property value with the given name, into the Message.
      *
      * @param name the name of the boolean property
      * @param value the boolean property value to set in the Message.
      *
      * @exception JMSException if JMS fails to  set Property due to
      *                         some internal JMS error.
      * @exception MessageNotWriteableException if properties are read-only
      */
    virtual void setBooleanProperty(StringRef name, jboolean value) ;
    /** Set a byte property value with the given name, into the Message.
      *
      * @param name the name of the byte property
      * @param value the byte property value to set in the Message.
      *
      * @exception JMSException if JMS fails to  set Property due to
      *                         some internal JMS error.
      * @exception MessageNotWriteableException if properties are read-only
      */
    virtual void setByteProperty(StringRef name, jbyte value) ;
    /** Set a short property value with the given name, into the Message.
      *
      * @param name the name of the short property
      * @param value the short property value to set in the Message.
      *
      * @exception JMSException if JMS fails to  set Property due to
      *                         some internal JMS error.
      * @exception MessageNotWriteableException if properties are read-only
      */
    virtual void setShortProperty(StringRef name, jshort value) ;
    /** Set an integer property value with the given name, into the Message.
      *
      * @param name the name of the integer property
      * @param value the integer property value to set in the Message.
      *
      * @exception JMSException if JMS fails to  set Property due to
      *                         some internal JMS error.
      * @exception MessageNotWriteableException if properties are read-only
      */
    virtual void setIntProperty(StringRef name, jint value) ;
    /** Set a long property value with the given name, into the Message.
      *
      * @param name the name of the long property
      * @param value the long property value to set in the Message.
      *
      * @exception JMSException if JMS fails to  set Property due to
      *                         some internal JMS error.
      * @exception MessageNotWriteableException if properties are read-only
      */
    virtual void setLongProperty(StringRef name, jlong value) ;
    /** Set a float property value with the given name, into the Message.
      *
      * @param name the name of the float property
      * @param value the float property value to set in the Message.
      *
      * @exception JMSException if JMS fails to  set Property due to
      *                         some internal JMS error.
      * @exception MessageNotWriteableException if properties are read-only
      */
    virtual void setFloatProperty(StringRef name, jfloat value) ;
    /** Set a double property value with the given name, into the Message.
      *
      * @param name the name of the double property
      * @param value the double property value to set in the Message.
      *
      * @exception JMSException if JMS fails to  set Property due to
      *                         some internal JMS error.
      * @exception MessageNotWriteableException if properties are read-only
      */
    virtual void setDoubleProperty(StringRef name, jdouble value) ;
    /** Set a String property value with the given name, into the Message.
      *
      * @param name the name of the String property
      * @param value the String property value to set in the Message.
      *
      * @exception JMSException if JMS fails to  set Property due to
      *                         some internal JMS error.
      * @exception MessageNotWriteableException if properties are read-only
      */
    virtual void setStringProperty(StringRef name, StringRef value) ;
    /** Set a Java object property value with the given name, into the Message.
      *
      * <P>Note that this method only works for the objectified primitive
      * object types (Integer, Double, Long ...) and String's.
      *
      * @param name the name of the Java object property.
      * @param value the Java object property value to set in the Message.
      *
      * @exception JMSException if JMS fails to  set Property due to
      *                         some internal JMS error.
      * @exception MessageFormatException if object is invalid
      * @exception MessageNotWriteableException if properties are read-only
      */
    virtual void setObjectProperty(StringRef name, ObjectRef value) ;
    /** Acknowledge this or all previous messages received by the session.
      *
      * <P>All JMS messages support the acknowledge() method for use when a client has specified
      * that a JMS session's messages are to be explicitly acknowledged. A client requests explicit
      * acknowledgement by creating a session with either the standard JMS CLIENT_ACKKNOWLEDGE mode,
      * or the non-JMS acknowledgement mode SINGLE_MESSAGE_ACKNOWLEDGE.
      *
      * <P>In the standard JMS CLIENT_ACKNOWLEDGE mode, all messages previously received for the
      * session are acknowledged. If the session has been created with SINGLE_MESSAGE_ACKNOWLEDGE,
      * only the current message is acknowledged.
      *
      * <P>JMS defaults to implicit message acknowledgement, AUTO_ACKNOWLEDGE. In this mode,
      * calls to acknowledge() are ignored, as JMS automatically acknowledges messages on behalf
      * of the client.
      *
      * <P>Messages that have been received but not acknowledged may be
      * redelivered to the consumer.
      *
      * @exception JMSException if JMS fails to acknowledge due to some
      *                         internal JMS error.
      */
    virtual void acknowledge() ;
    /** Clear out the message body. All other parts of the message are left
      * untouched.
      *
      * @exception JMSException if JMS fails to due to some internal JMS error.
      */
    virtual void clearBody() ;
    /** Get message properties (Progress Specific Message Interface Extensions)
      *
      * @return the message properties.
      */
    virtual HashtableRef getProperties();

    /** Get the size of the message body  (Progress Specific Message Interface Extensions)
      *
      * @exception JMSException if JMS fails to due to some internal JMS error.
      */
    virtual jint getBodySize() ;

    private: virtual void reservedv1();
    private: void PMJMreserved0();
    private: void PMJMreserved1();
    private: void PMJMreserved2();
    private: void PMJMreserved3();
    private: void PMJMreserved4();
    private: void PMJMreserved5();
    private: void PMJMreserved6();
    private: void PMJMreserved7();
    private: void PMJMreserved8();
    private: void PMJMreserved9();
    private: void PMJMreserved10();
    private: void PMJMreserved11();
    private: void PMJMreserved12();

};

}}} // namespace

#endif // _PROGRESS_MESSAGE_JCLIENT_MESSAGE_H_
