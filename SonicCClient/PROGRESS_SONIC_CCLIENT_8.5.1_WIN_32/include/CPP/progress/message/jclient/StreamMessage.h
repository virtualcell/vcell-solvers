/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#ifndef _PROGRESS_MESSAGE_JCLIENT_StreamMessage_H_
#define _PROGRESS_MESSAGE_JCLIENT_StreamMessage_H_

#include <java/lang/package.h>
#include <progress/message/jclient/package_decls.h>

/**
 *  A StreamMessage is used to send a stream of Java primitives.
 * It is filled and read sequentially. It inherits from <CODE>Message</CODE>
 * and adds a stream message body. It's methods are based largely on those
 * found in <CODE>java.io.DataInputStream</CODE> and
 * <CODE>java.io.DataOutputStream</CODE>.
 *
 * <P>The primitive types can be read or written explicitly using methods
 * for each type. They may also be read or written generically as objects.
 * For instance, a call to <CODE>StreamMessage.writeInt(6)</CODE> is
 * equivalent to <CODE>StreamMessage.writeObject(new Integer(6))</CODE>.
 * Both forms are provided because the explicit form is convenient for
 * static programming and the object form is needed when types are not known
 * at compile time.
 *
 * <P>When the message is first created, and when <CODE>clearBody</CODE>
 * is called, the body of the message is in write-only mode. After the
 * first call to <CODE>reset</CODE> has been made, the message is in
 * read-only mode. When a message has been sent, by definition, the
 * provider calls <CODE>reset</CODE> in order to read it's content, and
 * when a message has been received, the provider has called
 * <CODE>reset</CODE> so that the message is in read-only mode for the client.
 *
 * <P>If <CODE>clearBody</CODE> is called on a message in read-only mode,
 * the message body is cleared and the message is in write-only mode.
 *
 * <P>If a client attempts to read a message in write-only mode, a
 * MessageNotReadableException is thrown.
 *
 * <P>If a client attempts to write a message in read-only mode, a
 * MessageNotWriteableException is thrown.
 *
 * <P>Stream messages support the following conversion table. The marked cases
 * must be supported. The unmarked cases must throw a JMSException. The
 * String to primitive conversions may throw a runtime exception if the
 * primitives <CODE>valueOf()</CODE> method does not accept it as a valid
 * String representation of the primitive.
 *
 * <P>A value written as the row type can be read as the column type.
 *
 * <PRE>
 * |        | boolean byte short char int long float double String byte[]
 * |----------------------------------------------------------------------
 * |boolean |    X                                            X
 * |byte    |          X     X         X   X                  X
 * |short   |                X         X   X                  X
 * |char    |                     X                           X
 * |int     |                          X   X                  X
 * |long    |                              X                  X
 * |float   |                                    X     X      X
 * |double  |                                          X      X
 * |String  |    X     X     X    X    X   X     X     X      X
 * |byte[]  |                                                        X
 * |----------------------------------------------------------------------
 * </PRE>
 *
 * @version     1.0  7-Mar-99
 * @author      Giovanni Boschi
 *
 * @see         javax.jms.BytesMessage
 * @see         javax.jms.MapMessage
 * @see         javax.jms.Message
 * @see         javax.jms.ObjectMessage
 * @see         javax.jms.TextMessage
 * @see         java.io.DataInputStream
 * @see         java.io.DataOutputStream
 */

#include <progress/message/jclient/Message.h>

#if defined(WIN32) // Windows compiler bug workaround
#define CLONE_STREAM_RET_TYPE Object
#else
#define CLONE_STREAM_RET_TYPE StreamMessage
#endif


namespace progress { namespace message { namespace jclient {

SMQ_API StreamMessageRef createStreamMessage();

class SMQ_API StreamMessage : public Message
{


public:
       /** Clone this StreamMessage
        *
        * @return clone of StreamMessage
        */
       virtual CLONE_STREAM_RET_TYPE * clone();

       /** Read a <code>boolean</code> from the stream message.
        *
        * @return the <code>boolean</code> value read.
        *
        * @exception JMSException if JMS fails to read message due to
        *                         some internal JMS error.
        * @exception MessageEOFException if an end of message stream
        * @exception MessageFormatException if this type conversion is invalid
        * @exception MessageNotReadableException if message in write-only mode.
        */
       virtual jboolean readBoolean();

       /** Read a byte value from the stream message.
        *
        * @return the next byte from the stream message as a 8-bit
        * <code>byte</code>.
        *
        * @exception JMSException if JMS fails to read message due to
        *                         some internal JMS error.
        * @exception MessageEOFException if an end of message stream
        * @exception MessageFormatException if this type conversion is invalid
        * @exception MessageNotReadableException if message in write-only mode.
        */
       virtual jbyte readByte();

       /** Read a 16-bit number from the stream message.
        *
        * @return a 16-bit number from the stream message.
        *
        * @exception JMSException if JMS fails to read message due to
        *                         some internal JMS error.
        * @exception MessageEOFException if an end of message stream
        * @exception MessageFormatException if this type conversion is invalid
        * @exception MessageNotReadableException if message in write-only mode.
        */
       virtual jshort readShort();

       /** Read a Unicode character value from the stream message.
        *
        * @return a Unicode character from the stream message.
        *
        * @exception JMSException if JMS fails to read message due to
        *                         some internal JMS error.
        * @exception MessageEOFException if an end of message stream
        * @exception MessageFormatException if this type conversion is invalid
        * @exception MessageNotReadableException if message in write-only mode.
        */
       virtual jchar readChar();

       /** Read a 32-bit integer from the stream message.
        *
        * @return a 32-bit integer value from the stream message, interpreted
        * as a <code>int</code>.
        *
        * @exception JMSException if JMS fails to read message due to
        *                         some internal JMS error.
        * @exception MessageEOFException if an end of message stream
        * @exception MessageFormatException if this type conversion is invalid
        * @exception MessageNotReadableException if message in write-only mode.
        */
       virtual jint readInt();

       /** Read a 64-bit integer from the stream message.
        *
        * @return a 64-bit integer value from the stream message, interpreted as
        * a <code>long</code>.
        *
        * @exception JMSException if JMS fails to read message due to
        *                         some internal JMS error.
        * @exception MessageEOFException if an end of message stream
        * @exception MessageFormatException if this type conversion is invalid
        * @exception MessageNotReadableException if message in write-only mode.
        */
       virtual jlong readLong();

       /** Read a <code>float</code> from the stream message.
        *
        * @return a <code>float</code> value from the stream message.
        *
        * @exception JMSException if JMS fails to read message due to
        *                         some internal JMS error.
        * @exception MessageEOFException if an end of message stream
        * @exception MessageFormatException if this type conversion is invalid
        * @exception MessageNotReadableException if message in write-only mode.
        * @exception NumberFormatException if the value was originally written as a String that cannot
        *             be parsed into a Float
        */
       virtual jfloat readFloat();

       /** Read a <code>double</code> from the stream message.
        *
        * @return a <code>double</code> value from the stream message.
        *
        * @exception JMSException if JMS fails to read message due to
        *                         some internal JMS error.
        * @exception MessageEOFException if an end of message stream
        * @exception MessageFormatException if this type conversion is invalid
        * @exception MessageNotReadableException if message in write-only mode.
        * @exception NumberFormatException if the value was originally written as a String that cannot
        *             be parsed into a Float
        */
       virtual jdouble readDouble();

       /** Read in a string from the stream message.
        *
        * @return a Unicode string from the stream message.
        *
        * @exception JMSException if JMS fails to read message due to
        *                         some internal JMS error.
        * @exception MessageEOFException if an end of message stream
        * @exception MessageFormatException if this type conversion is invalid
        * @exception MessageNotReadableException if message in write-only mode.
        */
       virtual String * readString();

       /** Read a byte array from the stream message.
        *
        * @param value the buffer into which the data is read.
        *
        * @return the total number of bytes read into the buffer, or -1 if
        * there is no more data because the end of the stream has been reached.
        *
        * @exception JMSException if JMS fails to read message due to
        *                         some internal JMS error.
        * @exception MessageEOFException if an end of message stream
        * @exception MessageFormatException if this type conversion is invalid
        * @exception MessageNotReadableException if message in write-only mode.
        */
       virtual jint readBytes(jbyteArray * value);

       /** Read a Java object from the stream message.
        *
        * <P>Note that this method can be used to return in objectified format,
        * an object that had been written to the Stream with the equivalent
        * <CODE>writeObject</CODE> method call, or it's equivalent primitive
        * write<type> method.
        *
        * @return a Java object from the stream message, in objectified
        * format (ie. if it set as an int, then a Integer is returned).
        *
        * @exception JMSException if JMS fails to read message due to
        *                         some internal JMS error.
        * @exception MessageEOFException if an end of message stream
        * @exception MessageNotReadableException if message in write-only mode.
        */
       virtual Object * readObject();

       /** Write a <code>boolean</code> to the stream message.
        * The value <code>true</code> is written out as the value
        * <code>(byte)1</code>; the value <code>false</code> is written out as
        * the value <code>(byte)0</code>.
        *
        * @param value the <code>boolean</code> value to be written.
        *
        * @exception JMSException if JMS fails to read message due to
        *                         some internal JMS error.
        * @exception MessageNotWriteableException if message in read-only mode.
        */
       virtual void writeBoolean(jboolean value);

       /** Write out a <code>byte</code> to the stream message.
        *
        * @param value the <code>byte</code> value to be written.
        * @exception JMSException if JMS fails to write message due to
        *                         some internal JMS error.
        * @exception MessageNotWriteableException if message in read-only mode.
        */
       virtual void writeByte(jbyte value);

       /** Write a <code>short</code> to the stream message.
        *
        * @param value the <code>short</code> to be written.
        *
        * @exception JMSException if JMS fails to write message due to
        *                         some internal JMS error.
        * @exception MessageNotWriteableException if message in read-only mode.
        */
       virtual void writeShort(jshort value);

       /** Write a <code>char</code> to the stream message.
        *
        * @param value the <code>char</code> value to be written.
        *
        * @exception JMSException if JMS fails to write message due to
        *                         some internal JMS error.
        * @exception MessageNotWriteableException if message in read-only mode.
        */
       virtual void writeChar(jchar value);

       /** Write an <code>int</code> to the stream message.
        *
        * @param value the <code>int</code> to be written.
        *
        * @exception JMSException if JMS fails to write message due to
        *                         some internal JMS error.
        * @exception MessageNotWriteableException if message in read-only mode.
        */
       virtual void writeInt(jint value);

       /** Write a <code>long</code> to the stream message.
        *
        * @param value the <code>long</code> to be written.
        *
        * @exception JMSException if JMS fails to write message due to
        *                         some internal JMS error.
        * @exception MessageNotWriteableException if message in read-only mode.
        */
       virtual void writeLong(jlong value);

       /** Write a <code>float</code> to the stream message.
        *
        * @param value the <code>float</code> value to be written.
        *
        * @exception JMSException if JMS fails to write message due to
        *                         some internal JMS error.
        * @exception MessageNotWriteableException if message in read-only mode.
        */
       virtual void writeFloat(jfloat value);

       /** Write a <code>double</code> to the stream message.
        *
        * @param value the <code>double</code> value to be written.
        *
        * @exception JMSException if JMS fails to write message due to
        *                         some internal JMS error.
        * @exception MessageNotWriteableException if message in read-only mode.
        */
       virtual void writeDouble(jdouble value);

       /** Write a string to the stream message.
        *
        * @param value the <code>String</code> value to be written.
        *
        * @exception JMSException if JMS fails to write message due to
        *                         some internal JMS error.
        * @exception MessageNotWriteableException if message in read-only mode.
        */
       virtual void writeString(String * value);

       /** Write a byte array to the stream message.
        *
        * @param value the byte array to be written.
        *
        * @exception JMSException if JMS fails to write message due to
        *                         some internal JMS error.
        * @exception MessageNotWriteableException if message in read-only mode.
        */
       virtual void writeBytes(jbyteArray * value);

       /** Write a portion of a byte array to the stream message.
        *
        * @param value the byte array value to be written.
        * @param offset the initial offset within the byte array.
        * @param length the number of bytes to use.
        *
        * @exception JMSException if JMS fails to write message due to
        *                         some internal JMS error.
        * @exception MessageNotWriteableException if message in read-only mode.
        */
       virtual void writeBytes(jbyteArray * value, int offset, int length);

       /** Write a Java object to the stream message.
        *
        * <P>Note that this method only works for the objectified primitive
        * object types (Integer, Double, Long ...), String's and byte arrays.
        *
        * @param value the Java object to be written.
        *
        * @exception JMSException if JMS fails to write message due to
        *                         some internal JMS error.
        * @exception MessageNotWriteableException if message in read-only mode.
        * @exception MessageFormatException if the object is invalid
        */
       virtual void writeObject(Object * value);

       /** Put the message in read-only mode, and reposition the stream
        * to the beginning.
        *
        * @exception JMSException if JMS fails to reset the message due to
        *                         some internal JMS error.
        * @exception MessageFormatException if message has an invalid
        *                         format
        */
       virtual void reset();

       /** Clear out the message body. All other parts of the message are left
        * untouched.
        *
        * @exception JMSException if JMS fails to due to some internal JMS error.
        */
       virtual void clearBody();

    private: virtual void reservedv1();


public:

    /**
     * Returns the int corresponding to the StreamMessage type.
     *
     * @return the int corresponding to the StreamMessage type
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

};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_StreamMessage_H_


