#ifndef _PROGRESS_MESSAGE_JCLIENT_BYTESMESSAGE_H_
#define _PROGRESS_MESSAGE_JCLIENT_BYTESMESSAGE_H_
/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#include <progress/message/jclient/Message.h>

#if defined(WIN32) // Windows compiler bug workaround
#define CLONE_BYTE_RET_TYPE Object
#else
#define CLONE_BYTE_RET_TYPE BytesMessage
#endif


namespace progress { namespace message { namespace jclient {

SMQ_API BytesMessageRef createBytesMessage();
SMQ_API BytesMessageRef createBytesMessage(jint preAllocatedSize, jint growthSize);

/** A BytesMessage is used to send a message containing a stream of 
  * uninterpreted bytes. It inherits <CODE>Message</CODE> and adds a bytes
  * message body. The receiver of the message supplies the interpretation
  * of the bytes.
  *
  * <P>It's methods are based largely on those found in 
  * <CODE>java::io::DataInputStream</CODE> and
  * <CODE>java::io::DataOutputStream</CODE>.
  *
  * <P>This message type is for client encoding of existing message formats. 
  * If possible, one of the other self-defining message types should be used 
  * instead.
  *
  * <P>Although JMS allows the use of message properties with byte messages
  * it is typically not done since the inclusion of properties affects the
  * format.
  *
  * <P>The primitive types can be written explicitly using methods
  * for each type. They may also be written generically as objects.
  * For instance, a call to <CODE>BytesMessage.writeInt(6)</CODE> is
  * equivalent to <CODE>BytesMessage.writeObject(new Integer(6))</CODE>.
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
  * @see         javax::jms::MapMessage
  * @see         javax::jms::Message
  * @see         javax::jms::ObjectMessage
  * @see         javax::jms::StreamMessage
  * @see         javax::jms::TextMessage
  * @see         java::io::InputStream
  * @see         java::io::OutputStream
  */

class SMQ_API BytesMessage : public Message /* implements javax::jms::BytesMessage */
{
    public:
    virtual ~BytesMessage();

    /**
     * Returns the int corresponding to the BytesMessage type.
     *
     * @return the int corresponding to the BytesMessage type
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
     * Creates and returns a copy of this object.  In this case, 
     * the pointer to original envelope is preserved. The general 
     * intent is that, for any object <tt>x</tt>, the expression:
     * <blockquote>
     * <pre>
     * x->clone() != x</pre></blockquote>
     * will be <tt>jtrue<tt>, and that the expression:
     * <blockquote>
     * <pre>
     * x->clone()->getType() == x->getType()</pre></blockquote>
     * will be <tt>jtrue</tt>, but these are not absolute requirements. 
     * While it is typically the case that:
     * <blockquote>
     * <pre>
     * x->clone()->equals(x)</pre></blockquote>
     * will be <tt>jtrue</tt>, this is not an absolute requirement. 
     * Copying an object will typically entail creating a new instance of 
     * its class, but it also may require copying of internal data 
     * structures as well.  No constructors are called.
     * <p>
     * The class <tt>Object</tt> does not itself implement the interface 
     * <tt>Cloneable</tt>, so calling the <tt>clone</tt> method on an object 
     * whose class is <tt>Object</tt> will result in throwing an
     * exception at run time. 
     *
     * @return     a clone of this instance.
     * @exception  CloneNotSupportedExceptionRef  if the object's class does not
     *               support the <code>Cloneable</code> interface. Subclasses
     *               that override the <code>clone</code> method can also
     *               throw this exception to indicate that an instance cannot
     *               be cloned.
     */
    virtual CLONE_BYTE_RET_TYPE * clone();

    /** Read a <code>boolean</code> from the stream message.
      *
      * @return the <code>boolean</code> value read.
      *
      * @exception MessageNotReadableException if message in write-only mode.
      * @exception JMSException if JMS fails to read message due to
      *                         some internal JMS error.
      * @exception MessageEOFException if end of message stream
      */
    virtual jboolean readBoolean() /*:> throws JMSException */;

    /** Read a signed 8-bit value from the stream message.
      *
      * @return the next byte from the stream message as a signed 8-bit
      * <code>byte</code>.
      *
      * @exception MessageNotReadableException if message in write-only mode.
      * @exception MessageEOFException if end of message stream
      * @exception JMSException if JMS fails to read message due to
      *                         some internal JMS error.
      */ 
    virtual jbyte readByte() /*:>  throws JMSException */;

    /** Read an unsigned 8-bit number from the stream message.
      *  
      * @return the next byte from the stream message, interpreted as an
      * unsigned 8-bit number.
      *
      * @exception MessageNotReadableException if message in write-only mode.
      * @exception MessageEOFException if end of message stream
      * @exception JMSException if JMS fails to read message due to
      *                         some internal JMS error.
      */
    virtual jint readUnsignedByte() /*:>  throws JMSException */;

    /** Read a signed 16-bit number from the stream message.
      *
      * @return the next two bytes from the stream message, interpreted as a
      * signed 16-bit number.
      *
      * @exception MessageNotReadableException if message in write-only mode.
      * @exception MessageEOFException if end of message stream
      * @exception JMSException if JMS fails to read message due to
      *                         some internal JMS error.
      */ 
    virtual jshort readShort() /*:>  throws JMSException */;

    /** Read an unsigned 16-bit number from the stream message.
      *  
      * @return the next two bytes from the stream message, interpreted as an
      * unsigned 16-bit integer.
      *
      * @exception MessageNotReadableException if message in write-only mode.
      * @exception MessageEOFException if end of message stream
      * @exception JMSException if JMS fails to read message due to
      *                         some internal JMS error.
      */ 
    virtual jint readUnsignedShort() /*:>  throws JMSException */;

    /** Read a Unicode character value from the stream message.
      *
      * @return the next two bytes from the stream message as a Unicode
      * character.
      *
      * @exception MessageNotReadableException if message in write-only mode.
      * @exception MessageEOFException if end of message stream
      * @exception JMSException if JMS fails to read message due to
      *                         some internal JMS error.
      */ 
    virtual jchar readChar() /*:>  throws JMSException */;

    /** Read a signed 32-bit integer from the stream message.
      *
      * @return the next four bytes from the stream message, interpreted as
      * an <code>int</code>.
      *
      * @exception MessageNotReadableException if message in write-only mode.
      * @exception MessageEOFException if end of message stream
      * @exception JMSException if JMS fails to read message due to
      *                         some internal JMS error.
      */ 
    virtual jint readInt() /*:>  throws JMSException */;

    /** Read a signed 64-bit integer from the stream message.
      *
      * @return the next eight bytes from the stream message, interpreted as
      * a <code>long</code>.
      *
      * @exception MessageNotReadableException if message in write-only mode.
      * @exception MessageEOFException if end of message stream
      * @exception JMSException if JMS fails to read message due to
      *                         some internal JMS error.
      */ 
    virtual jlong readLong() /*:>  throws JMSException */;

    /** Read a <code>float</code> from the stream message.
      *
      * @return the next four bytes from the stream message, interpreted as
      * a <code>float</code>.
      *
      * @exception MessageNotReadableException if message in write-only mode.
      * @exception MessageEOFException if end of message stream
      * @exception JMSException if JMS fails to read message due to
      *                         some internal JMS error.
      */ 
    virtual jfloat readFloat() /*:>  throws JMSException */;

    /** Read a <code>double</code> from the stream message.
      *
      * @return the next eight bytes from the stream message, interpreted as
      * a <code>double</code>.
      *
      * @exception MessageNotReadableException if message in write-only mode.
      * @exception MessageEOFException if end of message stream
      * @exception JMSException if JMS fails to read message due to
      *                         some internal JMS error.
      */ 
    virtual jdouble readDouble() /*:>  throws JMSException */;

    /** Read in a string that has been encoded using a modified UTF-8
      * format from the stream message.
      *
      * <P>For more information on the UTF-8 format, see "File System Safe
      * UCS Transformation Format (FSS_UFT)", X/Open Preliminary Specification,
      * X/Open Company Ltd., Document Number: P316. This information also
      * appears in ISO/IEC 10646, Annex P.
      *
      * @return a Unicode string from the stream message.
      *
      * @exception MessageNotReadableException if message in write-only mode.
      * @exception MessageEOFException if end of message stream
      * @exception JMSException if JMS fails to read message due to
      *                         some internal JMS error.
      */ 
    virtual StringRef readUTF() /*:>  throws JMSException */;

    /** Read a byte array from the stream message.
      *
      * @param value the buffer into which the data is read.
      *
      * @return the total number of bytes read into the buffer, or -1 if 
      * there is no more data because the end of the stream has been reached.
      *
      * @exception MessageNotReadableException if message in write-only mode.
      * @exception JMSException if JMS fails to read message due to
      *                         some internal JMS error.
      */ 
    virtual jint readBytes(jbyteArrayRef value) /*:>  throws JMSException */;

    /** Read a portion of the bytes message.
      *  
      * @param value the buffer into which the data is read.
      * @param length the number of bytes to read.
      *  
      * @return the total number of bytes read into the buffer, or -1 if
      * there is no more data because the end of the stream has been reached.
      *
      * If length is negative, or length is greater than the length of the array 
      * value, then an IndexOutOfBoundsException is thrown. No bytes will be read
      * from the stream for this exception case.
      *
      * @exception IndexOutOfBoundsException if length is out of bound.
      * @exception MessageNotReadableException if message in write-only mode.
      * @exception JMSException if JMS fails to read message due to
      *                         some internal JMS error.
      */ 
    virtual jint readBytes(jbyteArrayRef value, jint length) /*:>  throws JMSException */;

    /** Write a <code>boolean</code> to the stream message as a 1-byte value.
      * The value <code>true</code> is written out as the value 
      * <code>(byte)1</code> the value <code>false</code> is written out as 
      * the value <code>(byte)0</code>.
      *
      * @param value the <code>boolean</code> value to be written.
      *
      * @exception MessageNotWriteableException if message in read-only mode.
      * @exception JMSException if JMS fails to write message due to
      *                         some internal JMS error.
      */
    virtual void writeBoolean(jboolean value) /*:>  throws JMSException */;

    /** Write out a <code>byte</code> to the stream message as a 1-byte value.
      *
      * @param value the <code>byte</code> value to be written.
      *
      * @exception MessageNotWriteableException if message in read-only mode.
      * @exception JMSException if JMS fails to write message due to
      *                         some internal JMS error.
      */ 
    virtual void writeByte(jbyte value) /*:>  throws JMSException */;

    /** Write a <code>short</code> to the stream message as two bytes, high 
      * byte first.
      *
      * @param value the <code>short</code> to be written.
      *
      * @exception MessageNotWriteableException if message in read-only mode.
      * @exception JMSException if JMS fails to write message due to
      *                         some internal JMS error.
      */ 
    virtual void writeShort(jshort value) /*:>  throws JMSException */;

    /** Write a <code>char</code> to the stream message as a 2-byte value, 
      * high byte first.
      *
      * @param value the <code>char</code> value to be written.
      *
      * @exception MessageNotWriteableException if message in read-only mode.
      * @exception JMSException if JMS fails to write message due to
      *                         some internal JMS error.
      */ 
    virtual void writeChar(jchar value) /*:>  throws JMSException */;

    /** Write an <code>int</code> to the stream message as four bytes, 
      * high byte first.
      *
      * @param value the <code>int</code> to be written.
      *
      * @exception MessageNotWriteableException if message in read-only mode.
      * @exception JMSException if JMS fails to write message due to
      *                         some internal JMS error.
      */ 
    virtual void writeInt(jint value) /*:>  throws JMSException */;

    /** Write a <code>long</code> to the stream message as eight bytes, 
      * high byte first.
      *
      * @param value the <code>long</code> to be written.
      *
      * @exception MessageNotWriteableException if message in read-only mode.
      * @exception JMSException if JMS fails to write message due to
      *                         some internal JMS error.
      */ 
    virtual void writeLong(jlong value) /*:>  throws JMSException */;

    /** Convert the float argument to an <code>int</code> using the
      * <code>floatToIntBits</code> method in class <code>Float</code>,
      * and then writes that <code>int</code> value to the stream
      * message as a 4-byte quantity, high byte first.
      *
      * @param value the <code>float</code> value to be written.
      *
      * @exception MessageNotWriteableException if message in read-only mode.
      * @exception JMSException if JMS fails to write message due to
      *                         some internal JMS error.
      */ 
    virtual void writeFloat(jfloat value) /*:>  throws JMSException */;

    /** Convert the double argument to a <code>long</code> using the
      * <code>doubleToLongBits</code> method in class <code>Double</code>,
      * and then writes that <code>long</code> value to the stream
      * message as an 8-byte quantity, high byte first.
      *
      * @param value the <code>double</code> value to be written.
      *
      * @exception MessageNotWriteableException if message in read-only mode.
      * @exception JMSException if JMS fails to write message due to
      *                         some internal JMS error.
      */ 
    virtual void writeDouble(jdouble value) /*:>  throws JMSException */;

    /** Write a string to the stream message using UTF-8 encoding in a 
      * machine-independent manner.
      *
      * <P>For more information on the UTF-8 format, see "File System Safe 
      * UCS Transformation Format (FSS_UFT)", X/Open Preliminary Specification,       
      * X/Open Company Ltd., Document Number: P316. This information also 
      * appears in ISO/IEC 10646, Annex P. 
      *
      * @param value the <code>String</code> value to be written.
      *
      * @exception MessageNotWriteableException if message in read-only mode.
      * @exception JMSException if JMS fails to write message due to
      *                         some internal JMS error.
      */ 
    virtual void writeUTF(StringRef value) /*:>  throws JMSException */;

    /** Write a byte array to the stream message.
      *
      * @param value the byte array to be written.
      *
      * @exception MessageNotWriteableException if message in read-only mode.
      * @exception JMSException if JMS fails to write message due to
      *                         some internal JMS error.
      */ 
    virtual void writeBytes(jbyteArrayRef value) /*:>  throws JMSException */;

    /** Write a portion of a byte array to the stream message.
      *  
      * @param value the byte array value to be written.
      * @param offset the initial offset within the byte array.
      * @param length the number of bytes to use.
      *
      * @exception MessageNotWriteableException if message in read-only mode.
      * @exception JMSException if JMS fails to write message due to
      *                         some internal JMS error.
      */ 
    virtual void writeBytes(jbyteArrayRef value, jint offset, jint length);
            /*:>  throws JMSException */

    /** Write a Java object to the stream message.
      *
      * <P>Note that this method only works for the objectified primitive
      * object types (Integer, Double, Long ...), String's and byte arrays.
      *
      * @param value the Java object to be written.
      *
      * @exception MessageNotWriteableException if message in read-only mode.
      * @exception javax::jms::MessageFormatException if object is invalid type.
      * @exception JMSException if JMS fails to write message due to
      *                         some internal JMS error.
      */ 
    virtual void writeObject(ObjectRef value) /*:>  throws JMSException */;

    /** Put the message in read-only mode, and reposition the stream of 
      * bytes to the beginning.
      *  
      * @exception JMSException if JMS fails to reset the message due to
      *                         some internal JMS error.
      * @exception javax::jms::MessageFormatException if message has an invalid
      *                         format
      */ 
    virtual void reset() /*:>  throws JMSException */;

    private: virtual void reservedv1();

};

}}} //:>  namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_BYTESMESSAGE_H_
