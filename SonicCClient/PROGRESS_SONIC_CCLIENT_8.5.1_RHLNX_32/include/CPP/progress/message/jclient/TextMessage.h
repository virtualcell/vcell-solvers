#ifndef _PROGRESS_MESSAGE_JCLIENT_TEXTMESSAGE_H_
#define _PROGRESS_MESSAGE_JCLIENT_TEXTMESSAGE_H_
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
#define CLONE_TEXTMESSAGE_RET_TYPE Object
#else
#define CLONE_TEXTMESSAGE_RET_TYPE TextMessage
#endif


namespace progress { namespace message { namespace jclient {

SMQ_API TextMessageRef createTextMessage();

class SMQ_API TextMessage : public Message
{

public:
    virtual ~TextMessage();

    /**
     * Returns the int corresponding to the TextMessage type.
     *
     * @return the int corresponding to the TextMessage type
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

    /** Clone this Message
      *
      * @return clone of Message
      */
    virtual CLONE_TEXTMESSAGE_RET_TYPE * clone();

    /** Set the string containing this message's data.
      *
      * @param string the String containing the message's data
      *
      * @exception JMSException if JMS fails to set text due to
      *                         some internal JMS error.
      * @exception MessageNotWriteableException if message in read-only mode.
      */
    virtual void setText(StringRef string);

    /** Get the string containing this message's data.  The default
      * value is null.
      *
      * @return the String containing the message's data
      *
      * @exception JMSException if JMS fails to get text due to
      *                         some internal JMS error.
      */
    virtual StringRef getText();

    private: virtual void reservedv1();
};

}}} // namespace

#endif // _PROGRESS_MESSAGE_JCLIENT_TEXTMESSAGE_H_
