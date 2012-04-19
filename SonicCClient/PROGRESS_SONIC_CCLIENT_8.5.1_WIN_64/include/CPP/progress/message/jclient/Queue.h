#ifndef _PROGRESS_MESSAGE_JCLIENT_QUEUE_H_
#define _PROGRESS_MESSAGE_JCLIENT_QUEUE_H_
/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#include <progress/message/jclient/Destination.h>
using java::lang::StringRef;

namespace progress { namespace message { namespace jclient {


SMQ_API QueueRef createQueue(StringRef queueName);

class SMQ_API Queue : public Destination
{
public:
    virtual ~Queue();

    /**
     * Returns the int corresponding to the Queue type.
     *
     * @return the int corresponding to the Queue type
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

    /** Return a pretty printed version of the queue name.
      *
      * @return the provider specific identity values for this queue.
      */
    virtual StringRef toString();

    /** Get the name of this queue.
      *  
      * <P>Clients that depend upon the name, are not portable.
      *  
      * @return the queue name
      *  
      * @exception JMSException if JMS implementation for Queue fails to
      *                         to return queue name due to some internal
      *                         error.
      */ 
    virtual StringRef getQueueName() /* throws JMSException */;

    private: virtual void reservedv1();
    private: void PMJQreserved0();
    private: void PMJQreserved1();
    private: void PMJQreserved2();
};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_QUEUE_H_
