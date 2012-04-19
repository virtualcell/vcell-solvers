#ifndef _PROGRESS_MESSAGE_JCLIENT_TEMPORARYQUEUE_H_
#define _PROGRESS_MESSAGE_JCLIENT_TEMPORARYQUEUE_H_
/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#include <progress/message/jclient/Queue.h>


namespace progress { namespace message { namespace jclient {

/** A TemporaryQueue is a unique Queue object created for the duration of 
  * a QueueConnection. It is a system defined Queue that can only be 
  * consumed by the QueueConnection that created it.
  */
class TemporaryQueue : public Queue
{
public:
    virtual ~TemporaryQueue();

    /**
     * Returns the int corresponding to the TemporaryQueue type.
     *
     * @return the int corresponding to the TemporaryQueue type
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

    /** Delete this temporary queue. If there are senders
     * or receivers still using it, then a JMSException will be thrown.
     *  
     * @exception JMSException if JMS implementation fails to delete a 
     *                         Temporary queue due to some internal error.
     */
    virtual void _delete();

    private: virtual void reservedv1();
    private: void PMJTQreserved0();
    private: void PMJTQreserved1();

};

}}} // namespace

#endif // _PROGRESS_MESSAGE_JCLIENT_TEMPORARYQUEUE_H_
