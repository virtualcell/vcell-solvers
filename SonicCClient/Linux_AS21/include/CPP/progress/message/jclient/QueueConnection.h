#ifndef _PROGRESS_MESSAGE_JCLIENT_QUEUECONNECTION_H_
#define _PROGRESS_MESSAGE_JCLIENT_QUEUECONNECTION_H_
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

#include <progress/message/jclient/Connection.h>


namespace progress { namespace message { namespace jclient {

SMQ_API QueueConnectionRef createQueueConnection(
	const StringRef brokerURL, 
	const StringRef connectID, 
	const StringRef username, 
	const StringRef password);

/**
 * A QueueConnection is an active connection to a JMS PTP provider. A client 
 * uses a QueueConnection to create one or more QueueSessions for producing
 * and consuming messages. 
 *
 * @version 1.0 6-Jul-1999
 * @author Giovanni Boschi
 */
class SMQ_API QueueConnection : public Connection //implements javax.jms.QueueConnection
{

public:	
  	virtual ~QueueConnection();

	/**
	 * Returns the int corresponding to the QueueConnection type.
	 *
	 * @return the int corresponding to the QueueConnection type
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

	/** Set the client identifier for this connection.
      *
      * Overrides setClientID of Connection class.
      *
      * @param clientID the unique client identifier
      *
      * @exception JMSException general exception if JMS implementation fails to
      *                         set the client ID for this Connection due
      *                         to some internal error.
      *
      * @exception javax.jms.InvalidClientIDException if JMS client specifies an
      *                         invalid or duplicate client id.
      * @exception javax.jms.IllegalStateException if JMS client tries to set the ClientID 
      *                         after it creates sessions.
      */
    virtual void setClientID(StringRef clientID);

    /** Create a QueueSession
      *  
      * @param transacted if true, the session is transacted.
      * @param acknowledgeMode indicates whether the consumer or the
      * client will acknowledge any messages it receives. This parameter
      * will be ignored if the session is transacted.
      *  
      * @return a newly created queue session.
      *  
      * @exception JMSException if JMS Connection fails to create a
      *                         session due to some internal error or
      *                         lack of support for specific transaction
      *                         and acknowledgement mode.
      */ 
	virtual QueueSessionRef createQueueSession(jboolean transacted,jint acknowledgeMode);

    /** Start (or restart) a Connection's delivery of incoming messages.
      * Restart begins with the oldest unacknowledged message.
      * Starting a started session is ignored.
      *  
      * @exception JMSException if JMS implementation fails to start the
      *                         message delivery due to some internal error.
      */
	virtual void start();

    /** Used to temporarily stop a Connection's delivery of incoming messages.
      * It can be restarted using its <CODE>start</CODE> method. When stopped,
      * delivery to all the Connection's message consumers is inhibited: 
      * synchronous receive's block and messages are not delivered to message 
      * listeners.
      *
      * <P>After stop is called there may still be some messages delivered.
      *
      * <P>Stopping a Session has no affect on its ability to send messages. 
      * Stopping a stopped session is ignored.
      *  
      * @exception JMSException if JMS implementation fails to stop the
      *                         message delivery due to some internal error.
      */
	virtual void stop();
    
    /** Since C-Client typically allocates significant resources on behalf
      * of a Connection, clients must close them when they are not needed.
      * This is true even when a connection is lost due to socket loss.  The
      * close method initiates cleanup of the entire object heirarchy.
      * Significant memory leaks will result if connections are not closed.
      *
      * @exception JMSException if JMS implementation fails to close the
      *                         connection due to internal error. For 
      *                         example, a failure to release resources
      *                         or to close socket connection can lead
      *                         to throwing of this exception.
      */
	virtual void close();

    private: void PMJQCONNreserved0();
    private: void PMJQCONNreserved1();
    private: void PMJQCONNreserved2();
    private: void PMJQCONNreserved3();
    private: void PMJQCONNreserved4();
    private: void PMJQCONNreserved5();
    private: void PMJQCONNreserved6();
    private: void PMJQCONNreserved7();
    private: void PMJQCONNreserved8();
    private: void PMJQCONNreserved9();
    private: void PMJQCONNreserved10();
    private: void PMJQCONNreserved11();
    private: void PMJQCONNreserved12();
    private: void PMJQCONNreserved13();
    private: void PMJQCONNreserved14();
};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_QUEUECONNECTION_H_
