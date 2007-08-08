#ifndef _PROGRESS_MESSAGE_JCLIENT_TOPICCONNECTION_H_
#define _PROGRESS_MESSAGE_JCLIENT_TOPICCONNECTION_H_
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

SMQ_API TopicConnectionRef createTopicConnection(
	const StringRef brokerURL,
	const StringRef connectID,
	const StringRef username,
	const StringRef password);

class SMQ_API TopicConnection : public progress::message::jclient::Connection
{
public:
	virtual ~TopicConnection();

	/**
	 * Returns the int corresponding to the TopicConnection type.
	 *
	 * @return the int corresponding to the TopicConnection type
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
	  * @exception javax::jms::InvalidClientIDException if JMS client specifies an
	  *                         invalid or duplicate client id.
	  * @exception javax::jms::IllegalStateException if JMS client tries to set the ClientID 
	  *                         after it creates sessions.
	  */
	virtual void setClientID(StringRef clientID);

	/** Create a TopicSession
	  *
	  * @param transacted if true, the session is transacted.
	  * @param acknowledgeMode indicates whether the consumer or the
	  * 	client will acknowledge any messages it receives. This parameter
	  * 	will be ignored if the session is transacted.
	  *
	  * @return a newly created topic session.
	  *
	  * @exception JMSException if JMS Connection fails to create a
	  *                         session due to some internal error or
	  *                         lack of support for specific transaction
	  *                         and acknowledgement mode.
	  */
    virtual TopicSessionRef createTopicSession(jboolean transacted, jint ackMode);

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

    private: void PMJTCONNreserved0();
    private: void PMJTCONNreserved1();
    private: void PMJTCONNreserved2();
    private: void PMJTCONNreserved3();
    private: void PMJTCONNreserved4();
    private: void PMJTCONNreserved5();
    private: void PMJTCONNreserved6();
};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_TOPICCONNECTION_H_
