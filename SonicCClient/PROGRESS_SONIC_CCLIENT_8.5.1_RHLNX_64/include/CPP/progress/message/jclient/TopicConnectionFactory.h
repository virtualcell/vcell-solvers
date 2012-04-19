#ifndef _PROGRESS_MESSAGE_JCLIENT_TOPICCONNECTIONFACTORY_H_
#define _PROGRESS_MESSAGE_JCLIENT_TOPICCONNECTIONFACTORY_H_
/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#include <progress/message/jclient/ConnectionFactory.h>


namespace progress { namespace message { namespace jclient {

	/**
	 * Create a new TopicConnectionFactory.
	 *
	 * @param brokerURL a String containing the broker URL.
	 *
	 * @return the newly created TopicConnectionFactory
	 *
	 * @exception JMSException if JMS Provider fails to create a TopicConnectionFactory
	 *                         due to some internal error.
	 */
	SMQ_API TopicConnectionFactoryRef createTopicConnectionFactory(StringRef brokerURL);

	/**
	 * Create a new TopicConnectionFactory.
	 *
	 * @param brokerHostName a String containing the broker hostname.
	 * @param brokerPort the broker port number.
	 * @param brokerProtocol a String containing the broker protocol.
	 * @param defaultUserName a String containing the default username.
	 * @param defaultPassword a String containing the default password.
	 *
	 * @return the newly created TopicConnectionFactory
	 *
	 * @exception JMSException if JMS Provider fails to create a TopicConnectionFactory
	 *                         due to some internal error.
	 */
	SMQ_API TopicConnectionFactoryRef createTopicConnectionFactory(StringRef brokerHostName, 
								 jint brokerPort,
								 StringRef brokerProtocol, 
								 StringRef defaultUserName,
								 StringRef defaultPassword);

	/**
	 * Create a new TopicConnectionFactory.
	 *
	 * @param brokerHostName a String containing the broker hostname.
	 * @param brokerPort the broker port number.
	 * @param brokerProtocol a String containing the broker protocol.
	 * @param connectID a String containing the connection ID.
	 * @param defaultUserName a String containing the default username.
	 * @param defaultPassword a String containing the default password.
	 *
	 * @return the newly created TopicConnectionFactory
	 *
	 * @exception JMSException if JMS Provider fails to create a TopicConnectionFactory
	 *                         due to some internal error.
	 */
	SMQ_API TopicConnectionFactoryRef createTopicConnectionFactory(StringRef brokerHostName, 
								 jint brokerPort,
								 StringRef brokerProtocol, 
								 StringRef connectID, 
								 StringRef defaultUserName,
								 StringRef defaultPassword);

	/**
	 * Create a new TopicConnectionFactory.
	 *
	 * @param brokerURL a String containing the broker URL.
	 * @param connectID a String containing the connection ID.
	 *
	 * @return the newly created TopicConnectionFactory
	 *
	 * @exception JMSException if JMS Provider fails to create a TopicConnectionFactory
	 *                         due to some internal error.
	 */
	SMQ_API TopicConnectionFactoryRef createTopicConnectionFactory(StringRef brokerURL, StringRef connectID);

	/**
	 * Create a new TopicConnectionFactory.
	 *
	 * @param brokerURL a String containing the broker URL.
	 * @param defaultUserName a String containing the default username.
	 * @param defaultPassword a String containing the default password.
	 *
	 * @return the newly created TopicConnectionFactory
	 *
	 * @exception JMSException if JMS Provider fails to create a TopicConnectionFactory
	 *                         due to some internal error.
	 */
	SMQ_API TopicConnectionFactoryRef createTopicConnectionFactory(StringRef brokerURL, 
								 StringRef defaultUserName,
								 StringRef defaultPassword);

	/**
	 * Create a new TopicConnectionFactory.
	 *
	 * @param brokerURL a String containing the broker URL.
	 * @param connectID a String containing the connection ID.
	 * @param defaultUserName a String containing the default username.
	 * @param defaultPassword a String containing the default password.
	 *
	 * @return the newly created TopicConnectionFactory
	 *
	 * @exception JMSException if JMS Provider fails to create a TopicConnectionFactory
	 *                         due to some internal error.
	 */
	SMQ_API TopicConnectionFactoryRef createTopicConnectionFactory(StringRef brokerURL, 
								 StringRef connectID,
								 StringRef defaultUserName, 
								 StringRef defaultPassword);

/** An implementation of a JMS TopicConnectionFactory.
  * A client uses a TopicConnectionFactory to create TopicConnections with
  * a JMS Pub/Sub provider.
  *
  */
class SMQ_API TopicConnectionFactory : public ConnectionFactory
{
  public:

    virtual ~TopicConnectionFactory();

	/**
	 * Returns the int corresponding to the TopicConnectionFactory type.
	 *
	 * @return the int corresponding to the TopicConnectionFactory type
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
	 * Create a topic connection with default user identity.
	 *
	 * @return a newly created TopicConnection.
	 *
	 * @exception JMSException if JMS Provider fails to create a Topic Connection
	 *                         due to some internal error.
	 * @exception JMSSecurityException  if client authentication fails due to
	 *                         invalid user name or password.
	 */
    TopicConnectionRef createTopicConnection();
	/**
	 * Create a topic connection with default user identity.
	 *
	 * @param username the caller's user name
	 * @param password the caller's password
	 *
	 * @return a newly created TopicConnection.
	 *
	 * @exception JMSException if JMS Provider fails to create a Topic Connection
	 *                         due to some internal error.
	 * @exception JMSSecurityException  if client authentication fails due to
	 *                         invalid user name or password.
	 */
    TopicConnectionRef createTopicConnection(StringRef username, StringRef password);
};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_TOPICCONNECTIONFACTORY_H_
