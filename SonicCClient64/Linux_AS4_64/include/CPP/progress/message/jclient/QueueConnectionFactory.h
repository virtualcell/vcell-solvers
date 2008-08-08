/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#ifndef _PROGRESS_MESSAGE_JCLIENT_QUEUECONNECTIONFACTORY_H_
#define _PROGRESS_MESSAGE_JCLIENT_QUEUECONNECTIONFACTORY_H_

#include <progress/message/jclient/package.h>


namespace progress { namespace message { namespace jclient {

  /**
   * Construct a QueueConnectionFactory (with a default connectID, user and password of ""). 
   *
   * @param   brokerURL - the URL (in the form [protocol://]hostname[:port]) 
   *			of the message broker to which connection are made.
   *
   * @return  QueueConnectionFactoryRef
   *
   * @exception JMSException if a connection factory can not be created.
   */
  SMQ_API QueueConnectionFactoryRef createQueueConnectionFactory(StringRef brokerURL);

  /**
   * Construct a QueueConnectionFactory (with a default connectID of null). 
   *
   * @param   brokerHostName - the broker host name
   * @param   brokerPort - the broker port number
   * @param   brokerProtocol - the broker protocol, can be null or ""
   * @param   userName - the default user name
   * @param   password - the default password
   *
   * @return  QueueConnectionFactoryRef
   *
   * @exception JMSException if a connection factory can not be created.
   */
  SMQ_API QueueConnectionFactoryRef createQueueConnectionFactory(StringRef brokerHostName, 
                                                                 jint brokerPort,
                                                                 StringRef brokerProtocol, 
                                                                 StringRef defaultUserName,
                                                                 StringRef defaultPassword);

  /**
   * Construct a QueueConnectionFactory
   *
   * @param   brokerHostName - the broker host name
   * @param   brokerPort - the broker port number
   * @param   brokerProtocol - the broker protocol, can be null or ""
   * @param   connectID - the id string used to identify connection. This may be null.
   * @param   userName - the default user name
   * @param   password - the default password
   *
   * @return  QueueConnectionFactoryRef
   *
   * @exception JMSException if a connection factory can not be created.
   */
  SMQ_API QueueConnectionFactoryRef createQueueConnectionFactory(StringRef brokerHostName, 
                                                                 jint brokerPort,
                                                                 StringRef brokerProtocol, 
                                                                 StringRef connectID, 
                                                                 StringRef defaultUserName,
                                                                 StringRef defaultPassword);

  /**
   * Construct a QueueConnectionFactory (with a default user and password of ""). 
   *
   * @param   brokerURL - the URL (in the form [protocol://]hostname[:port]) 
   *			of the message broker to which connection are made.
   * @param   connectID - the id string used to identify connection. This may be null.
   *
   * @return  QueueConnectionFactoryRef
   *
   * @exception JMSException if a connection factory can not be created.
   */
  SMQ_API QueueConnectionFactoryRef createQueueConnectionFactory(StringRef brokerURL, 
                                                                 StringRef connectID);

  /**
   * Construct a QueueConnectionFactory (with a default connectID of null).
   *
   * @param   brokerURL - the URL (in the form [protocol://]hostname[:port]) 
   *			of the message broker to which connection are made.
   * @param   userName - the default user name
   * @param   password - the default password
   *
   * @return  QueueConnectionFactoryRef
   *
   * @exception JMSException if a connection factory can not be created.
   */
  SMQ_API QueueConnectionFactoryRef createQueueConnectionFactory(StringRef brokerURL, 
                                                                 StringRef defaultUserName,
                                                                 StringRef defaultPassword);

  /**
   * Construct a QueueConnectionFactory.
   *
   * @param   brokerURL - the URL (in the form [protocol://]hostname[:port]) 
   *			of the message broker to which connection are made.
   * @param   connectID - the id string used to identify connection. This may be null.
   * @param   userName - the default user name
   * @param   password - the default password
   *
   * @return  QueueConnectionFactoryRef
   *
   * @exception JMSException if a connection factory can not be created.
   */
  SMQ_API QueueConnectionFactoryRef createQueueConnectionFactory(StringRef brokerURL, 
                                                                 StringRef connectID,
                                                                 StringRef defaultUserName, 
                                                                 StringRef defaultPassword);

  /** 
   * An implementation of a JMS QueueConnectionFactory.
   * A client uses a QueueConnectionFactory to create QueueConnections with
   * a JMS PTP provider.
   */
  class SMQ_API QueueConnectionFactory : public ConnectionFactory
  {
  public:

    virtual ~QueueConnectionFactory();

	/**
	 * Returns the int corresponding to the QueueConnectionFactory type.
	 *
	 * @return the int corresponding to the QueueConnectionFactory type
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
	 * Create a queue connection with default user identity.
	 *
	 * @return  a newly created queue connection.
	 *
	 * @exception JMSException if JMS Provider fails to create a Queue Connection 
	 *			due to some internal error.
	 * @exception JMSSecurityException - if client authentication fails due to 
	 *			invalid user name or password.
	 */
    QueueConnectionRef createQueueConnection();

	/**
	 * Create a queue connection with specified user identity.
	 *
	 * @param   userName - the caller's user name
	 * @param   password - the caller's password
	 *
	 * @return  a newly created queue connection.
	 *
	 * @exception JMSException if JMS Provider fails to create a Queue Connection 
	 *			due to some internal error.
	 * @exception JMSSecurityException - if client authentication fails due to 
	 *			invalid user name or password.
	 */
    QueueConnectionRef createQueueConnection(StringRef username, StringRef password);


    /**
     * Set the prefetch count for the QueueReceiver.  When this value
     * is greater than one, the broker will be able to send multiple
     * messages as part of a single QueueReceiver request. This can
     * improve performance.
     *
     * <p>Note that this is a Progress SonicMQ extention not found in the
     * standard {@link javax.jms.QueueReceiver} interface.
     *
     * @param count The number of messages to prefetch in int.
     * @exception javax.jms.JMSException if an invalid value is set.
     * @see #getPrefetchCount()
     */
    void setPrefetchCount(jint count);

    /**
     * Get the prefetch count for the QueueReceiver. When this value
     * is greater than one, the broker will be able to send multiple
     * messages as part of a single QueueReceiver request.
     * @return The number of messages to prefetch as int.
     * @see #setPrefetchCount(int)
     */
    jint getPrefetchCount();

    /**
     * Set the default prefetch threshold for the QueueReceivers.  When the
     * number of messages waiting to be processed by a QueueReceiver
     * falls to, or below, this number, a new batch of messages will be fetched.
     *
     * <p>Note that this is a Progress SonicMQ extention not found in the
     * standard {@link javax.jms.QueueReceiver} interface.
     *
     * @param val The default threshold value for QueueReceivers.
     * @exception javax.jms.JMSException if an invalid value is set.
     * @see #getPrefetchThreshold()
     */
    void setPrefetchThreshold(jint val);
      
    /**
     * Get the default prefetch threshold for the QueueReceiver.  When the
     * number of messages waiting to be processed by the QueueReceiver
     * falls to, or below, this number, a new batch of messages will be fetched.
     * @return The threshold value for prefetching messages as int.
     * @see #setPrefetchThreshold(int)
     */
    jint getPrefetchThreshold();

    private: void PMJQCFreserved0();

};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_QUEUECONNECTIONFACTORY_H_
