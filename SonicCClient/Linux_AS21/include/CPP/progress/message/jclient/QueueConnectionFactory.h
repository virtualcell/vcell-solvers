#ifndef _PROGRESS_MESSAGE_JCLIENT_QUEUECONNECTIONFACTORY_H_
#define _PROGRESS_MESSAGE_JCLIENT_QUEUECONNECTIONFACTORY_H_
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

#include <java/lang/package.h>
#include <progress/message/jclient/package_decls.h>
#include <progress/message/jclient/ILoginSPI.h>


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
  SMQ_API QueueConnectionFactoryRef createQueueConnectionFactory(StringRef brokerURL, StringRef connectID);

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

  /** An implementation of a JMS QueueConnectionFactory.
    * A client uses a QueueConnectionFactory to create QueueConnections with
    * a JMS PTP provider.
    */
  class SMQ_API QueueConnectionFactory : public java::lang::Object
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
	 * Retrieve the broker hostname.
	 *
	 * @return String value of broker hostname.
	 */
    StringRef getBrokerHostName();
	/**
	 * Retrieve the broker port number.
	 *
	 * @return jint value of broker port number.
	 */
    jint getBrokerPort();
	/**
	 * Retrieve the broker protocol.
	 *
	 * @return String containing the broker protocol.
	 */
    StringRef getBrokerProtocol();
	/**
	 * Retrieve the broker URL.
	 *
	 * @return String containing the broker URL.
	 */
    StringRef getBrokerURL();
	/**
	 * Retrieve the client ID.
	 *
	 * @return String containing the client ID.
	 */
    StringRef getClientID();
	/**
	 * Retrieve the connect ID.
	 *
	 * @return String containing the connect ID.
	 */
    StringRef getConnectID();
	/**
	 * Get list of brokers to try to connect to.
	 *
	 * @return String containing comma-separated list of broker URLs.
	 */
    StringRef getConnectionURLs();
	/**
	 * Retrieve the default password.
	 *
	 * @return String containing the default password.
	 */
    StringRef getDefaultPassword();
	/**
	 * Retrieve the default username.
	 *
	 * @return String containing the default username.
	 */
    StringRef getDefaultUser();
	/**
	 * Determines whether client-side load balancing is enabled.
	 *
	 * @return jboolean - if true, indicates that the client is willing to have a 
	 *        connect request redirected to another broker within a SonicMQ cluster.
	 */
    jboolean getLoadBalancing();

    /** 
     * Gets the administrative flow control monitoring interval.
     *
     * @return The interval in seconds over which administrative monitoring of flow control will occur.
     *         A value of 0 (zero) indicates that no monitoring will occur.
     *
     * @see    setMonitorInterval(Integer)
     */
    jint getMonitorInterval();

	/**
	 * Determines whether the option to start with the first or a random element 
	 * of the broker list has been selected.
	 *
	 * @return jboolean - if true, connect attempts start with the first broker in the list. 
	 *        If false, connect attempts start with a random element in the broker list.
	 */
    jboolean getSequential();
	/**
	 * Determines whether the option to disable Nagle's algorithm for TCP connections
	 * has been selected.
	 *
	 * @return jboolean - if true, created TCP connections will have Nagle's algorithm
	 *        disabled.
	 */
    jboolean getTcpNoDelay();
	/**
	 * Sets the broker hostname.
	 *
	 * @param brokerHostName a String w/ the new broker hostname to set.
	 */
    void setBrokerHostName(StringRef brokerHostName);
	/**
	 * Sets the broker port number.
	 *
	 * @param brokerPort the new broker port number to set.
	 */
    void setBrokerPort(jint brokerPort);
	/**
	 * Sets the broker protocol.
	 *
	 * @param brokerProtocol a String w/ the new broker protocol to set.
	 */
    void setBrokerProtocol(StringRef brokerProtocol);
	/**
	 * Sets the broker URL.
	 *
	 * @param brokerURL a String w/ the new broker URL to set.
	 */
    void setBrokerURL(StringRef brokerURL);
	/**
	 * Sets the client ID.
	 *
	 * @param clientID a String w/ the new client ID to set.
	 */
    void setClientID(StringRef clientID);
	/**
	 * Sets the connect ID.
	 *
	 * @param connectID a String w/ the new connect ID to set.
	 */
    void setConnectID(StringRef connectID);
	/**
	 * Configures a list of brokers to try when creating a connection.
	 *
	 * @param brokerList -	a String containing a comma-separated list of broker URLs.
	 *						Allows a client to connect to the first available broker on a list.
	 *						If present, this parameter overrides the brokerURL parameter in the
	 *						QueueConnectionFactory constructor, which specifies a single broker.
	 *						This option can be used independently of any other load balancing options.
	 */
    void setConnectionURLs(StringRef brokerList);
	/**
	 * Enable or disable client-side load balancing.
	 *
	 * @param loadBalancing -	if true, indicates that the client is willing to have
	 *							a connect request re-directed to another broker within
	 *							a SonicMQ cluster.
	 */
    void setLoadBalancing(jboolean loadBalancing);

    /** Set the administrative flow control monitoring interval.
    * <p>This method which takes a Integer argument is provided for interoperability
    * with object storage systems which require that the attributes of
    * the stored object be of Object datatype.
    *
    * @param interval The interval in seconds over which administrative monitoring of flow control will occur.
    *                 A value of 0 (zero) indicates that no monitoring will occur.
    * @see    getMonitorInterval()
    */
    void setMonitorInterval(jint interval);

	/**
	 * Specifies whether to start with the first broker in the list or a random element.
	 *
	 * @param sequential -	If true, starts attempting to connect to the first broker
	 *						in the list; if false, starts attempting to connect to a 
	 *						random element in the list.
	 *						After that, tries to connect to each broker in sequence 
	 *						until a successful connect occurs, or the list is exhausted.
	 */
    void setSequential(jboolean sequential);
	/**
	 * Enables or disables Nagle's algorithm for TCP connections.
	 *
	 * @param noDelay -	If true, created connections have Nagle's algorithm disabled.
	 */
    void setTcpNoDelay(jboolean noDelay);

    /** C++ interface to register the LoginSPI with the queue connection factory.
     *  When used with 4.0 brokers only clear text passwords are supported.  Users should
     *  use the setPassword method to set the clear text password.
     *  When used with 5.0 brokers only both clear text and encrypted (byte array) 
     *  passwords are supported.  Users should use the setPassword method to set the clear 
     *  text password and/or use setTransformedPassword to set the encrypted byte array
     *  password.
     *
     * @param loginSPI - an instance of the user defined login class, inherited from the
     *  ILoginSPI class.
     */
    void setLoginSPI(ILoginSPI *loginSPI);

    /** Sets the default batch size of all sessions, created via connections
      * created from this ConnectionFactory. A Session's batch size specifies
      * the number of bytes to be batched on the client before flushing the 
      * batched messages.  This is a performance optimization utilized on transacted 
      * sessions.  The size parameter is a measure of the message payloads only.
      *
      * @param size  The number of bytes to batch, specified as a jint.
      *
      */
    void setDefaultTxnBatchSize(jint size);

    /** Gets the default batch size
      *
      * @return The number of bytes to batch, specified as an int.
      */
    jint getDefaultTxnBatchSize();

    /** Sets the TCP connection timeout, in milliseconds.  This is the number of
      * milliseconds that a C-Client will wait for a connection to be established.
      * Default is zero (i.e. allow the TCP connect call to time out on its own).
      *
      * @param size  The number of milliseconds to wait for a connection.
      *
      */
    void setTCPConnectionTimeout(jint connectTimeout);

    /** Gets the TCP connection timeout
      *
      * @return The number of milliseconds to wait for a connection.
      */
    jint getTCPConnectionTimeout();

    /** Set the default Password for connections created from this factory.
     *  @param password The password as a String.
     */
    void setDefaultPassword(StringRef defaultPassword);
    
    /** Set the default Username for connections created from this factory.
     *  @param username The Username as a String.
     */
    void setDefaultUser(StringRef defaultUserName);
             /** Enable or disable fault tolerant connection creation.
     *<P>
     * By default, connections are created non-fault tolerant(false). For a connection to
     * be created fault-tolerant, fault-tolerant must be set in the ConnectionFactory,
     * and, the broker must support(be licensed for) fault-tolerance.
     *
     * @param faultTolerant    true indicates the client wishes to create fault-tolerant
     * connections
     * @see    getFaultTolerant()
     * @see    progress.message.jclient.Connection#isFaultTolerant()
     * @see    setFaultTolerantReconnectTimeout(Integer)
     * @see    getFaultTolerantReconnectTimeout()
     * @see    setInitialConnectTimeout(Integer)
     * @see    getInitialConnectTimeout()
     * @see    setClientTransactionBufferSize(Long)
     * @see    getClientTransactionBufferSize()
     */
    void setFaultTolerant(jboolean faultTolerant);

    /** Determines whether fault tolerant connection creation is enabled.
     *<P>
     * By default, connections are created non-fault tolerant(false). For a connection to
     * be created fault-tolerant, fault-tolerant must be set in the ConnectionFactory,
     * and, the broker must support(be licensed for) fault-tolerance.
     *
     * @return       true indicates the client wishes to create fault-tolerant
     * connections
     *
     * @see    setFaultTolerant(Boolean)
     * @see    progress.message.jclient.Connection#isFaultTolerant()
     * @see    setFaultTolerantReconnectTimeout(Integer)
     * @see    getFaultTolerantReconnectTimeout()
     * @see    setInitialConnectTimeout(Integer)
     * @see    getInitialConnectTimeout()
     * @see    setClientTransactionBufferSize(Long)
     * @see    getClientTransactionBufferSize()
     */
    jboolean getFaultTolerant();

    /** Set fault tolerance reconnect timeout.
     * <P>
     * Indicates how long the client runtime will try to establish a physical network connection for the purpose of
     * connection re-establishment after failure. Applicable to fault-tolerant connections only.
     * <P>
     * For reconnects, attempts are made to the previously connected broker URL and to redundant broker URLs
     * reported by the previously connected broker. Redundant broker URLs may be on the last connected
     * broker(if redundant network interfaces are available), or, on a standby broker. URLs are redundantly
     * associated if they have the same Directory Service acceptor name. The broker dynamically supplies the
     * client runtime with redundant URL information(at connect time and on configuration change). Redundant
     * URL information can be obtained by the progress.message.jclient.Connection methods getBrokerReconnectURLs()
     * and getBrokerStandbyReconnectURLs().
     * <P>
     * Default 60 seconds. 0 means no timeout; the runtime will try indefinitely, or, until the broker reports that
     * it can no longer support the fault-tolerant client. A broker can limit the amount of time it maintains
     * state on behalf of a failed fault-tolerant client that has not reconnected by configuration of the
     * advanced broker connections parameter "Client Reconnect Timeout".
     * <P>
     * For reconnects after connection failure, if the connection cannot be reconnected within the allocated time,
     * an exception will be returned to the ExceptionListener(if configured), and all pending connection operations
     * will fail. This is the usual failed connection behavior.
     * <P>
     * @param seconds    maximum time in seconds to attempt reconnection of a failed
     *                   fault-tolerant connection
     * @see    getFaultTolerantReconnectTimeout()
     * @see    setFaultTolerant(Boolean)
     * @see    getFaultTolerant()
     * @see    progress.message.jclient.Connection#getBrokerReconnectURLs()
     * @see    progress.message.jclient.Connection#getStandbyBrokerReconnectURLs()
     * @see    setInitialConnectTimeout(Integer)
     * @see    getInitialConnectTimeout()
     */
    void setFaultTolerantReconnectTimeout(jint seconds);

    /** Get fault tolerance reconnect timeout.
     *
     * @return maximum time in seconds to attempt reconnection of a failed
     *         fault-tolerant connection
     * @see    setFaultTolerantReconnectTimeout(Integer)
     * @see    setFaultTolerant(Boolean)
     * @see    getFaultTolerant()
     * @see    setInitialConnectTimeout(Integer)
     * @see    getInitialConnectTimeout()
     */
    jint getFaultTolerantReconnectTimeout();

    /** Set initial connect timeout for a fault-tolerant connection.
     * <P>
     * Indicates how long the client runtime will try to establish a physical network connection for the purpose of
     * initial connection establishment. Applicable to fault-tolerant connections only.
     * <P>
     * For initial connections, attempts are made against all URLs listed in the ConnectionFactory. URLs
     * are attempted sequentially. The starting URL is normally the first in the list but can be randomly
     * selected.
     * <P>
     * Default 30.
     * 0 means no timeout; the runtime will try indefinitely.
     * -1 means each URL is attempted one time only; the runtime will try each URL sequentially one at a time
     * until a successful connection is made, or until all URLs have been tried.
     * <P>
     * For initial connection establishment, if connection cannot be made within the allocated time the
     * ConnectionFactory create method throws a JMSException.
     *
     * @param seconds    maximum time in seconds to attempt initial connection of a
     *                   fault-tolerant connection
     * @see    getInitialConnectTimeout()
     * @see    setFaultTolerant(Boolean)
     * @see    getFaultTolerant()
     * @see    setFaultTolerantReconnectTimeout(Integer)
     * @see    getFaultTolerantReconnectTimeout()
     */
    void setInitialConnectTimeout(jint seconds);
  
    /** Get initial connect timeout for a fault-tolerant connection.
     *
     * @return maximum time in seconds to attempt initial connection of a fault-tolerant connection
     *         fault-tolerant connection
     * @see    setInitialConnectTimeout(Integer)
     * @see    setFaultTolerant(Boolean)
     * @see    getFaultTolerant()
     * @see    setFaultTolerantReconnectTimeout(Integer)
     * @see    getFaultTolerantReconnectTimeout()
     */
    jint getInitialConnectTimeout();
                               
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
  

    /** Set client transaction buffer size.
     * Indicates the maximum size of messages in bytes the client runtime is willing to buffer
     * per transaction to support transactions over fault tolerant connections.
     * <P>
     * Transacted message remain in the client runtime until saved or replicated by the broker.
     * JMS client threads sending transacted messages will block if the buffer size is reached,
     * and resume when the broker saves or replicates more messages.
     * A larger buffer size should yield better performance at the expense of more client memory
     * and longer resend time during fault-tolerant reconnect.
     * <P>
     * The default setting (0) indicates that the client runtime must be able to buffer up
     * to the broker Transactions Buffer Size parameter per transaction
     * <P>
     * @param size client transaction buffer size in bytes.
     * @see    getClientTransactionBufferSize()
     * @see    setFaultTolerant(Boolean)
     * @see    getFaultTolerant()
     */
    void setClientTransactionBufferSize(jlong size);

    /** Get client transaction buffer size.
     *
     * @return client transaction buffer size.
     * @see    setClientTransactionBufferSize(Long)
     * @see    setFaultTolerant(Boolean)
     * @see    getFaultTolerant()
     */
    jlong getClientTransactionBufferSize();
 
    private: void PMJQCFreserved0();
    private: void PMJQCFreserved1();
    private: void PMJQCFreserved2();
    private: void PMJQCFreserved3();
    
    private: void PMJQCFreserved4();
    private: void PMJQCFreserved5();
    private: void PMJQCFreserved6();

  };

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_QUEUECONNECTIONFACTORY_H_
