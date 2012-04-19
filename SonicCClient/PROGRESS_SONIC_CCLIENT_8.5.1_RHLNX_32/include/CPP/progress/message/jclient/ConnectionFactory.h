/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#ifndef _PROGRESS_MESSAGE_JCLIENT_CONNECTIONFACTORY_H_
#define _PROGRESS_MESSAGE_JCLIENT_CONNECTIONFACTORY_H_

#include <java/lang/package.h>
#include <progress/message/jclient/package_decls.h>
#include <progress/message/jclient/ILoginSPI.h>


namespace progress { namespace message { namespace jclient {

class SMQ_API ConnectionFactory : public java::lang::Object
{
  public:

    virtual ~ConnectionFactory();

	/**
	 * Returns the int corresponding to the ConnectionFactory type.
	 *
	 * @return the int corresponding to the ConnectionFactory type
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
	 * Retrieve the broker hostname.
	 *
	 * @return String w/ the broker hostname.
	 */
    virtual StringRef getBrokerHostName();
	/**
	 * Retrieve the broker port number.
	 *
	 * @return the broker port number.
	 */
    virtual jint getBrokerPort();
	/**
	 * Retrieve the broker protocol.
	 *
	 * @return String containing the broker protocol.
	 */
    virtual StringRef getBrokerProtocol();
	/**
	 * Retrieve the broker URL.
	 *
	 * @return String containing the broker URL.
	 */
    virtual StringRef getBrokerURL();
	/**
	 * Retrieve the client ID.
	 *
	 * @return String containing the client ID.
	 */
    virtual StringRef getClientID();
	/**
	 * Retrieve the connect ID.
	 *
	 * @return String containing the connect ID.
	 */
    virtual StringRef getConnectID();
	/**
	 * Get list of brokers to try to connect to.
	 *
	 * @return String containing a comma-separated list of broker URLs.
	 */
    virtual StringRef getConnectionURLs();
	/**
	 * Retrieve the default password.
	 *
	 * @return String containing the default password.
	 */
    virtual StringRef getDefaultPassword();
	/**
	 * Retrieve the default username.
	 *
	 * @return String containing the default username.
	 */
    virtual StringRef getDefaultUser();
	/**
	 * Determines whether client-side load balancing is enabled.
	 *
	 * @return true, indicates that the client is willing to have a connect request
	 *        redirected to another broker within a SonicMQ cluster.
	 *
	 * @see    ConnectionFactory_setLoadBalancing()
	 */
    virtual jboolean getLoadBalancing();

    /**
     * Gets the administrative flow control monitoring interval.
     *
     * @return The interval in seconds over which administrative monitoring of flow control will occur.
     *         A value of 0 (zero) indicates that no monitoring will occur.
     *
     * @see    setMonitorInterval(Integer)
     */
    virtual jint getMonitorInterval();

	/**
	 * Determines whether the option to start with the first or a random element of the broker list
	 * has been selected.
	 *
	 * @return true, connect attempts start with the first broker in the list.
	 *        If false, connect attempts start with a random element in the broker list.
	 *
	 * @see ConnectionFactory_setSequential()
	 * @see ConnectionFactory_setConnectionURLs()
	 * @see ConnectionFactory_getConnectionURLs()
	 */
    virtual jboolean getSequential();
	/**
	 * Determines whether the option to disable Nagle's algorithm for TCP connections
	 * has been selected.
	 *
	 * @return jboolean - if true, created TCP connections will have Nagle's algorithm
	 *        disabled.
	 */
    virtual jboolean getTcpNoDelay();
	/**
	 * Sets the broker hostname.
	 *
	 * @param brokerHostName a String w/ the new broker hostname to set.
	 */
    virtual void setBrokerHostName(StringRef brokerHostName);
	/**
	 * Sets the broker port number.
	 *
	 * @param brokerPort the new broker port number to set.
	 */
    virtual void setBrokerPort(jint brokerPort);
	/**
	 * Sets the broker protocol.
	 *
	 * @param brokerProtocol a String w/ the new broker protocol to set.
	 */
    virtual void setBrokerProtocol(StringRef brokerProtocol);
	/**
	 * Sets the broker URL.
	 *
	 * @param brokerURL a String w/ the new broker URL to set.
	 */
    virtual void setBrokerURL(StringRef brokerURL);
	/**
	 * Sets the client ID.
	 *
	 * @param clientID a String w/ the new client ID to set.
	 */
    virtual void setClientID(StringRef clientID);
	/**
	 * Sets the connect ID.
	 *
	 * @param connectID a String w/ the new connect ID to set.
	 */
    virtual void setConnectID(StringRef connectID);
	/**
	 * Configures a list of brokers to try when creating a connection.
	 *
	 * @param brokerList   a String containing a comma-separated list of broker URLs.
	 *                     Allows a client to connect to the first available broker on a list.
	 *                     If present, this parameter overrides the brokerURL parameter in the
	 *                     ConnectionFactory constructor, which specifies a single broker.
	 *                     This option can be used independently of any other load balancing options.
	 *
	 * @see ConnectionFactory_getConnectionURLs()
	 * @see ConnectionFactory_setSequential()
	 * @see ConnectionFactory_getSequential()
	 */
    virtual void setConnectionURLs(StringRef brokerList);
	/**
	 * Enable or disable client-side load balancing.
	 *
	 * @param loadBalancing    if true, indicates that the client is willing to have
	 *                         a connect request re-directed to another broker within
	 *                         a SonicMQ cluster.
	 *
	 * @see ConnectionFactory_getLoadBalancing()
	 */
    virtual void setLoadBalancing(jboolean loadBalancing);

    /** Set the administrative flow control monitoring interval.
    * <p>This method which takes a Integer argument is provided for interoperability
    * with object storage systems which require that the attributes of
    * the stored object be of Object datatype.
    *
    * @param interval The interval in seconds over which administrative monitoring of flow control will occur.
    *                 A value of 0 (zero) indicates that no monitoring will occur.
    * @see    getMonitorInterval()
    */
    virtual void setMonitorInterval(jint interval);

	/**
	 * Specifies whether to start with the first broker in the list or a random element.
	 *
	 * @param sequential   If true, starts attempting to connect to the first broker
	 *                     in the list; if false, starts attempting to connect to a random element in
	 *                     the list.
	 *                     After that, tries to connect to each broker in sequence
	 *                     until a successful connect occurs, or the list is exhausted.
	 *
	 * @see ConnectionFactory_getSequential()
	 * @see ConnectionFactory_setConnectionURLs()
	 * @see ConnectionFactory_getConnectionURLs()
	 */
    virtual void setSequential(jboolean sequential);
	/**
	 * Enables or disables Nagle's algorithm for TCP connections.
	 *
	 * @param noDelay -	If true, created connections have Nagle's algorithm disabled.
	 */
    virtual void setTcpNoDelay(jboolean noDelay);

    /**
    * Set the SSL properties for the connection.
    *
    * @param caDir the CA directory containing trusted certs in PEM format
    *              the certs are identified by CA subject name hash values
    *              the hash link files are created using c_rehash utility
    * @param clientCert the client certificate for mutual authentication
    *                   ignored if broker is enabled for server authentication
    *                   pass NULL if broker is enabled for server authentication
    * @param privateKey the private key for the client cert
    *                   ignored if broker is enabled for server authentication
    *                   pass NULL if broker is enabled for server authentication
    * @param ciphers the preferred cipher suite algorithms
    *                if NULL, default is "RC4-SHA:RC4-MD5"
    *                check http://www.openssl.org/docs/apps/ciphers.html#
    */
    void setSSLProperties(StringRef caDir, StringRef clientCert, StringRef privKey, StringRef ciphers);

    /** C++ interface to register the LoginSPI with the topic connection factory.
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
    virtual void setLoginSPI(ILoginSPI *loginSPI);

    /** Sets the default batch size of all sessions, created via connections
      * created from this ConnectionFactory. A Session's batch size specifies
      * the number of bytes to be batched on the client before flushing the
      * batched messages.  This is a performance optimization utilized on transacted
      * sessions.  The size parameter is a measure of the message payloads only.
      *
      * @param size  The number of bytes to batch, specified as a jint.
      *
      */
    virtual void setDefaultTxnBatchSize(jint size);

    /** Gets the default batch size
      *
      * @return The number of bytes to batch, specified as an int.
      */
    virtual jint getDefaultTxnBatchSize();

    /** Sets the TCP connection timeout, in seconds.  This is the number of
      * seconds that a C-Client will wait for a connection to be established.
      * Default is 30, which allows TCP connection to connect in the allotted time.
      * timeout value zero means allow the TCP connect call to time out on its own.
      *
      * @param size  The number of seconds to wait for a connection.
      *
      * @see    getTCPConnectionTimeout()
      */
    virtual void setTCPConnectionTimeout(jint connectTimeout);

    /** Gets the TCP connection timeout
      *
      * @return The number of seconds to wait for a connection.
      * @see    setTCPConnectionTimeout(Integer)
      */
    virtual jint getTCPConnectionTimeout();

    /** Set the default Password for connections created from this factory.
     *  @param password The password as a String.
     */
    virtual void setDefaultPassword(StringRef defaultPassword);

    /** Set the default Username for connections created from this factory.
     *  @param username The Username as a String.
     */
    virtual void setDefaultUser(StringRef defaultUserName);


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
    virtual void setFaultTolerant(jboolean faultTolerant);

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
    virtual jboolean getFaultTolerant();

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
    virtual void setFaultTolerantReconnectTimeout(jint seconds);

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
    virtual jint getFaultTolerantReconnectTimeout();

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
    virtual void setInitialConnectTimeout(jint seconds);

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
    virtual jint getInitialConnectTimeout();


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
    virtual void setClientTransactionBufferSize(jlong size);

     /** Get client transaction buffer size.
      *
      * @return client transaction buffer size.
      * @see    setClientTransactionBufferSize(Long)
      * @see    setFaultTolerant(Boolean)
      * @see    getFaultTolerant()
      */
    virtual jlong getClientTransactionBufferSize();

    /**
    * Enable or disable the minimizing subscriber traffic
    *
    * @param minimizeTraffic If true, minimizing the subscriber traffic.
    */
    void setMinimizeSubscriberTraffic(jboolean minimizeTraffic);

    /**
    * Determines whether the option to disable the minimizing subscriber traffic
    * has been selected.
    *
    * @return jboolean - if true, Minimizes the subscriber traffic
    */
    jboolean getMinimizeSubscriberTraffic();

    /**
    * Set to indicate if compression should be enabled for the connections.
    *
    * @param enabled	the boolean value to indicate if compression should enabled.
    */
    void setEnableCompression(jboolean enabled);

    /**
    * Queries the ConnectionFactory to see if compression is enabled.
    * 
    * @return	the boolean value indicating if compression is enabled.
    */
    jboolean getEnableCompression();


    private: void PMJCFreserved0();
    private: void PMJCFreserved1();
    private: void PMJCFreserved2();
    private: void PMJCFreserved3();

    private: void PMJCFreserved4();
    private: void PMJCFreserved5();
    private: void PMJCFreserved6();
    private: void PMJCFreserved7();

};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_CONNECTIONFACTORY_H_
