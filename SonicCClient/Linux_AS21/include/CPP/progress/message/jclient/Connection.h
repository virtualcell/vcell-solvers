#ifndef _PROGRESS_MESSAGE_JCLIENT_CONNECTION_H_
#define _PROGRESS_MESSAGE_JCLIENT_CONNECTION_H_
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

#include <progress/message/jclient/package_decls.h>
#include <progress/message/jclient/ExceptionListener.h>
#include <progress/message/jclient/ConnectionStateChangeListener.h>


namespace progress { namespace message { namespace jclient {

class SMQ_API Connection : public java::lang::Object
{
public: 
    virtual ~Connection();

    /**
     * Returns the int corresponding to the Connection type.
     *
     * @return the int corresponding to the Connection type
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

    /** Get the client identifier for this connection.
      *
      * @return the unique client identifier.
      *
      * @exception JMSException if JMS implementation fails to return
      *                         the client ID for this Connection due
      *                         to some internal error.
      **/
    virtual StringRef getClientID();
    /** Set the client identifier for this connection.
      *
      * <P>The preferred way to assign a Client's client identifier is for
      * it to be configured in a client-specific ConnectionFactory and
      * transparently assigned to the Connection it creates. Alternatively,
      * a client can set a Connections's client identifier using a
      * provider-specific value.
      *
      * <P>The purpose of client identifier is to associate a session and
      * its objects with a state maintained on behalf of the client by a
      * provider. The only such state identified by JMS is that required
      * to support durable subscriptions
      *
      * @param clientID the unique client identifier
      *
      * @exception JMSException general exception if JMS implementation fails to
      *                         set the client ID for this Connection due
      *                         to some internal error.
      *
      * @exception javax::jms::InvalidClientIDException if JMS client specifies an
      *                         invalid or duplicate client id.
      */
    virtual void setClientID(StringRef clientID) ;
    /** Get the meta data for this connection.
      *
      * @return the connection meta data.
      *
      * @exception JMSException general exception if JMS implementation fails to
      *                         get the Connection meta-data for this Connection.
      */
    virtual ConnectionMetaDataRef getMetaData() ;
    /** Set an exception listener for this connection.
      *
      * This method is obsoleted by the class based listener setExceptionListenerObj.
      * It is retained for backward compatibility.
      *
      * <P>If a JMS provider detects a serious problem with a connection it
      * will inform the connection's ExceptionListener if one has been
      * registered. It does this by calling the listener's onException()
      * method passing it a JMSException describing the problem.
      *
      * <P>This allows a client to be asynchronously notified of a problem.
      * Some connections only consume messages so they would have no other
      * way to learn their connection has failed.
      *
      * <P>A Connection serializes execution of its ExceptionListener.
      *
      * <P>A JMS provider should attempt to resolve connection problems
      * itself prior to notifying the client of them.
      *
      * @param listener the exception listener.
      *
      * @exception JMSException general exception if JMS implementation fails to
      *                         set the Exception listener for this Connection.
      */
    virtual void setExceptionListener(pfnExceptionListener listener) ;
    /** Set an exception listener for this connection.
      *
      * <P>If a JMS provider detects a serious problem with a connection it
      * will inform the connection's ExceptionListener if one has been
      * registered. It does this by calling the listener's onException()
      * method passing it a JMSException describing the problem.
      *
      * <P>This allows a client to be asynchronously notified of a problem.
      * Some connections only consume messages so they would have no other
      * way to learn their connection has failed.
      *
      * <P>A Connection serializes execution of its ExceptionListener.
      *
      * <P>A JMS provider should attempt to resolve connection problems
      * itself prior to notifying the client of them.
      *
      * @param listener the exception listener object.
      *
      * @exception JMSException general exception if JMS implementation fails to
      *                         set the Exception listener for this Connection.
      */
    virtual void setExceptionListenerObj(ExceptionListener *listener) ;
    /**
     * Get the ExceptionListener for this Connection.
     *
     * This method is obsoleted by the class based listener setExceptionListenerObj.
     * It is retained for backward compatibility.
     *
     * @return the ExceptionListener for this Connection.
     *
     * @exception JMSException general exception if JMS implementation fails to
     *                         get the Exception listener for this Connection.
     */
    virtual pfnExceptionListener getExceptionListener() ;
    /**
     * Get the ExceptionListener for this Connection.
     *
     * @return the ExceptionListener object for this Connection.
     *
     * @exception JMSException general exception if JMS implementation fails to
     *                         get the Exception listener for this Connection.
     */
    virtual ExceptionListener *getExceptionListenerObj() ;

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
    /** Get the user of this connection.
      *
      * @return the user of connection.
      */
    virtual StringRef getUsername();
    /** Get the connect ID for this connection.
      * This is a non-JMS public method.
      *
      * @return the connect ID of connection.
      */
    virtual StringRef getConnectID();
    /** Enable/disable sending active pings on this connection.
      * This is a non-JMS public method.
      *
      * @param interval indicates the interval in seconds for sending a ping.
      *                 Setting interval to 0 or any negative value
      *                 effectively disables the ping.
      */
    virtual void setPingInterval(jlong interval);
   /**
    * Get URL of broker actually connected to.
    *
    * @return   URL of broker successfully connected to.
    */
    virtual StringRef getBrokerURL();

    /**
     * Returns true if the connection is fault tolerant
     * <P>
     * For a connection to be fault-tolerant, fault-tolerant must be set in the
     * ConnectionFactory, and the broker must support(be licensed for) fault-tolerance.
     *
     * @see progress.message.jclient.ConnectionFactory#setFaultTolerant
     */
    virtual jboolean isFaultTolerant();

    /**
     * Returns URLs available on a standby broker, if any, which is paired for
     * fault-tolerance with the currently connected broker, and that may be used
     * for the purpose re-connection to a standby broker in the event of currently
     * connected broker failure.
     * <P>
     * Re-connection is automatically performed by the client runtime. Standby
     * broker reconnect URLs are provided for informational use.
     * <P>
     * The standby broker reconnect URLs are derived from the broker configuration.
     * If a default routing URL is configured on the standby, this will be used
     * for standby re-connection. Otherwise, standby broker acceptors URLs, configured
     * with the same acceptor name as the currently connected broker URL are
     * used for re-connection.
     * <P>
     * If called after the connection is closed, the last known standby reconnect URLs
     * are returned.
     * <P>
     * If the currently connected broker is standalone, or, if from the configuration,
     * no redundant URLs are present on the standby, or, if the connection is non
     * fault-tolerant, null is returned.
     * <P>
     * @see getBrokerURL()
     * @see getBrokerReconnectURLs()
     * @see isFaultTolerant()
     * @return an enumeration of URLs to the standby broker.
     */
    virtual EnumerationRef getStandbyBrokerReconnectURLs();


    /**
     * Returns URLs available on the currently connected broker, that may be used for
     * the purpose re-connection of fault-tolerant connections when temporary
     * or permanent network failure is encountered.
     * <P>
     * Re-connection is automatically performed by the client runtime. Broker reconnect URLs
     * are provided for informational use.
     * <P>
     * The broker reconnect URLs are derived from the broker configuration. If a default routing
     * URL is configured on the broker, the currently connected URL is used for reconnection.
     * Otherwise, acceptors URLs, configured with the same acceptor name as the currently
     * connectedURL are used for reconnection.
     * <P>
     * If called after the connection is closed, the last known reconnect URLs are
     * returned.
     *<P>
     * If the connection is non fault-tolerant, returns null.
     *
     * @return an enumeration of reconnect URLs
     * @see getBrokerURL()
     * @see getStandbyBrokerReconnectURLs()
     * @see isFaultTolerant()
     */
    virtual EnumerationRef getBrokerReconnectURLs();


    /**
     * Get connection state, one of ACTIVE, RECONNECTING, FAILED or CLOSED.
     * <P>
     * A non fault-tolerant connection will never see a RECONNECTING value.
     * This method may be called after the connection is closed.
     * @see progress.message.jclient.Constants#ACTIVE
     * @see progress.message.jclient.Constants#RECONNECTING
     * @see progress.message.jclient.Constants#FAILED
     * @see progress.message.jclient.Constants#CLOSED
     * @see progress.message.jclient.ConnectionStateChangeListener
     * @see setConnectionStateChangeListener(ConnectionStateChangeListener)
     * @see getConnectionStateChangeListener()
     * @return connection state
     */
    virtual jint getConnectionState();

    /**
     * Get connection state change listener.
     *
     * @see progress.message.jclient.ConnectionStateChangeListener
     * @see setConnectionStateChangeListener(ConnectionStateChangeListener)
     */
    virtual ConnectionStateChangeListener * getConnectionStateChangeListener();


    /**
     * Set connection state change listener.
     *
     * @see progress.message.jclient.ConnectionStateChangeListener
     * @see getConnectionStateChangeListener()
     */
    virtual void setConnectionStateChangeListener(ConnectionStateChangeListener * listener);
    
    /**
     * Returns the name of the DRA routing node currently associated with the
     *  connection.
     *
     * @return  a routing node name
     */
    virtual StringRef getRoutingNodeName();
    
    private: void PMJCONNreserved0();
    private: void PMJCONNreserved1();
    private: void PMJCONNreserved2();
    private: void PMJCONNreserved3();
    private: void PMJCONNreserved4();
    private: void PMJCONNreserved5();
    private: void PMJCONNreserved6();
    private: void PMJCONNreserved7();
    private: void PMJCONNreserved8();
    private: void PMJCONNreserved9();
    private: void PMJCONNreserved10();
    private: void PMJCONNreserved11();
    private: void PMJCONNreserved12();
    private: void PMJCONNreserved13();
    private: void PMJCONNreserved14();
    private: void PMJCONNreserved15();
    private: void PMJCONNreserved16();
    private: void PMJCONNreserved17();
    private: void PMJCONNreserved18();
    private: void PMJCONNreserved19();
    private: void PMJCONNreserved20();
    private: void PMJCONNreserved21();
    private: void PMJCONNreserved22();
    private: void PMJCONNreserved23();
    private: void PMJCONNreserved24();
    private: void PMJCONNreserved25();
    private: void PMJCONNreserved26();
    private: void PMJCONNreserved27();
    private: void PMJCONNreserved28();
    private: void PMJCONNreserved29();
    private: void PMJCONNreserved30();
    private: void PMJCONNreserved31();
    private: void PMJCONNreserved32();
    private: void PMJCONNreserved33();
    private: void PMJCONNreserved34();

    private: void PMJCONNreserved35();

    private: void PMJCONNreserved36();
    private: void PMJCONNreserved37();
    private: void PMJCONNreserved38();

    
};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_CONNECTION_H_
