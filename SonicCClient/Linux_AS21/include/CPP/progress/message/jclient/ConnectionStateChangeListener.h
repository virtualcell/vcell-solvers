#ifndef _PROGRESS_MESSAGE_JCLIENT_ConnectionStateChangeListener_H_
#define _PROGRESS_MESSAGE_JCLIENT_ConnectionStateChangeListener_H_
/*
 * Copyright (c) 2003 Sonic Software Corporation. All Rights Reserved.
 *
 * This software is the confidential and proprietary information of Sonic
 * Software Corpoation. ("Confidential Information").  You shall not
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

/**
 * This class is implemented by developers who wish to be informed of changes in the
 * state of a connection.
 * <P>
 * connectionStateChanged must not call any JMS operations related to the connection other than
 * getConnectionState(), getBrokerURL(), getBrokerReconnectURLs() and getStandbyBrokerReconnectURLs().
 * <P>
 * It is recommended that you do not perform any time- or CPU-intensive processing in the
 * connectionStateChanged( ) method, as this may impede the client reconnect.
 * <P>
 * A fault-tolerant connection that fails and reconnects successfully will call its
 * ConnectionStateChangeListener with RECONNECTING state on the failure detection,
 * followed with ACTIVE state on successful reconnect.
 * <P>
 * A fault-tolerant connection that fails and fails to reconnect will call its
 * ConnectionStateChangeListener with RECONNECTING state on the initial failure detection,
 * followed with FAILED state when all reconnect attempts fail. Finally, if an ExceptionListener
 * is registered, ExceptionListener onException method is called
 * <P>
 * When a connection fails the ConnectionStateChangeListener connectionStateChanged method
 * is called before the ExceptionListener onException method.
 * <P>
 * @see progress.message.jclient.Connection#setConnectionStateChangeListener( ConnectionStateChangeListener )
 * @see progress.message.jclient.Connection#getConnectionStateChangeListener( )
 * @see progress.message.jclient.Connection#getConnectionState( )
 * @see progress.message.jclient.Connection#getBrokerURL( )
 * @see progress.message.jclient.Connection#getBrokerReconnectURLs( )
 * @see progress.message.jclient.Connection#getStandbyBrokerReconnectURLs( )
 * @see progress.message.jclient.Constants#ACTIVE
 * @see progress.message.jclient.Constants#RECONNECTING
 * @see progress.message.jclient.Constants#FAILED
 * @see progress.message.jclient.Constants#CLOSED
 */


namespace progress { namespace message { namespace jclient {

class SMQ_API ConnectionStateChangeListener
{
public:
    /**
     * Creates a ExceptionListener
     */
    ConnectionStateChangeListener();

	virtual ~ConnectionStateChangeListener();
  
    /**
     * virtual method expected to be overridden by user.
     */
	virtual void connectionStateChanged(jint state) = 0;
	/**
	 * Returns the int corresponding to the ConnectionStateChangeListener type.
	 *
	 * @return the int corresponding to the ConnectionStateChangeListener type
	 */
	static  int type();
    /**
     * Returns the type-code for this object's classtype.
     *
     * @return     the type-code for this object.
     */
	virtual int getType() const;
};

}}} // namespace progress::message::jclient

#endif // _PROGRESS_MESSAGE_JCLIENT_ConnectionStateChangeListener_H_

