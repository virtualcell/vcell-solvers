#ifndef _INCLUDE_JMSCONSTANTS_H_
#define _INCLUDE_JMSCONSTANTS_H_
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

/*
 * This file contains JMS constants common to C and C++ interfaces.
 */

/**
 * Message has been marked as undelivered for an unknown reason.
 * This value will typically never be seen and is included for
 * completeness.
 */
#define Constants_UNDELIVERED_UNKNOWN_REASON ((jint)0)

/**
 * Message has been marked as undelivered because the
 * current system time on the broker (as GMT) exceeds the
 * message's expiration time.
 */
#define Constants_UNDELIVERED_TTL_EXPIRED ((jint)1)

/**
 * Message with routing information arrived at a broker
 * that does not currently have routing enabled.
 *
 * @deprecated Routing is always enabled on a broker.
 *
 * <P>Since the message cannot be forwarded on to
 * a remote broker, it will be placed on the
 * dead message queue.
 */
#define Constants_UNDELIVERED_ROUTING_NOT_ENABLED ((jint)2)

/**
 * Message has been marked as undelivered while in the
 * broker's routing queue because the target routing node
 * in the destination cannot be found in the broker's
 * list of routing connections.
 *
 * <P>Typically, this is a programmer or administrative
 * error. The routing node in the destination set by
 * the JMS client does not exist in the set of valid
 * routing connections for this broker.
 */
#define Constants_UNDELIVERED_ROUTING_INVALID_NODE ((jint)3)

/**
 * Message has been marked as undelivered by a broker
 * which has received a message from a remote routing node.
 * The message is undeliverable because no destination
 * exists in the current broker or routing node for
 * the destination.
 *
 * <P>Typically, this is a programmer error at the sending
 * routing node where an invalid destination has been used.
 * It can also arise if a queue was removed from a broker
 * while a message was on route.
 */
#define Constants_UNDELIVERED_ROUTING_INVALID_DESTINATION ((jint)4)

/**
 * Message has been marked as undelivered by a broker
 * because a remote connection to the destination routing
 * node cannot be established over an extended period
 * of time.  The routing connection itself is considered
 * to have timed out, and messages are marked as
 * undeliverd.
 *
 * <P>Typically, this is due to network error, or simply
 * because the remote system has not been started.
 */
#define Constants_UNDELIVERED_ROUTING_TIMEOUT ((jint)5)

/**
 * Message has been marked as undelivered by a broker
 * because it has been left in an "in doubt" state
 * during its transfer between brokers.
 *
 * <P>Typically, this occurs due to a network failure
 * that occurs after some message has been sent between
 * brokers, but the acknowledgement cycle has not
 * completed.
 *
 * <P>The brokers will make an effort to reestablish the
 * connection and resolve the ambiguity, but if this
 * state persists for a period of time, then the indoubt
 * messages are marked as undelivered.
 */
#define Constants_UNDELIVERED_ROUTING_INDOUBT ((jint)6)

/**
 * Message with routing information could not be delivered
 * to specified node due to authentication (invalid credentials)
 * error while establishing connection to remote broker.
 *
 * <P>Since the message cannot be forwarded on to
 * a remote broker, it will be placed on the
 * dead message queue.
 */
#define Constants_UNDELIVERED_ROUTING_CONNECTION_AUTHENTICATION_FAILURE ((jint)7)

/**
 * Message with routing information could not be delivered
 * to specified node due to authorization (insufficient permissions)
 * error while establishing connection to remote broker
 *
 * <P>Since the message cannot be forwarded on to
 * a remote broker, it will be placed on the
 * dead message queue.
 */
#define Constants_UNDELIVERED_ROUTING_CONNECTION_AUTHORIZATION_FAILURE ((jint)8)

/**
 * Message could not be delivered to the destination queue because it
 *  is larger than the maximum size of the queue.
 */
#define Constants_UNDELIVERED_MESSAGE_TOO_LARGE_FOR_QUEUE ((jint)9)

/**
 * Message could not be delivered to the destination because the remote node
 * doesn't support remote topic messages.
 */
#define Constants_UNDELIVERED_ROUTING_TOPIC_MESSAGES_NOT_SUPPORTED ((jint)18)

/**
 * Subscription request could not be delivered to the destination because the remote node
 * denies subscribe permission to the subscription's topic.
 */
#define Constants_UNDELIVERED_ROUTING_SUBSCRIPTION_AUTHORIZATION_FAILURE ((jint)19)

/**
 * Subscription request could not be delivered to the destination because the remote node
 * doesn't support remote subscriptions.
 */
#define Constants_UNDELIVERED_ROUTING_REMOTE_SUBSCRIPTIONS_NOT_SUPPORTED ((jint)20)

/**
 * Connection state for an active JMS connection (normal state)
 */
#define Constants_ACTIVE  ((jint)0)

/**
 * Connection state for a reconnecting fault-tolerant JMS connection. The connection has detected a failure
 * and is attempting to reconnect automatically.
 */
#define Constants_RECONNECTING ((jint)1)

/**
 * Connection state for a failed JMS connection.
 */
#define Constants_FAILED ((jint)2)

/**
 * Connection state constant for a closed JMS connection
 */
#define Constants_CLOSED ((jint)3)

#define DeliveryMode_NON_PERSISTENT ((jint)1)
#define DeliveryMode_PERSISTENT ((jint)2)
#define DeliveryMode_NON_PERSISTENT_ASYNC ((jint)102)
#define DeliveryMode_NON_PERSISTENT_SYNC ((jint)103)
#define DeliveryMode_DISCARDABLE ((jint)104)

#define ErrorCodes_ERROR ((jint)-1)
#define ErrorCodes_ERR_PRIVACY_FAILED ((jint)-2)
#define ErrorCodes_ERR_INTEGRITY_FAILED ((jint)-3)
#define ErrorCodes_ERR_NONREPUDIATION_FAILED ((jint)-4)
#define ErrorCodes_ERR_CONNECTION_DROPPED ((jint)-5)
#define ErrorCodes_ERR_PUBLISH_NOT_AUTHORIZED ((jint)-6)
#define ErrorCodes_ERR_SUBSCRIBE_NOT_AUTHORIZED ((jint)-7)
#define ErrorCodes_ERR_GUARANTEE_NOT_AUTHORIZED ((jint)-8)
#define ErrorCodes_ERR_WRONG_SUBJECT_ADDR ((jint)-9)
#define ErrorCodes_ERR_GENERAL_SECURITY_ERR ((jint)-10)
#define ErrorCodes_ERR_TXN_NOT_FOUND ((jint)-11)
#define ErrorCodes_ERR_TXN_ACCESS_VIOLATION ((jint)-12)
#define ErrorCodes_ERR_TXN_SEQUENCE_ERR ((jint)-13)
#define ErrorCodes_ERR_REQUEST_NOSUB_FOR_SUBJECT ((jint)-14)
#define ErrorCodes_ERR_MESSAGELISTENER_RUNTIME_EXCEPTION ((jint)-15)
#define ErrorCodes_ERR_FLOW_CONTROL_EXCEPTION ((jint)-16)
#define ErrorCodes_ERR_TOO_LARGE_FOR_QUEUE ((jint)-17)
#define ErrorCodes_TXN_INDEX_ALREADY_EXISTS ((jint)-18)
#define ErrorCodes_TXN_DATABASE_EXCEPTION ((jint)-19)
#define ErrorCodes_TXN_INVALID_DATA ((jint)-20)
#define ErrorCodes_TXN_INDICES_NOT_SUPPORTED ((jint)-21)
#define ErrorCodes_ERR_JMS_OBJECT_CLOSED ((jint)-22)
#define ErrorCodes_ERR_TXN_IDLE_TIMEOUT ((jint)-23)
#define ErrorCodes_ERR_THREAD_INTERRUPTED ((jint)-24)
#define ErrorCodes_ERR_EXTERNAL_AUTNENTICATION_FAILED ((jint)-25)
#define ErrorCodes_ERR_CONNECTION_LIMIT_EXCEEDED ((jint)-26)

#define Message_TYPE_UNDEFINED ((jshort)0)
#define Message_TYPE_QUEUE ((jshort)1)
#define Message_TYPE_TOPIC ((jshort)2)
#define Message_NATIVE_MESSAGE_TYPE ((jshort)0)
#define Message_BASE_MESSAGE_TYPE ((jshort)1)
#define Message_BYTES_MESSAGE_TYPE ((jshort)2)
#define Message_MAP_MESSAGE_TYPE ((jshort)3)
#define Message_OBJECT_MESSAGE_TYPE ((jshort)4)
#define Message_STREAM_MESSAGE_TYPE ((jshort)5)
#define Message_TEXT_MESSAGE_TYPE ((jshort)6)
#define Message_XML_MESSAGE_TYPE ((jshort)7)
#define Message_DEFAULT_DELIVERY_MODE ((jshort)2)
#define Message_DEFAULT_PRIORITY ((jshort)4)
#define Message_DEFAULT_TIME_TO_LIVE ((jshort)0)

/**
 * If a session is created with the non-JMS SINGLE_MESSAGE_ACKNOWLEDGE mode,
 * the acknowledge() method only acknowledges the message on which it is called.
 * This is unlike the JMS standard CLIENT_ACKNOWLEDGE mode, in which a call to
 * acknowledge() acknowledges the message on which it is called, and all messages
 * previously received within that session.
 */
#define Session_AUTO_ACKNOWLEDGE ((jint)1)
#define Session_CLIENT_ACKNOWLEDGE ((jint)2)
#define Session_DUPS_OK_ACKNOWLEDGE ((jint)3)
#define Session_SINGLE_MESSAGE_ACKNOWLEDGE ((jint)1004)


#endif /* _INCLUDE_JMSCONSTANTS_H_ */

