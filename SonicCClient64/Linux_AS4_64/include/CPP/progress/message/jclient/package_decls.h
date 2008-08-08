/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */

#ifndef _PROGRESS_MESSAGE_JCLIENT_PACKAGE_DECLS_H_
#define _PROGRESS_MESSAGE_JCLIENT_PACKAGE_DECLS_H_

#include <java/lang/package_decls.h>
#include <jmsconstants.h>


namespace progress { namespace message { namespace jclient {

// Forward class declarations.

class JMSException;
class IllegalStateException;
class InvalidClientIDException;
class InvalidDestinationException;
class InvalidSelectorException;
class JMSSecurityException;
class MessageEOFException;
class MessageFormatException;
class MessageNotReadableException;
class MessageNotWriteableException;
class ResourceAllocationException;
class TransactionInProgressException;
class TransactionRolledBackException;

SMQ_DECLARE_REF(JMSException, Exception)
SMQ_DECLARE_REF(IllegalStateException,JMSException)
SMQ_DECLARE_REF(InvalidClientIDException,JMSException)
SMQ_DECLARE_REF(InvalidDestinationException,JMSException)
SMQ_DECLARE_REF(InvalidSelectorException,JMSException)
SMQ_DECLARE_REF(JMSSecurityException,JMSException)
SMQ_DECLARE_REF(MessageEOFException,JMSException)
SMQ_DECLARE_REF(MessageFormatException,JMSException)
SMQ_DECLARE_REF(MessageNotReadableException,JMSException)
SMQ_DECLARE_REF(MessageNotWriteableException,JMSException)
SMQ_DECLARE_REF(ResourceAllocationException,JMSException)
SMQ_DECLARE_REF(TransactionInProgressException,JMSException)
SMQ_DECLARE_REF(TransactionRolledBackException,JMSException)

class BrokerName;
class BytesMessage;
class StreamMessage;
class Connection;
class ConnectionFactory;
class ConnectionMetaData;
class Constants; // No ConstantsRef defined
class Destination;
class DurableSubscriber;
class ErrorCodes; // No ErrorCodesRef defined
class ILoginSPI;
class ITopicSubscriber;
class Message;
class MessageConsumer;
class MessageProducer;
class Queue;
class QueueBrowser;
class QueueConnection;
class QueueConnectionFactory;
class QueueReceiver;
class QueueSender;
class QueueSession;
class Session;
class TemporaryQueue;
class TemporaryTopic;
class TextMessage;
class Topic;
class TopicConnection;
class TopicConnectionFactory;
class TopicPublisher;
class TopicSession;
class TopicSubscriber;

SMQ_DECLARE_REF(BrokerName,Object)
SMQ_DECLARE_REF(Connection,Object)
SMQ_DECLARE_REF(ConnectionFactory,Object)
SMQ_DECLARE_REF(ConnectionMetaData,Object)
SMQ_DECLARE_REF(Destination,Object)
SMQ_DECLARE_REF(Message,Object)
SMQ_DECLARE_REF(BytesMessage,Message)
SMQ_DECLARE_REF(StreamMessage,Message)
SMQ_DECLARE_REF(MessageConsumer,Object)
SMQ_DECLARE_REF(MessageProducer,Object)
SMQ_DECLARE_REF(ILoginSPI,Object)
SMQ_DECLARE_REF(ITopicSubscriber,MessageConsumer)
SMQ_DECLARE_REF(DurableSubscriber,ITopicSubscriber)
SMQ_DECLARE_REF(Session,Object)
SMQ_DECLARE_REF(Queue,Destination)
SMQ_DECLARE_REF(QueueBrowser,Object)
SMQ_DECLARE_REF(QueueConnection,Connection)
SMQ_DECLARE_REF(QueueConnectionFactory,ConnectionFactory)
SMQ_DECLARE_REF(QueueReceiver,MessageConsumer)
SMQ_DECLARE_REF(QueueSender,MessageProducer)
SMQ_DECLARE_REF(QueueSession,Session)
SMQ_DECLARE_REF(TemporaryQueue,Queue)
SMQ_DECLARE_REF(Topic,Destination)
SMQ_DECLARE_REF(TemporaryTopic,Topic)
SMQ_DECLARE_REF(TextMessage,Message)
SMQ_DECLARE_REF(TopicConnection,Connection)
SMQ_DECLARE_REF(TopicConnectionFactory,ConnectionFactory)
SMQ_DECLARE_REF(TopicPublisher,MessageProducer)
SMQ_DECLARE_REF(TopicSession,Session)
SMQ_DECLARE_REF(TopicSubscriber,ITopicSubscriber)


extern "C" {
typedef void (*pfnMessageListener)(MessageRef msg);
typedef void (*pfnExceptionListener)(ExceptionRef e);
}

}}} // namespace progress::message::jclient

using namespace progress::message::jclient;

#endif // _PROGRESS_MESSAGE_JCLIENT_PACKAGE_DECLS_H_
