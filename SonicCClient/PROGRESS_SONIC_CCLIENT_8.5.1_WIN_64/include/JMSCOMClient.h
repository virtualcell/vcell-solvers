

/* this ALWAYS GENERATED file contains the definitions for the interfaces */


 /* File created by MIDL compiler version 6.00.0366 */
/* at Fri Mar 02 08:30:14 2012
 */
/* Compiler settings for JMSCOMClient.idl:
    Oicf, W1, Zp8, env=Win32 (32b run)
    protocol : dce , ms_ext, c_ext, robust
    error checks: allocation ref bounds_check enum stub_data 
    VC __declspec() decoration level: 
         __declspec(uuid()), __declspec(selectany), __declspec(novtable)
         DECLSPEC_UUID(), MIDL_INTERFACE()
*/
//@@MIDL_FILE_HEADING(  )

#pragma warning( disable: 4049 )  /* more than 64k source lines */


/* verify that the <rpcndr.h> version is high enough to compile this file*/
#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 475
#endif

#include "rpc.h"
#include "rpcndr.h"

#ifndef __RPCNDR_H_VERSION__
#error this stub requires an updated version of <rpcndr.h>
#endif // __RPCNDR_H_VERSION__

#ifndef COM_NO_WINDOWS_H
#include "windows.h"
#include "ole2.h"
#endif /*COM_NO_WINDOWS_H*/

#ifndef __JMSCOMClient_h__
#define __JMSCOMClient_h__

#if defined(_MSC_VER) && (_MSC_VER >= 1020)
#pragma once
#endif

/* Forward Declarations */ 

#ifndef __IJMSObject_FWD_DEFINED__
#define __IJMSObject_FWD_DEFINED__
typedef interface IJMSObject IJMSObject;
#endif 	/* __IJMSObject_FWD_DEFINED__ */


#ifndef __IJMSEnumeration_FWD_DEFINED__
#define __IJMSEnumeration_FWD_DEFINED__
typedef interface IJMSEnumeration IJMSEnumeration;
#endif 	/* __IJMSEnumeration_FWD_DEFINED__ */


#ifndef __IJMSThrowable_FWD_DEFINED__
#define __IJMSThrowable_FWD_DEFINED__
typedef interface IJMSThrowable IJMSThrowable;
#endif 	/* __IJMSThrowable_FWD_DEFINED__ */


#ifndef __IJMSCOMException_FWD_DEFINED__
#define __IJMSCOMException_FWD_DEFINED__
typedef interface IJMSCOMException IJMSCOMException;
#endif 	/* __IJMSCOMException_FWD_DEFINED__ */


#ifndef __IJMSConnectionMetaData_FWD_DEFINED__
#define __IJMSConnectionMetaData_FWD_DEFINED__
typedef interface IJMSConnectionMetaData IJMSConnectionMetaData;
#endif 	/* __IJMSConnectionMetaData_FWD_DEFINED__ */


#ifndef __IJMSQueueConnectionFactory_FWD_DEFINED__
#define __IJMSQueueConnectionFactory_FWD_DEFINED__
typedef interface IJMSQueueConnectionFactory IJMSQueueConnectionFactory;
#endif 	/* __IJMSQueueConnectionFactory_FWD_DEFINED__ */


#ifndef __IJMSTopicConnectionFactory_FWD_DEFINED__
#define __IJMSTopicConnectionFactory_FWD_DEFINED__
typedef interface IJMSTopicConnectionFactory IJMSTopicConnectionFactory;
#endif 	/* __IJMSTopicConnectionFactory_FWD_DEFINED__ */


#ifndef __IJMSDestination_FWD_DEFINED__
#define __IJMSDestination_FWD_DEFINED__
typedef interface IJMSDestination IJMSDestination;
#endif 	/* __IJMSDestination_FWD_DEFINED__ */


#ifndef __IJMSQueue_FWD_DEFINED__
#define __IJMSQueue_FWD_DEFINED__
typedef interface IJMSQueue IJMSQueue;
#endif 	/* __IJMSQueue_FWD_DEFINED__ */


#ifndef __IJMSTopic_FWD_DEFINED__
#define __IJMSTopic_FWD_DEFINED__
typedef interface IJMSTopic IJMSTopic;
#endif 	/* __IJMSTopic_FWD_DEFINED__ */


#ifndef __IJMSTemporaryQueue_FWD_DEFINED__
#define __IJMSTemporaryQueue_FWD_DEFINED__
typedef interface IJMSTemporaryQueue IJMSTemporaryQueue;
#endif 	/* __IJMSTemporaryQueue_FWD_DEFINED__ */


#ifndef __IJMSTemporaryTopic_FWD_DEFINED__
#define __IJMSTemporaryTopic_FWD_DEFINED__
typedef interface IJMSTemporaryTopic IJMSTemporaryTopic;
#endif 	/* __IJMSTemporaryTopic_FWD_DEFINED__ */


#ifndef __IJMSMessage_FWD_DEFINED__
#define __IJMSMessage_FWD_DEFINED__
typedef interface IJMSMessage IJMSMessage;
#endif 	/* __IJMSMessage_FWD_DEFINED__ */


#ifndef __IJMSBytesMessage_FWD_DEFINED__
#define __IJMSBytesMessage_FWD_DEFINED__
typedef interface IJMSBytesMessage IJMSBytesMessage;
#endif 	/* __IJMSBytesMessage_FWD_DEFINED__ */


#ifndef __IJMSObjectMessage_FWD_DEFINED__
#define __IJMSObjectMessage_FWD_DEFINED__
typedef interface IJMSObjectMessage IJMSObjectMessage;
#endif 	/* __IJMSObjectMessage_FWD_DEFINED__ */


#ifndef __IJMSStreamMessage_FWD_DEFINED__
#define __IJMSStreamMessage_FWD_DEFINED__
typedef interface IJMSStreamMessage IJMSStreamMessage;
#endif 	/* __IJMSStreamMessage_FWD_DEFINED__ */


#ifndef __IJMSTextMessage_FWD_DEFINED__
#define __IJMSTextMessage_FWD_DEFINED__
typedef interface IJMSTextMessage IJMSTextMessage;
#endif 	/* __IJMSTextMessage_FWD_DEFINED__ */


#ifndef __IJMSConnection_FWD_DEFINED__
#define __IJMSConnection_FWD_DEFINED__
typedef interface IJMSConnection IJMSConnection;
#endif 	/* __IJMSConnection_FWD_DEFINED__ */


#ifndef __IJMSQueueConnection_FWD_DEFINED__
#define __IJMSQueueConnection_FWD_DEFINED__
typedef interface IJMSQueueConnection IJMSQueueConnection;
#endif 	/* __IJMSQueueConnection_FWD_DEFINED__ */


#ifndef __IJMSTopicConnection_FWD_DEFINED__
#define __IJMSTopicConnection_FWD_DEFINED__
typedef interface IJMSTopicConnection IJMSTopicConnection;
#endif 	/* __IJMSTopicConnection_FWD_DEFINED__ */


#ifndef __IJMSMessageConsumer_FWD_DEFINED__
#define __IJMSMessageConsumer_FWD_DEFINED__
typedef interface IJMSMessageConsumer IJMSMessageConsumer;
#endif 	/* __IJMSMessageConsumer_FWD_DEFINED__ */


#ifndef __IJMSQueueReceiver_FWD_DEFINED__
#define __IJMSQueueReceiver_FWD_DEFINED__
typedef interface IJMSQueueReceiver IJMSQueueReceiver;
#endif 	/* __IJMSQueueReceiver_FWD_DEFINED__ */


#ifndef __IJMSTopicSubscriber_FWD_DEFINED__
#define __IJMSTopicSubscriber_FWD_DEFINED__
typedef interface IJMSTopicSubscriber IJMSTopicSubscriber;
#endif 	/* __IJMSTopicSubscriber_FWD_DEFINED__ */


#ifndef __IJMSMessageProducer_FWD_DEFINED__
#define __IJMSMessageProducer_FWD_DEFINED__
typedef interface IJMSMessageProducer IJMSMessageProducer;
#endif 	/* __IJMSMessageProducer_FWD_DEFINED__ */


#ifndef __IJMSQueueSender_FWD_DEFINED__
#define __IJMSQueueSender_FWD_DEFINED__
typedef interface IJMSQueueSender IJMSQueueSender;
#endif 	/* __IJMSQueueSender_FWD_DEFINED__ */


#ifndef __IJMSTopicPublisher_FWD_DEFINED__
#define __IJMSTopicPublisher_FWD_DEFINED__
typedef interface IJMSTopicPublisher IJMSTopicPublisher;
#endif 	/* __IJMSTopicPublisher_FWD_DEFINED__ */


#ifndef __IJMSSession_FWD_DEFINED__
#define __IJMSSession_FWD_DEFINED__
typedef interface IJMSSession IJMSSession;
#endif 	/* __IJMSSession_FWD_DEFINED__ */


#ifndef __IJMSQueueSession_FWD_DEFINED__
#define __IJMSQueueSession_FWD_DEFINED__
typedef interface IJMSQueueSession IJMSQueueSession;
#endif 	/* __IJMSQueueSession_FWD_DEFINED__ */


#ifndef __IJMSTopicSession_FWD_DEFINED__
#define __IJMSTopicSession_FWD_DEFINED__
typedef interface IJMSTopicSession IJMSTopicSession;
#endif 	/* __IJMSTopicSession_FWD_DEFINED__ */


#ifndef __IJMSQueueBrowser_FWD_DEFINED__
#define __IJMSQueueBrowser_FWD_DEFINED__
typedef interface IJMSQueueBrowser IJMSQueueBrowser;
#endif 	/* __IJMSQueueBrowser_FWD_DEFINED__ */


#ifndef __IJMSDurableSubscriber_FWD_DEFINED__
#define __IJMSDurableSubscriber_FWD_DEFINED__
typedef interface IJMSDurableSubscriber IJMSDurableSubscriber;
#endif 	/* __IJMSDurableSubscriber_FWD_DEFINED__ */


#ifndef __IJMSMapMessage_FWD_DEFINED__
#define __IJMSMapMessage_FWD_DEFINED__
typedef interface IJMSMapMessage IJMSMapMessage;
#endif 	/* __IJMSMapMessage_FWD_DEFINED__ */


#ifndef __IJMSMessageListener_FWD_DEFINED__
#define __IJMSMessageListener_FWD_DEFINED__
typedef interface IJMSMessageListener IJMSMessageListener;
#endif 	/* __IJMSMessageListener_FWD_DEFINED__ */


#ifndef __IJMSExceptionListener_FWD_DEFINED__
#define __IJMSExceptionListener_FWD_DEFINED__
typedef interface IJMSExceptionListener IJMSExceptionListener;
#endif 	/* __IJMSExceptionListener_FWD_DEFINED__ */


#ifndef __IJMSConnectionStateChangeListener_FWD_DEFINED__
#define __IJMSConnectionStateChangeListener_FWD_DEFINED__
typedef interface IJMSConnectionStateChangeListener IJMSConnectionStateChangeListener;
#endif 	/* __IJMSConnectionStateChangeListener_FWD_DEFINED__ */


#ifndef __CJMSObject_FWD_DEFINED__
#define __CJMSObject_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSObject CJMSObject;
#else
typedef struct CJMSObject CJMSObject;
#endif /* __cplusplus */

#endif 	/* __CJMSObject_FWD_DEFINED__ */


#ifndef __CJMSQueueConnectionFactory_FWD_DEFINED__
#define __CJMSQueueConnectionFactory_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSQueueConnectionFactory CJMSQueueConnectionFactory;
#else
typedef struct CJMSQueueConnectionFactory CJMSQueueConnectionFactory;
#endif /* __cplusplus */

#endif 	/* __CJMSQueueConnectionFactory_FWD_DEFINED__ */


#ifndef __CJMSTopicConnectionFactory_FWD_DEFINED__
#define __CJMSTopicConnectionFactory_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSTopicConnectionFactory CJMSTopicConnectionFactory;
#else
typedef struct CJMSTopicConnectionFactory CJMSTopicConnectionFactory;
#endif /* __cplusplus */

#endif 	/* __CJMSTopicConnectionFactory_FWD_DEFINED__ */


#ifndef __CJMSDestination_FWD_DEFINED__
#define __CJMSDestination_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSDestination CJMSDestination;
#else
typedef struct CJMSDestination CJMSDestination;
#endif /* __cplusplus */

#endif 	/* __CJMSDestination_FWD_DEFINED__ */


#ifndef __CJMSQueue_FWD_DEFINED__
#define __CJMSQueue_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSQueue CJMSQueue;
#else
typedef struct CJMSQueue CJMSQueue;
#endif /* __cplusplus */

#endif 	/* __CJMSQueue_FWD_DEFINED__ */


#ifndef __CJMSTopic_FWD_DEFINED__
#define __CJMSTopic_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSTopic CJMSTopic;
#else
typedef struct CJMSTopic CJMSTopic;
#endif /* __cplusplus */

#endif 	/* __CJMSTopic_FWD_DEFINED__ */


#ifndef __CJMSTemporaryQueue_FWD_DEFINED__
#define __CJMSTemporaryQueue_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSTemporaryQueue CJMSTemporaryQueue;
#else
typedef struct CJMSTemporaryQueue CJMSTemporaryQueue;
#endif /* __cplusplus */

#endif 	/* __CJMSTemporaryQueue_FWD_DEFINED__ */


#ifndef __CJMSTemporaryTopic_FWD_DEFINED__
#define __CJMSTemporaryTopic_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSTemporaryTopic CJMSTemporaryTopic;
#else
typedef struct CJMSTemporaryTopic CJMSTemporaryTopic;
#endif /* __cplusplus */

#endif 	/* __CJMSTemporaryTopic_FWD_DEFINED__ */


#ifndef __CJMSMessage_FWD_DEFINED__
#define __CJMSMessage_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSMessage CJMSMessage;
#else
typedef struct CJMSMessage CJMSMessage;
#endif /* __cplusplus */

#endif 	/* __CJMSMessage_FWD_DEFINED__ */


#ifndef __CJMSBytesMessage_FWD_DEFINED__
#define __CJMSBytesMessage_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSBytesMessage CJMSBytesMessage;
#else
typedef struct CJMSBytesMessage CJMSBytesMessage;
#endif /* __cplusplus */

#endif 	/* __CJMSBytesMessage_FWD_DEFINED__ */


#ifndef __CJMSObjectMessage_FWD_DEFINED__
#define __CJMSObjectMessage_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSObjectMessage CJMSObjectMessage;
#else
typedef struct CJMSObjectMessage CJMSObjectMessage;
#endif /* __cplusplus */

#endif 	/* __CJMSObjectMessage_FWD_DEFINED__ */


#ifndef __CJMSStreamMessage_FWD_DEFINED__
#define __CJMSStreamMessage_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSStreamMessage CJMSStreamMessage;
#else
typedef struct CJMSStreamMessage CJMSStreamMessage;
#endif /* __cplusplus */

#endif 	/* __CJMSStreamMessage_FWD_DEFINED__ */


#ifndef __CJMSTextMessage_FWD_DEFINED__
#define __CJMSTextMessage_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSTextMessage CJMSTextMessage;
#else
typedef struct CJMSTextMessage CJMSTextMessage;
#endif /* __cplusplus */

#endif 	/* __CJMSTextMessage_FWD_DEFINED__ */


#ifndef __CJMSConnectionStateChangeListenerHelper_FWD_DEFINED__
#define __CJMSConnectionStateChangeListenerHelper_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSConnectionStateChangeListenerHelper CJMSConnectionStateChangeListenerHelper;
#else
typedef struct CJMSConnectionStateChangeListenerHelper CJMSConnectionStateChangeListenerHelper;
#endif /* __cplusplus */

#endif 	/* __CJMSConnectionStateChangeListenerHelper_FWD_DEFINED__ */


#ifndef __CJMSConnection_FWD_DEFINED__
#define __CJMSConnection_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSConnection CJMSConnection;
#else
typedef struct CJMSConnection CJMSConnection;
#endif /* __cplusplus */

#endif 	/* __CJMSConnection_FWD_DEFINED__ */


#ifndef __CJMSQueueConnection_FWD_DEFINED__
#define __CJMSQueueConnection_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSQueueConnection CJMSQueueConnection;
#else
typedef struct CJMSQueueConnection CJMSQueueConnection;
#endif /* __cplusplus */

#endif 	/* __CJMSQueueConnection_FWD_DEFINED__ */


#ifndef __CJMSTopicConnection_FWD_DEFINED__
#define __CJMSTopicConnection_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSTopicConnection CJMSTopicConnection;
#else
typedef struct CJMSTopicConnection CJMSTopicConnection;
#endif /* __cplusplus */

#endif 	/* __CJMSTopicConnection_FWD_DEFINED__ */


#ifndef __CJMSMessageConsumer_FWD_DEFINED__
#define __CJMSMessageConsumer_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSMessageConsumer CJMSMessageConsumer;
#else
typedef struct CJMSMessageConsumer CJMSMessageConsumer;
#endif /* __cplusplus */

#endif 	/* __CJMSMessageConsumer_FWD_DEFINED__ */


#ifndef __CJMSQueueReceiver_FWD_DEFINED__
#define __CJMSQueueReceiver_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSQueueReceiver CJMSQueueReceiver;
#else
typedef struct CJMSQueueReceiver CJMSQueueReceiver;
#endif /* __cplusplus */

#endif 	/* __CJMSQueueReceiver_FWD_DEFINED__ */


#ifndef __CJMSTopicSubscriber_FWD_DEFINED__
#define __CJMSTopicSubscriber_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSTopicSubscriber CJMSTopicSubscriber;
#else
typedef struct CJMSTopicSubscriber CJMSTopicSubscriber;
#endif /* __cplusplus */

#endif 	/* __CJMSTopicSubscriber_FWD_DEFINED__ */


#ifndef __CJMSMessageProducer_FWD_DEFINED__
#define __CJMSMessageProducer_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSMessageProducer CJMSMessageProducer;
#else
typedef struct CJMSMessageProducer CJMSMessageProducer;
#endif /* __cplusplus */

#endif 	/* __CJMSMessageProducer_FWD_DEFINED__ */


#ifndef __CJMSQueueSender_FWD_DEFINED__
#define __CJMSQueueSender_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSQueueSender CJMSQueueSender;
#else
typedef struct CJMSQueueSender CJMSQueueSender;
#endif /* __cplusplus */

#endif 	/* __CJMSQueueSender_FWD_DEFINED__ */


#ifndef __CJMSTopicPublisher_FWD_DEFINED__
#define __CJMSTopicPublisher_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSTopicPublisher CJMSTopicPublisher;
#else
typedef struct CJMSTopicPublisher CJMSTopicPublisher;
#endif /* __cplusplus */

#endif 	/* __CJMSTopicPublisher_FWD_DEFINED__ */


#ifndef __CJMSSession_FWD_DEFINED__
#define __CJMSSession_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSSession CJMSSession;
#else
typedef struct CJMSSession CJMSSession;
#endif /* __cplusplus */

#endif 	/* __CJMSSession_FWD_DEFINED__ */


#ifndef __CJMSQueueSession_FWD_DEFINED__
#define __CJMSQueueSession_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSQueueSession CJMSQueueSession;
#else
typedef struct CJMSQueueSession CJMSQueueSession;
#endif /* __cplusplus */

#endif 	/* __CJMSQueueSession_FWD_DEFINED__ */


#ifndef __CJMSTopicSession_FWD_DEFINED__
#define __CJMSTopicSession_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSTopicSession CJMSTopicSession;
#else
typedef struct CJMSTopicSession CJMSTopicSession;
#endif /* __cplusplus */

#endif 	/* __CJMSTopicSession_FWD_DEFINED__ */


#ifndef __CJMSQueueBrowser_FWD_DEFINED__
#define __CJMSQueueBrowser_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSQueueBrowser CJMSQueueBrowser;
#else
typedef struct CJMSQueueBrowser CJMSQueueBrowser;
#endif /* __cplusplus */

#endif 	/* __CJMSQueueBrowser_FWD_DEFINED__ */


#ifndef __CJMSEnumeration_FWD_DEFINED__
#define __CJMSEnumeration_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSEnumeration CJMSEnumeration;
#else
typedef struct CJMSEnumeration CJMSEnumeration;
#endif /* __cplusplus */

#endif 	/* __CJMSEnumeration_FWD_DEFINED__ */


#ifndef __CJMSConnectionMetaData_FWD_DEFINED__
#define __CJMSConnectionMetaData_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSConnectionMetaData CJMSConnectionMetaData;
#else
typedef struct CJMSConnectionMetaData CJMSConnectionMetaData;
#endif /* __cplusplus */

#endif 	/* __CJMSConnectionMetaData_FWD_DEFINED__ */


#ifndef __CJMSCOMException_FWD_DEFINED__
#define __CJMSCOMException_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSCOMException CJMSCOMException;
#else
typedef struct CJMSCOMException CJMSCOMException;
#endif /* __cplusplus */

#endif 	/* __CJMSCOMException_FWD_DEFINED__ */


#ifndef __CJMSDurableSubscriber_FWD_DEFINED__
#define __CJMSDurableSubscriber_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSDurableSubscriber CJMSDurableSubscriber;
#else
typedef struct CJMSDurableSubscriber CJMSDurableSubscriber;
#endif /* __cplusplus */

#endif 	/* __CJMSDurableSubscriber_FWD_DEFINED__ */


#ifndef __CJMSMapMessage_FWD_DEFINED__
#define __CJMSMapMessage_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSMapMessage CJMSMapMessage;
#else
typedef struct CJMSMapMessage CJMSMapMessage;
#endif /* __cplusplus */

#endif 	/* __CJMSMapMessage_FWD_DEFINED__ */


#ifndef __CJMSThrowable_FWD_DEFINED__
#define __CJMSThrowable_FWD_DEFINED__

#ifdef __cplusplus
typedef class CJMSThrowable CJMSThrowable;
#else
typedef struct CJMSThrowable CJMSThrowable;
#endif /* __cplusplus */

#endif 	/* __CJMSThrowable_FWD_DEFINED__ */


/* header files for imported files */
#include "oaidl.h"
#include "ocidl.h"

#ifdef __cplusplus
extern "C"{
#endif 

void * __RPC_USER MIDL_user_allocate(size_t);
void __RPC_USER MIDL_user_free( void * ); 

/* interface __MIDL_itf_JMSCOMClient_0000 */
/* [local] */ 



















enum JMSDeliveryMode
    {	JMSCOM_DeliveryMode_NON_PERSISTENT	= 1,
	JMSCOM_DeliveryMode_PERSISTENT	= 2,
	JMSCOM_DeliveryMode_NON_PERSISTENT_ASYNC	= 102,
	JMSCOM_DeliveryMode_NON_PERSISTENT_SYNC	= 103,
	JMSCOM_DeliveryMode_DISCARDABLE	= 104
    } ;

enum JMSMessageAcknowledgeMode
    {	JMSCOM_Session_AUTO_ACKNOWLEDGE	= 1,
	JMSCOM_Session_CLIENT_ACKNOWLEDGE	= 2,
	JMSCOM_Session_DUPS_OK_ACKNOWLEDGE	= 3,
	JMSCOM_Session_SINGLE_MESSAGE_ACKNOWLEDGE	= 1004
    } ;

enum JMSExceptionCode
    {	JMSCOM_ErrorCodes_ERROR	= -1,
	JMSCOM_ErrorCodes_ERR_PRIVACY_FAILED	= -2,
	JMSCOM_ErrorCodes_ERR_INTEGRITY_FAILED	= -3,
	JMSCOM_ErrorCodes_ERR_NONREPUDIATION_FAILED	= -4,
	JMSCOM_ErrorCodes_ERR_CONNECTION_DROPPED	= -5,
	JMSCOM_ErrorCodes_ERR_PUBLISH_NOT_AUTHORIZED	= -6,
	JMSCOM_ErrorCodes_ERR_SUBSCRIBE_NOT_AUTHORIZED	= -7,
	JMSCOM_ErrorCodes_ERR_GUARANTEE_NOT_AUTHORIZED	= -8,
	JMSCOM_ErrorCodes_ERR_WRONG_SUBJECT_ADDR	= -9,
	JMSCOM_ErrorCodes_ERR_GENERAL_SECURITY_ERR	= -10,
	JMSCOM_ErrorCodes_ERR_TXN_NOT_FOUND	= -11,
	JMSCOM_ErrorCodes_ERR_TXN_ACCESS_VIOLATION	= -12,
	JMSCOM_ErrorCodes_ERR_TXN_SEQUENCE_ERR	= -13,
	JMSCOM_ErrorCodes_ERR_REQUEST_NOSUB_FOR_SUBJECT	= -14,
	JMSCOM_ErrorCodes_ERR_MESSAGELISTENER_RUNTIME_EXCEPTION	= -15,
	JMSCOM_ErrorCodes_ERR_FLOW_CONTROL_EXCEPTION	= -16,
	JMSCOM_ErrorCodes_ERR_TOO_LARGE_FOR_QUEUE	= -17
    } ;
#define	JMSCOM_Message_TYPE_UNDEFINED	( 0 )

#define	JMSCOM_Message_TYPE_QUEUE	( 1 )

#define	JMSCOM_Message_TYPE_TOPIC	( 2 )

#define	JMSCOM_Message_NATIVE_MESSAGE_TYPE	( 0 )

#define	JMSCOM_Message_BASE_MESSAGE_TYPE	( 1 )

#define	JMSCOM_Message_BYTES_MESSAGE_TYPE	( 2 )

#define	JMSCOM_Message_MAP_MESSAGE_TYPE	( 3 )

#define	JMSCOM_Message_OBJECT_MESSAGE_TYPE	( 4 )

#define	JMSCOM_Message_STREAM_MESSAGE_TYPE	( 5 )

#define	JMSCOM_Message_TEXT_MESSAGE_TYPE	( 6 )

#define	JMSCOM_Message_XML_MESSAGE_TYPE	( 7 )

#define	JMSCOM_Message_DEFAULT_DELIVERY_MODE	( 2 )

#define	JMSCOM_Message_DEFAULT_PRIORITY	( 4 )

#define	JMSCOM_Message_DEFAULT_TIME_TO_LIVE	( 0 )

#define	JMSCOM_ConnectionState_ACTIVE	( 0 )

#define	JMSCOM_ConnectionState_RECONNECTING	( 1 )

#define	JMSCOM_ConnectionState_FAILED	( 2 )

#define	JMSCOM_ConnectionState_CLOSED	( 3 )



extern RPC_IF_HANDLE __MIDL_itf_JMSCOMClient_0000_v0_0_c_ifspec;
extern RPC_IF_HANDLE __MIDL_itf_JMSCOMClient_0000_v0_0_s_ifspec;

#ifndef __IJMSObject_INTERFACE_DEFINED__
#define __IJMSObject_INTERFACE_DEFINED__

/* interface IJMSObject */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSObject;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("7C48E53B-A00D-4927-AD74-51FB75A3148E")
    IJMSObject : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE equals( 
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE clone( 
            /* [retval][out] */ IJMSObject **clonedObj) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE toString( 
            /* [retval][out] */ BSTR *objDesc) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSObjectVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSObject * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSObject * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSObject * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSObject * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSObject * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSObject * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSObject * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSObject * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSObject * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSObject * This,
            /* [retval][out] */ BSTR *objDesc);
        
        END_INTERFACE
    } IJMSObjectVtbl;

    interface IJMSObject
    {
        CONST_VTBL struct IJMSObjectVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSObject_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSObject_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSObject_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSObject_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSObject_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSObject_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSObject_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSObject_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSObject_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSObject_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSObject_equals_Proxy( 
    IJMSObject * This,
    /* [in] */ IJMSObject *obj,
    /* [retval][out] */ BOOL *isEqual);


void __RPC_STUB IJMSObject_equals_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSObject_clone_Proxy( 
    IJMSObject * This,
    /* [retval][out] */ IJMSObject **clonedObj);


void __RPC_STUB IJMSObject_clone_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSObject_toString_Proxy( 
    IJMSObject * This,
    /* [retval][out] */ BSTR *objDesc);


void __RPC_STUB IJMSObject_toString_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSObject_INTERFACE_DEFINED__ */


#ifndef __IJMSEnumeration_INTERFACE_DEFINED__
#define __IJMSEnumeration_INTERFACE_DEFINED__

/* interface IJMSEnumeration */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSEnumeration;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("34BB73EB-2357-41B7-B12E-F08C1C044A9B")
    IJMSEnumeration : public IJMSObject
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE hasMoreElements( 
            /* [retval][out] */ BOOL *pMore) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE nextElement( 
            /* [retval][out] */ VARIANT *pNextElement) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE containsObjects( 
            /* [retval][out] */ BOOL *pHasObjects) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE nextObject( 
            /* [retval][out] */ IJMSObject **pNextObject) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSEnumerationVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSEnumeration * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSEnumeration * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSEnumeration * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSEnumeration * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSEnumeration * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSEnumeration * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSEnumeration * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSEnumeration * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSEnumeration * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSEnumeration * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *hasMoreElements )( 
            IJMSEnumeration * This,
            /* [retval][out] */ BOOL *pMore);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *nextElement )( 
            IJMSEnumeration * This,
            /* [retval][out] */ VARIANT *pNextElement);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *containsObjects )( 
            IJMSEnumeration * This,
            /* [retval][out] */ BOOL *pHasObjects);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *nextObject )( 
            IJMSEnumeration * This,
            /* [retval][out] */ IJMSObject **pNextObject);
        
        END_INTERFACE
    } IJMSEnumerationVtbl;

    interface IJMSEnumeration
    {
        CONST_VTBL struct IJMSEnumerationVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSEnumeration_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSEnumeration_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSEnumeration_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSEnumeration_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSEnumeration_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSEnumeration_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSEnumeration_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSEnumeration_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSEnumeration_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSEnumeration_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSEnumeration_hasMoreElements(This,pMore)	\
    (This)->lpVtbl -> hasMoreElements(This,pMore)

#define IJMSEnumeration_nextElement(This,pNextElement)	\
    (This)->lpVtbl -> nextElement(This,pNextElement)

#define IJMSEnumeration_containsObjects(This,pHasObjects)	\
    (This)->lpVtbl -> containsObjects(This,pHasObjects)

#define IJMSEnumeration_nextObject(This,pNextObject)	\
    (This)->lpVtbl -> nextObject(This,pNextObject)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSEnumeration_hasMoreElements_Proxy( 
    IJMSEnumeration * This,
    /* [retval][out] */ BOOL *pMore);


void __RPC_STUB IJMSEnumeration_hasMoreElements_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSEnumeration_nextElement_Proxy( 
    IJMSEnumeration * This,
    /* [retval][out] */ VARIANT *pNextElement);


void __RPC_STUB IJMSEnumeration_nextElement_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSEnumeration_containsObjects_Proxy( 
    IJMSEnumeration * This,
    /* [retval][out] */ BOOL *pHasObjects);


void __RPC_STUB IJMSEnumeration_containsObjects_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSEnumeration_nextObject_Proxy( 
    IJMSEnumeration * This,
    /* [retval][out] */ IJMSObject **pNextObject);


void __RPC_STUB IJMSEnumeration_nextObject_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSEnumeration_INTERFACE_DEFINED__ */


#ifndef __IJMSThrowable_INTERFACE_DEFINED__
#define __IJMSThrowable_INTERFACE_DEFINED__

/* interface IJMSThrowable */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSThrowable;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("AF0C872A-1D33-447C-9F27-A4632BC6A5D6")
    IJMSThrowable : public IJMSObject
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getMessage( 
            /* [retval][out] */ BSTR *exceptionText) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getLocalizedMessage( 
            /* [retval][out] */ BSTR *exceptionText) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSThrowableVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSThrowable * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSThrowable * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSThrowable * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSThrowable * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSThrowable * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSThrowable * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSThrowable * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSThrowable * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSThrowable * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSThrowable * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getMessage )( 
            IJMSThrowable * This,
            /* [retval][out] */ BSTR *exceptionText);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getLocalizedMessage )( 
            IJMSThrowable * This,
            /* [retval][out] */ BSTR *exceptionText);
        
        END_INTERFACE
    } IJMSThrowableVtbl;

    interface IJMSThrowable
    {
        CONST_VTBL struct IJMSThrowableVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSThrowable_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSThrowable_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSThrowable_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSThrowable_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSThrowable_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSThrowable_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSThrowable_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSThrowable_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSThrowable_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSThrowable_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSThrowable_getMessage(This,exceptionText)	\
    (This)->lpVtbl -> getMessage(This,exceptionText)

#define IJMSThrowable_getLocalizedMessage(This,exceptionText)	\
    (This)->lpVtbl -> getLocalizedMessage(This,exceptionText)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSThrowable_getMessage_Proxy( 
    IJMSThrowable * This,
    /* [retval][out] */ BSTR *exceptionText);


void __RPC_STUB IJMSThrowable_getMessage_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSThrowable_getLocalizedMessage_Proxy( 
    IJMSThrowable * This,
    /* [retval][out] */ BSTR *exceptionText);


void __RPC_STUB IJMSThrowable_getLocalizedMessage_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSThrowable_INTERFACE_DEFINED__ */


#ifndef __IJMSCOMException_INTERFACE_DEFINED__
#define __IJMSCOMException_INTERFACE_DEFINED__

/* interface IJMSCOMException */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSCOMException;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("FE8C0E7F-6E13-44BC-86B4-CD0795597EB8")
    IJMSCOMException : public IJMSThrowable
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getErrorCode( 
            /* [retval][out] */ BSTR *pErrorCode) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getLinkedException( 
            /* [retval][out] */ IJMSThrowable **ppErrorCode) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setLinkedException( 
            /* [in] */ IJMSThrowable *pErrorCode) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE testException( 
            /* [in] */ enum JMSExceptionCode errCode,
            /* [retval][out] */ BOOL *pIsException) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSCOMExceptionVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSCOMException * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSCOMException * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSCOMException * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSCOMException * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSCOMException * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSCOMException * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSCOMException * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSCOMException * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSCOMException * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSCOMException * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getMessage )( 
            IJMSCOMException * This,
            /* [retval][out] */ BSTR *exceptionText);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getLocalizedMessage )( 
            IJMSCOMException * This,
            /* [retval][out] */ BSTR *exceptionText);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getErrorCode )( 
            IJMSCOMException * This,
            /* [retval][out] */ BSTR *pErrorCode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getLinkedException )( 
            IJMSCOMException * This,
            /* [retval][out] */ IJMSThrowable **ppErrorCode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setLinkedException )( 
            IJMSCOMException * This,
            /* [in] */ IJMSThrowable *pErrorCode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *testException )( 
            IJMSCOMException * This,
            /* [in] */ enum JMSExceptionCode errCode,
            /* [retval][out] */ BOOL *pIsException);
        
        END_INTERFACE
    } IJMSCOMExceptionVtbl;

    interface IJMSCOMException
    {
        CONST_VTBL struct IJMSCOMExceptionVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSCOMException_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSCOMException_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSCOMException_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSCOMException_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSCOMException_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSCOMException_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSCOMException_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSCOMException_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSCOMException_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSCOMException_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSCOMException_getMessage(This,exceptionText)	\
    (This)->lpVtbl -> getMessage(This,exceptionText)

#define IJMSCOMException_getLocalizedMessage(This,exceptionText)	\
    (This)->lpVtbl -> getLocalizedMessage(This,exceptionText)


#define IJMSCOMException_getErrorCode(This,pErrorCode)	\
    (This)->lpVtbl -> getErrorCode(This,pErrorCode)

#define IJMSCOMException_getLinkedException(This,ppErrorCode)	\
    (This)->lpVtbl -> getLinkedException(This,ppErrorCode)

#define IJMSCOMException_setLinkedException(This,pErrorCode)	\
    (This)->lpVtbl -> setLinkedException(This,pErrorCode)

#define IJMSCOMException_testException(This,errCode,pIsException)	\
    (This)->lpVtbl -> testException(This,errCode,pIsException)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSCOMException_getErrorCode_Proxy( 
    IJMSCOMException * This,
    /* [retval][out] */ BSTR *pErrorCode);


void __RPC_STUB IJMSCOMException_getErrorCode_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSCOMException_getLinkedException_Proxy( 
    IJMSCOMException * This,
    /* [retval][out] */ IJMSThrowable **ppErrorCode);


void __RPC_STUB IJMSCOMException_getLinkedException_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSCOMException_setLinkedException_Proxy( 
    IJMSCOMException * This,
    /* [in] */ IJMSThrowable *pErrorCode);


void __RPC_STUB IJMSCOMException_setLinkedException_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSCOMException_testException_Proxy( 
    IJMSCOMException * This,
    /* [in] */ enum JMSExceptionCode errCode,
    /* [retval][out] */ BOOL *pIsException);


void __RPC_STUB IJMSCOMException_testException_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSCOMException_INTERFACE_DEFINED__ */


#ifndef __IJMSConnectionMetaData_INTERFACE_DEFINED__
#define __IJMSConnectionMetaData_INTERFACE_DEFINED__

/* interface IJMSConnectionMetaData */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSConnectionMetaData;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("410462D0-3304-46FC-8FCE-4B93CAD3DAAD")
    IJMSConnectionMetaData : public IJMSObject
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSMajorVersion( 
            /* [retval][out] */ int *pVersion) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSMinorVersion( 
            /* [retval][out] */ int *pVersion) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSVersion( 
            /* [retval][out] */ BSTR *pVersion) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSProviderName( 
            /* [retval][out] */ BSTR *pProviderName) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getProviderMajorVersion( 
            /* [retval][out] */ int *pVersion) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getProviderMinorVersion( 
            /* [retval][out] */ int *pVersion) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getProviderVersion( 
            /* [retval][out] */ BSTR *pProvider) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSXPropertyNames( 
            /* [retval][out] */ IJMSEnumeration **pPropertyNames) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSConnectionMetaDataVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSConnectionMetaData * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSConnectionMetaData * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSConnectionMetaData * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSConnectionMetaData * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSConnectionMetaData * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSConnectionMetaData * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSConnectionMetaData * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSConnectionMetaData * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSConnectionMetaData * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSConnectionMetaData * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSMajorVersion )( 
            IJMSConnectionMetaData * This,
            /* [retval][out] */ int *pVersion);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSMinorVersion )( 
            IJMSConnectionMetaData * This,
            /* [retval][out] */ int *pVersion);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSVersion )( 
            IJMSConnectionMetaData * This,
            /* [retval][out] */ BSTR *pVersion);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSProviderName )( 
            IJMSConnectionMetaData * This,
            /* [retval][out] */ BSTR *pProviderName);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getProviderMajorVersion )( 
            IJMSConnectionMetaData * This,
            /* [retval][out] */ int *pVersion);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getProviderMinorVersion )( 
            IJMSConnectionMetaData * This,
            /* [retval][out] */ int *pVersion);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getProviderVersion )( 
            IJMSConnectionMetaData * This,
            /* [retval][out] */ BSTR *pProvider);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSXPropertyNames )( 
            IJMSConnectionMetaData * This,
            /* [retval][out] */ IJMSEnumeration **pPropertyNames);
        
        END_INTERFACE
    } IJMSConnectionMetaDataVtbl;

    interface IJMSConnectionMetaData
    {
        CONST_VTBL struct IJMSConnectionMetaDataVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSConnectionMetaData_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSConnectionMetaData_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSConnectionMetaData_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSConnectionMetaData_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSConnectionMetaData_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSConnectionMetaData_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSConnectionMetaData_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSConnectionMetaData_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSConnectionMetaData_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSConnectionMetaData_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSConnectionMetaData_getJMSMajorVersion(This,pVersion)	\
    (This)->lpVtbl -> getJMSMajorVersion(This,pVersion)

#define IJMSConnectionMetaData_getJMSMinorVersion(This,pVersion)	\
    (This)->lpVtbl -> getJMSMinorVersion(This,pVersion)

#define IJMSConnectionMetaData_getJMSVersion(This,pVersion)	\
    (This)->lpVtbl -> getJMSVersion(This,pVersion)

#define IJMSConnectionMetaData_getJMSProviderName(This,pProviderName)	\
    (This)->lpVtbl -> getJMSProviderName(This,pProviderName)

#define IJMSConnectionMetaData_getProviderMajorVersion(This,pVersion)	\
    (This)->lpVtbl -> getProviderMajorVersion(This,pVersion)

#define IJMSConnectionMetaData_getProviderMinorVersion(This,pVersion)	\
    (This)->lpVtbl -> getProviderMinorVersion(This,pVersion)

#define IJMSConnectionMetaData_getProviderVersion(This,pProvider)	\
    (This)->lpVtbl -> getProviderVersion(This,pProvider)

#define IJMSConnectionMetaData_getJMSXPropertyNames(This,pPropertyNames)	\
    (This)->lpVtbl -> getJMSXPropertyNames(This,pPropertyNames)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnectionMetaData_getJMSMajorVersion_Proxy( 
    IJMSConnectionMetaData * This,
    /* [retval][out] */ int *pVersion);


void __RPC_STUB IJMSConnectionMetaData_getJMSMajorVersion_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnectionMetaData_getJMSMinorVersion_Proxy( 
    IJMSConnectionMetaData * This,
    /* [retval][out] */ int *pVersion);


void __RPC_STUB IJMSConnectionMetaData_getJMSMinorVersion_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnectionMetaData_getJMSVersion_Proxy( 
    IJMSConnectionMetaData * This,
    /* [retval][out] */ BSTR *pVersion);


void __RPC_STUB IJMSConnectionMetaData_getJMSVersion_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnectionMetaData_getJMSProviderName_Proxy( 
    IJMSConnectionMetaData * This,
    /* [retval][out] */ BSTR *pProviderName);


void __RPC_STUB IJMSConnectionMetaData_getJMSProviderName_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnectionMetaData_getProviderMajorVersion_Proxy( 
    IJMSConnectionMetaData * This,
    /* [retval][out] */ int *pVersion);


void __RPC_STUB IJMSConnectionMetaData_getProviderMajorVersion_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnectionMetaData_getProviderMinorVersion_Proxy( 
    IJMSConnectionMetaData * This,
    /* [retval][out] */ int *pVersion);


void __RPC_STUB IJMSConnectionMetaData_getProviderMinorVersion_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnectionMetaData_getProviderVersion_Proxy( 
    IJMSConnectionMetaData * This,
    /* [retval][out] */ BSTR *pProvider);


void __RPC_STUB IJMSConnectionMetaData_getProviderVersion_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnectionMetaData_getJMSXPropertyNames_Proxy( 
    IJMSConnectionMetaData * This,
    /* [retval][out] */ IJMSEnumeration **pPropertyNames);


void __RPC_STUB IJMSConnectionMetaData_getJMSXPropertyNames_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSConnectionMetaData_INTERFACE_DEFINED__ */


#ifndef __IJMSQueueConnectionFactory_INTERFACE_DEFINED__
#define __IJMSQueueConnectionFactory_INTERFACE_DEFINED__

/* interface IJMSQueueConnectionFactory */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSQueueConnectionFactory;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("0273E6F2-C86D-4203-B5D7-A40663F9CB1C")
    IJMSQueueConnectionFactory : public IJMSObject
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE initialize1( 
            /* [in] */ BSTR brokerURL) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE initialize2( 
            /* [in] */ BSTR brokerHostName,
            /* [in] */ int brokerPort,
            /* [in] */ BSTR brokerProtocol,
            /* [in] */ BSTR defaultUserName,
            /* [in] */ BSTR defaultPassword) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE initialize3( 
            /* [in] */ BSTR brokerHostName,
            /* [in] */ int brokerPort,
            /* [in] */ BSTR brokerProtocol,
            /* [in] */ BSTR connectID,
            /* [in] */ BSTR defaultPassword) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE initialize4( 
            /* [in] */ BSTR brokerURL,
            /* [in] */ BSTR connectID) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE initialize5( 
            /* [in] */ BSTR brokerURL,
            /* [in] */ BSTR defaultUserName,
            /* [in] */ BSTR defaultPassword) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE initialize6( 
            /* [in] */ BSTR brokerURL,
            /* [in] */ BSTR connectID,
            /* [in] */ BSTR defaultUserName,
            /* [in] */ BSTR defaultPassword) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createQueueConnection( 
            /* [retval][out] */ IJMSQueueConnection **ppConn) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createQueueConnection2( 
            /* [in] */ BSTR username,
            /* [in] */ BSTR password,
            /* [retval][out] */ IJMSQueueConnection **ppConn) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getBrokerHostName( 
            /* [retval][out] */ BSTR *brokerHostName) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getBrokerPort( 
            /* [retval][out] */ int *brokerPort) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getBrokerProtocol( 
            /* [retval][out] */ BSTR *brokerProtocol) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getBrokerURL( 
            /* [retval][out] */ BSTR *brokerURL) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getClientID( 
            /* [retval][out] */ BSTR *clientID) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getConnectID( 
            /* [retval][out] */ BSTR *connectID) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getConnectionURLs( 
            /* [retval][out] */ BSTR *connectionURLs) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getDefaultPassword( 
            /* [retval][out] */ BSTR *defaultPassword) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getDefaultUser( 
            /* [retval][out] */ BSTR *defaultUser) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getLoadBalancing( 
            /* [retval][out] */ BOOL *loadBalancing) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getSequential( 
            /* [retval][out] */ BOOL *sequential) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getTcpNoDelay( 
            /* [retval][out] */ BOOL *tcpNoDelay) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setBrokerHostName( 
            /* [in] */ BSTR brokerHostName) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setBrokerPort( 
            /* [in] */ int brokerPort) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setBrokerProtocol( 
            /* [in] */ BSTR brokerProtocol) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setBrokerURL( 
            /* [in] */ BSTR brokerURL) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setClientID( 
            /* [in] */ BSTR clientID) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setConnectID( 
            /* [in] */ BSTR connectID) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setConnectionURLs( 
            /* [in] */ BSTR connectionURLs) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setLoadBalancing( 
            /* [in] */ BOOL loadBalancing) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setSequential( 
            /* [in] */ BOOL sequential) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setTcpNoDelay( 
            /* [in] */ BOOL tcpNoDelay) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setMonitorInterval( 
            /* [in] */ long interval) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getMonitorInterval( 
            /* [retval][out] */ long *interval) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setDefaultTxnBatchSize( 
            /* [in] */ long txnBatchSize) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getDefaultTxnBatchSize( 
            /* [retval][out] */ long *txnBatchSize) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setTCPConnectionTimeout( 
            /* [in] */ long tcpTimeout) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getTCPConnectionTimeout( 
            /* [retval][out] */ long *tcpTimeout) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setFaultTolerant( 
            /* [in] */ BOOL faultTolerant) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getFaultTolerant( 
            /* [retval][out] */ BOOL *faultTolerant) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setFaultTolerantReconnectTimeout( 
            /* [in] */ long faultTolerantReconnectTimeout) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getFaultTolerantReconnectTimeout( 
            /* [retval][out] */ long *faultTolerantReconnectTimeout) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setInitialConnectTimeout( 
            /* [in] */ long initialConnectTimeout) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getInitialConnectTimeout( 
            /* [retval][out] */ long *initialConnectTimeout) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setClientTransactionBufferSize( 
            /* [in] */ long size) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getClientTransactionBufferSize( 
            /* [retval][out] */ long *size) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getMinimizeSubscriberTraffic( 
            /* [retval][out] */ BOOL *minimizeTraffic) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setMinimizeSubscriberTraffic( 
            /* [in] */ BOOL minimizeTraffic) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSQueueConnectionFactoryVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSQueueConnectionFactory * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSQueueConnectionFactory * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSQueueConnectionFactory * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *initialize1 )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ BSTR brokerURL);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *initialize2 )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ BSTR brokerHostName,
            /* [in] */ int brokerPort,
            /* [in] */ BSTR brokerProtocol,
            /* [in] */ BSTR defaultUserName,
            /* [in] */ BSTR defaultPassword);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *initialize3 )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ BSTR brokerHostName,
            /* [in] */ int brokerPort,
            /* [in] */ BSTR brokerProtocol,
            /* [in] */ BSTR connectID,
            /* [in] */ BSTR defaultPassword);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *initialize4 )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ BSTR brokerURL,
            /* [in] */ BSTR connectID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *initialize5 )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ BSTR brokerURL,
            /* [in] */ BSTR defaultUserName,
            /* [in] */ BSTR defaultPassword);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *initialize6 )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ BSTR brokerURL,
            /* [in] */ BSTR connectID,
            /* [in] */ BSTR defaultUserName,
            /* [in] */ BSTR defaultPassword);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createQueueConnection )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ IJMSQueueConnection **ppConn);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createQueueConnection2 )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ BSTR username,
            /* [in] */ BSTR password,
            /* [retval][out] */ IJMSQueueConnection **ppConn);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBrokerHostName )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ BSTR *brokerHostName);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBrokerPort )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ int *brokerPort);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBrokerProtocol )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ BSTR *brokerProtocol);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBrokerURL )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ BSTR *brokerURL);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getClientID )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ BSTR *clientID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getConnectID )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ BSTR *connectID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getConnectionURLs )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ BSTR *connectionURLs);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDefaultPassword )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ BSTR *defaultPassword);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDefaultUser )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ BSTR *defaultUser);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getLoadBalancing )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ BOOL *loadBalancing);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getSequential )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ BOOL *sequential);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getTcpNoDelay )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ BOOL *tcpNoDelay);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setBrokerHostName )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ BSTR brokerHostName);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setBrokerPort )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ int brokerPort);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setBrokerProtocol )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ BSTR brokerProtocol);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setBrokerURL )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ BSTR brokerURL);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setClientID )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ BSTR clientID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setConnectID )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ BSTR connectID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setConnectionURLs )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ BSTR connectionURLs);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setLoadBalancing )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ BOOL loadBalancing);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setSequential )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ BOOL sequential);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setTcpNoDelay )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ BOOL tcpNoDelay);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setMonitorInterval )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ long interval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getMonitorInterval )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ long *interval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setDefaultTxnBatchSize )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ long txnBatchSize);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDefaultTxnBatchSize )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ long *txnBatchSize);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setTCPConnectionTimeout )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ long tcpTimeout);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getTCPConnectionTimeout )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ long *tcpTimeout);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setFaultTolerant )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ BOOL faultTolerant);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getFaultTolerant )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ BOOL *faultTolerant);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setFaultTolerantReconnectTimeout )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ long faultTolerantReconnectTimeout);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getFaultTolerantReconnectTimeout )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ long *faultTolerantReconnectTimeout);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setInitialConnectTimeout )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ long initialConnectTimeout);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getInitialConnectTimeout )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ long *initialConnectTimeout);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setClientTransactionBufferSize )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ long size);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getClientTransactionBufferSize )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ long *size);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getMinimizeSubscriberTraffic )( 
            IJMSQueueConnectionFactory * This,
            /* [retval][out] */ BOOL *minimizeTraffic);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setMinimizeSubscriberTraffic )( 
            IJMSQueueConnectionFactory * This,
            /* [in] */ BOOL minimizeTraffic);
        
        END_INTERFACE
    } IJMSQueueConnectionFactoryVtbl;

    interface IJMSQueueConnectionFactory
    {
        CONST_VTBL struct IJMSQueueConnectionFactoryVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSQueueConnectionFactory_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSQueueConnectionFactory_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSQueueConnectionFactory_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSQueueConnectionFactory_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSQueueConnectionFactory_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSQueueConnectionFactory_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSQueueConnectionFactory_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSQueueConnectionFactory_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSQueueConnectionFactory_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSQueueConnectionFactory_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSQueueConnectionFactory_initialize1(This,brokerURL)	\
    (This)->lpVtbl -> initialize1(This,brokerURL)

#define IJMSQueueConnectionFactory_initialize2(This,brokerHostName,brokerPort,brokerProtocol,defaultUserName,defaultPassword)	\
    (This)->lpVtbl -> initialize2(This,brokerHostName,brokerPort,brokerProtocol,defaultUserName,defaultPassword)

#define IJMSQueueConnectionFactory_initialize3(This,brokerHostName,brokerPort,brokerProtocol,connectID,defaultPassword)	\
    (This)->lpVtbl -> initialize3(This,brokerHostName,brokerPort,brokerProtocol,connectID,defaultPassword)

#define IJMSQueueConnectionFactory_initialize4(This,brokerURL,connectID)	\
    (This)->lpVtbl -> initialize4(This,brokerURL,connectID)

#define IJMSQueueConnectionFactory_initialize5(This,brokerURL,defaultUserName,defaultPassword)	\
    (This)->lpVtbl -> initialize5(This,brokerURL,defaultUserName,defaultPassword)

#define IJMSQueueConnectionFactory_initialize6(This,brokerURL,connectID,defaultUserName,defaultPassword)	\
    (This)->lpVtbl -> initialize6(This,brokerURL,connectID,defaultUserName,defaultPassword)

#define IJMSQueueConnectionFactory_createQueueConnection(This,ppConn)	\
    (This)->lpVtbl -> createQueueConnection(This,ppConn)

#define IJMSQueueConnectionFactory_createQueueConnection2(This,username,password,ppConn)	\
    (This)->lpVtbl -> createQueueConnection2(This,username,password,ppConn)

#define IJMSQueueConnectionFactory_getBrokerHostName(This,brokerHostName)	\
    (This)->lpVtbl -> getBrokerHostName(This,brokerHostName)

#define IJMSQueueConnectionFactory_getBrokerPort(This,brokerPort)	\
    (This)->lpVtbl -> getBrokerPort(This,brokerPort)

#define IJMSQueueConnectionFactory_getBrokerProtocol(This,brokerProtocol)	\
    (This)->lpVtbl -> getBrokerProtocol(This,brokerProtocol)

#define IJMSQueueConnectionFactory_getBrokerURL(This,brokerURL)	\
    (This)->lpVtbl -> getBrokerURL(This,brokerURL)

#define IJMSQueueConnectionFactory_getClientID(This,clientID)	\
    (This)->lpVtbl -> getClientID(This,clientID)

#define IJMSQueueConnectionFactory_getConnectID(This,connectID)	\
    (This)->lpVtbl -> getConnectID(This,connectID)

#define IJMSQueueConnectionFactory_getConnectionURLs(This,connectionURLs)	\
    (This)->lpVtbl -> getConnectionURLs(This,connectionURLs)

#define IJMSQueueConnectionFactory_getDefaultPassword(This,defaultPassword)	\
    (This)->lpVtbl -> getDefaultPassword(This,defaultPassword)

#define IJMSQueueConnectionFactory_getDefaultUser(This,defaultUser)	\
    (This)->lpVtbl -> getDefaultUser(This,defaultUser)

#define IJMSQueueConnectionFactory_getLoadBalancing(This,loadBalancing)	\
    (This)->lpVtbl -> getLoadBalancing(This,loadBalancing)

#define IJMSQueueConnectionFactory_getSequential(This,sequential)	\
    (This)->lpVtbl -> getSequential(This,sequential)

#define IJMSQueueConnectionFactory_getTcpNoDelay(This,tcpNoDelay)	\
    (This)->lpVtbl -> getTcpNoDelay(This,tcpNoDelay)

#define IJMSQueueConnectionFactory_setBrokerHostName(This,brokerHostName)	\
    (This)->lpVtbl -> setBrokerHostName(This,brokerHostName)

#define IJMSQueueConnectionFactory_setBrokerPort(This,brokerPort)	\
    (This)->lpVtbl -> setBrokerPort(This,brokerPort)

#define IJMSQueueConnectionFactory_setBrokerProtocol(This,brokerProtocol)	\
    (This)->lpVtbl -> setBrokerProtocol(This,brokerProtocol)

#define IJMSQueueConnectionFactory_setBrokerURL(This,brokerURL)	\
    (This)->lpVtbl -> setBrokerURL(This,brokerURL)

#define IJMSQueueConnectionFactory_setClientID(This,clientID)	\
    (This)->lpVtbl -> setClientID(This,clientID)

#define IJMSQueueConnectionFactory_setConnectID(This,connectID)	\
    (This)->lpVtbl -> setConnectID(This,connectID)

#define IJMSQueueConnectionFactory_setConnectionURLs(This,connectionURLs)	\
    (This)->lpVtbl -> setConnectionURLs(This,connectionURLs)

#define IJMSQueueConnectionFactory_setLoadBalancing(This,loadBalancing)	\
    (This)->lpVtbl -> setLoadBalancing(This,loadBalancing)

#define IJMSQueueConnectionFactory_setSequential(This,sequential)	\
    (This)->lpVtbl -> setSequential(This,sequential)

#define IJMSQueueConnectionFactory_setTcpNoDelay(This,tcpNoDelay)	\
    (This)->lpVtbl -> setTcpNoDelay(This,tcpNoDelay)

#define IJMSQueueConnectionFactory_setMonitorInterval(This,interval)	\
    (This)->lpVtbl -> setMonitorInterval(This,interval)

#define IJMSQueueConnectionFactory_getMonitorInterval(This,interval)	\
    (This)->lpVtbl -> getMonitorInterval(This,interval)

#define IJMSQueueConnectionFactory_setDefaultTxnBatchSize(This,txnBatchSize)	\
    (This)->lpVtbl -> setDefaultTxnBatchSize(This,txnBatchSize)

#define IJMSQueueConnectionFactory_getDefaultTxnBatchSize(This,txnBatchSize)	\
    (This)->lpVtbl -> getDefaultTxnBatchSize(This,txnBatchSize)

#define IJMSQueueConnectionFactory_setTCPConnectionTimeout(This,tcpTimeout)	\
    (This)->lpVtbl -> setTCPConnectionTimeout(This,tcpTimeout)

#define IJMSQueueConnectionFactory_getTCPConnectionTimeout(This,tcpTimeout)	\
    (This)->lpVtbl -> getTCPConnectionTimeout(This,tcpTimeout)

#define IJMSQueueConnectionFactory_setFaultTolerant(This,faultTolerant)	\
    (This)->lpVtbl -> setFaultTolerant(This,faultTolerant)

#define IJMSQueueConnectionFactory_getFaultTolerant(This,faultTolerant)	\
    (This)->lpVtbl -> getFaultTolerant(This,faultTolerant)

#define IJMSQueueConnectionFactory_setFaultTolerantReconnectTimeout(This,faultTolerantReconnectTimeout)	\
    (This)->lpVtbl -> setFaultTolerantReconnectTimeout(This,faultTolerantReconnectTimeout)

#define IJMSQueueConnectionFactory_getFaultTolerantReconnectTimeout(This,faultTolerantReconnectTimeout)	\
    (This)->lpVtbl -> getFaultTolerantReconnectTimeout(This,faultTolerantReconnectTimeout)

#define IJMSQueueConnectionFactory_setInitialConnectTimeout(This,initialConnectTimeout)	\
    (This)->lpVtbl -> setInitialConnectTimeout(This,initialConnectTimeout)

#define IJMSQueueConnectionFactory_getInitialConnectTimeout(This,initialConnectTimeout)	\
    (This)->lpVtbl -> getInitialConnectTimeout(This,initialConnectTimeout)

#define IJMSQueueConnectionFactory_setClientTransactionBufferSize(This,size)	\
    (This)->lpVtbl -> setClientTransactionBufferSize(This,size)

#define IJMSQueueConnectionFactory_getClientTransactionBufferSize(This,size)	\
    (This)->lpVtbl -> getClientTransactionBufferSize(This,size)

#define IJMSQueueConnectionFactory_getMinimizeSubscriberTraffic(This,minimizeTraffic)	\
    (This)->lpVtbl -> getMinimizeSubscriberTraffic(This,minimizeTraffic)

#define IJMSQueueConnectionFactory_setMinimizeSubscriberTraffic(This,minimizeTraffic)	\
    (This)->lpVtbl -> setMinimizeSubscriberTraffic(This,minimizeTraffic)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_initialize1_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ BSTR brokerURL);


void __RPC_STUB IJMSQueueConnectionFactory_initialize1_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_initialize2_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ BSTR brokerHostName,
    /* [in] */ int brokerPort,
    /* [in] */ BSTR brokerProtocol,
    /* [in] */ BSTR defaultUserName,
    /* [in] */ BSTR defaultPassword);


void __RPC_STUB IJMSQueueConnectionFactory_initialize2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_initialize3_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ BSTR brokerHostName,
    /* [in] */ int brokerPort,
    /* [in] */ BSTR brokerProtocol,
    /* [in] */ BSTR connectID,
    /* [in] */ BSTR defaultPassword);


void __RPC_STUB IJMSQueueConnectionFactory_initialize3_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_initialize4_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ BSTR brokerURL,
    /* [in] */ BSTR connectID);


void __RPC_STUB IJMSQueueConnectionFactory_initialize4_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_initialize5_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ BSTR brokerURL,
    /* [in] */ BSTR defaultUserName,
    /* [in] */ BSTR defaultPassword);


void __RPC_STUB IJMSQueueConnectionFactory_initialize5_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_initialize6_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ BSTR brokerURL,
    /* [in] */ BSTR connectID,
    /* [in] */ BSTR defaultUserName,
    /* [in] */ BSTR defaultPassword);


void __RPC_STUB IJMSQueueConnectionFactory_initialize6_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_createQueueConnection_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ IJMSQueueConnection **ppConn);


void __RPC_STUB IJMSQueueConnectionFactory_createQueueConnection_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_createQueueConnection2_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ BSTR username,
    /* [in] */ BSTR password,
    /* [retval][out] */ IJMSQueueConnection **ppConn);


void __RPC_STUB IJMSQueueConnectionFactory_createQueueConnection2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getBrokerHostName_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ BSTR *brokerHostName);


void __RPC_STUB IJMSQueueConnectionFactory_getBrokerHostName_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getBrokerPort_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ int *brokerPort);


void __RPC_STUB IJMSQueueConnectionFactory_getBrokerPort_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getBrokerProtocol_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ BSTR *brokerProtocol);


void __RPC_STUB IJMSQueueConnectionFactory_getBrokerProtocol_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getBrokerURL_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ BSTR *brokerURL);


void __RPC_STUB IJMSQueueConnectionFactory_getBrokerURL_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getClientID_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ BSTR *clientID);


void __RPC_STUB IJMSQueueConnectionFactory_getClientID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getConnectID_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ BSTR *connectID);


void __RPC_STUB IJMSQueueConnectionFactory_getConnectID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getConnectionURLs_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ BSTR *connectionURLs);


void __RPC_STUB IJMSQueueConnectionFactory_getConnectionURLs_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getDefaultPassword_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ BSTR *defaultPassword);


void __RPC_STUB IJMSQueueConnectionFactory_getDefaultPassword_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getDefaultUser_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ BSTR *defaultUser);


void __RPC_STUB IJMSQueueConnectionFactory_getDefaultUser_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getLoadBalancing_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ BOOL *loadBalancing);


void __RPC_STUB IJMSQueueConnectionFactory_getLoadBalancing_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getSequential_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ BOOL *sequential);


void __RPC_STUB IJMSQueueConnectionFactory_getSequential_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getTcpNoDelay_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ BOOL *tcpNoDelay);


void __RPC_STUB IJMSQueueConnectionFactory_getTcpNoDelay_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_setBrokerHostName_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ BSTR brokerHostName);


void __RPC_STUB IJMSQueueConnectionFactory_setBrokerHostName_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_setBrokerPort_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ int brokerPort);


void __RPC_STUB IJMSQueueConnectionFactory_setBrokerPort_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_setBrokerProtocol_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ BSTR brokerProtocol);


void __RPC_STUB IJMSQueueConnectionFactory_setBrokerProtocol_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_setBrokerURL_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ BSTR brokerURL);


void __RPC_STUB IJMSQueueConnectionFactory_setBrokerURL_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_setClientID_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ BSTR clientID);


void __RPC_STUB IJMSQueueConnectionFactory_setClientID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_setConnectID_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ BSTR connectID);


void __RPC_STUB IJMSQueueConnectionFactory_setConnectID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_setConnectionURLs_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ BSTR connectionURLs);


void __RPC_STUB IJMSQueueConnectionFactory_setConnectionURLs_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_setLoadBalancing_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ BOOL loadBalancing);


void __RPC_STUB IJMSQueueConnectionFactory_setLoadBalancing_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_setSequential_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ BOOL sequential);


void __RPC_STUB IJMSQueueConnectionFactory_setSequential_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_setTcpNoDelay_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ BOOL tcpNoDelay);


void __RPC_STUB IJMSQueueConnectionFactory_setTcpNoDelay_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_setMonitorInterval_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ long interval);


void __RPC_STUB IJMSQueueConnectionFactory_setMonitorInterval_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getMonitorInterval_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ long *interval);


void __RPC_STUB IJMSQueueConnectionFactory_getMonitorInterval_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_setDefaultTxnBatchSize_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ long txnBatchSize);


void __RPC_STUB IJMSQueueConnectionFactory_setDefaultTxnBatchSize_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getDefaultTxnBatchSize_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ long *txnBatchSize);


void __RPC_STUB IJMSQueueConnectionFactory_getDefaultTxnBatchSize_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_setTCPConnectionTimeout_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ long tcpTimeout);


void __RPC_STUB IJMSQueueConnectionFactory_setTCPConnectionTimeout_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getTCPConnectionTimeout_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ long *tcpTimeout);


void __RPC_STUB IJMSQueueConnectionFactory_getTCPConnectionTimeout_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_setFaultTolerant_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ BOOL faultTolerant);


void __RPC_STUB IJMSQueueConnectionFactory_setFaultTolerant_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getFaultTolerant_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ BOOL *faultTolerant);


void __RPC_STUB IJMSQueueConnectionFactory_getFaultTolerant_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_setFaultTolerantReconnectTimeout_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ long faultTolerantReconnectTimeout);


void __RPC_STUB IJMSQueueConnectionFactory_setFaultTolerantReconnectTimeout_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getFaultTolerantReconnectTimeout_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ long *faultTolerantReconnectTimeout);


void __RPC_STUB IJMSQueueConnectionFactory_getFaultTolerantReconnectTimeout_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_setInitialConnectTimeout_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ long initialConnectTimeout);


void __RPC_STUB IJMSQueueConnectionFactory_setInitialConnectTimeout_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getInitialConnectTimeout_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ long *initialConnectTimeout);


void __RPC_STUB IJMSQueueConnectionFactory_getInitialConnectTimeout_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_setClientTransactionBufferSize_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ long size);


void __RPC_STUB IJMSQueueConnectionFactory_setClientTransactionBufferSize_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getClientTransactionBufferSize_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ long *size);


void __RPC_STUB IJMSQueueConnectionFactory_getClientTransactionBufferSize_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_getMinimizeSubscriberTraffic_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [retval][out] */ BOOL *minimizeTraffic);


void __RPC_STUB IJMSQueueConnectionFactory_getMinimizeSubscriberTraffic_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnectionFactory_setMinimizeSubscriberTraffic_Proxy( 
    IJMSQueueConnectionFactory * This,
    /* [in] */ BOOL minimizeTraffic);


void __RPC_STUB IJMSQueueConnectionFactory_setMinimizeSubscriberTraffic_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSQueueConnectionFactory_INTERFACE_DEFINED__ */


#ifndef __IJMSTopicConnectionFactory_INTERFACE_DEFINED__
#define __IJMSTopicConnectionFactory_INTERFACE_DEFINED__

/* interface IJMSTopicConnectionFactory */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSTopicConnectionFactory;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("4E8E3C59-433D-426B-A7CE-9ABEA21E326A")
    IJMSTopicConnectionFactory : public IJMSObject
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE initialize1( 
            /* [in] */ BSTR brokerURL) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE initialize2( 
            /* [in] */ BSTR brokerHostName,
            /* [in] */ int brokerPort,
            /* [in] */ BSTR brokerProtocol,
            /* [in] */ BSTR defaultUserName,
            /* [in] */ BSTR defaultPassword) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE initialize3( 
            /* [in] */ BSTR brokerHostName,
            /* [in] */ int brokerPort,
            /* [in] */ BSTR brokerProtocol,
            /* [in] */ BSTR connectID,
            /* [in] */ BSTR defaultPassword) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE initialize4( 
            /* [in] */ BSTR brokerURL,
            /* [in] */ BSTR connectID) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE initialize5( 
            /* [in] */ BSTR brokerURL,
            /* [in] */ BSTR defaultUserName,
            /* [in] */ BSTR defaultPassword) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE initialize6( 
            /* [in] */ BSTR brokerURL,
            /* [in] */ BSTR connectID,
            /* [in] */ BSTR defaultUserName,
            /* [in] */ BSTR defaultPassword) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createTopicConnection( 
            /* [retval][out] */ IJMSTopicConnection **ppConn) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createTopicConnection2( 
            /* [in] */ BSTR username,
            /* [in] */ BSTR password,
            /* [retval][out] */ IJMSTopicConnection **ppConn) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getBrokerHostName( 
            /* [retval][out] */ BSTR *brokerHostName) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getBrokerPort( 
            /* [retval][out] */ int *brokerPort) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getBrokerProtocol( 
            /* [retval][out] */ BSTR *brokerProtocol) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getBrokerURL( 
            /* [retval][out] */ BSTR *brokerURL) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getClientID( 
            /* [retval][out] */ BSTR *clientID) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getConnectID( 
            /* [retval][out] */ BSTR *connectID) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getConnectionURLs( 
            /* [retval][out] */ BSTR *connectionURLs) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getDefaultPassword( 
            /* [retval][out] */ BSTR *defaultPassword) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getDefaultUser( 
            /* [retval][out] */ BSTR *defaultUser) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getLoadBalancing( 
            /* [retval][out] */ BOOL *loadBalancing) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getSequential( 
            /* [retval][out] */ BOOL *sequential) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getTcpNoDelay( 
            /* [retval][out] */ BOOL *tcpNoDelay) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setBrokerHostName( 
            /* [in] */ BSTR brokerHostName) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setBrokerPort( 
            /* [in] */ int brokerPort) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setBrokerProtocol( 
            /* [in] */ BSTR brokerProtocol) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setBrokerURL( 
            /* [in] */ BSTR brokerURL) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setClientID( 
            /* [in] */ BSTR clientID) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setConnectID( 
            /* [in] */ BSTR connectID) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setConnectionURLs( 
            /* [in] */ BSTR connectionURLs) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setLoadBalancing( 
            /* [in] */ BOOL loadBalancing) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setSequential( 
            /* [in] */ BOOL sequential) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setTcpNoDelay( 
            /* [in] */ BOOL tcpNoDelay) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setMonitorInterval( 
            /* [in] */ long interval) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getMonitorInterval( 
            /* [retval][out] */ long *interval) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setDefaultTxnBatchSize( 
            /* [in] */ long txnBatchSize) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getDefaultTxnBatchSize( 
            /* [retval][out] */ long *txnBatchSize) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setTCPConnectionTimeout( 
            /* [in] */ long tcpTimeout) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getTCPConnectionTimeout( 
            /* [retval][out] */ long *tcpTimeout) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setFaultTolerant( 
            /* [in] */ BOOL faultTolerant) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getFaultTolerant( 
            /* [retval][out] */ BOOL *faultTolerant) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setFaultTolerantReconnectTimeout( 
            /* [in] */ long faultTolerantReconnectTimeout) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getFaultTolerantReconnectTimeout( 
            /* [retval][out] */ long *faultTolerantReconnectTimeout) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setInitialConnectTimeout( 
            /* [in] */ long initialConnectTimeout) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getInitialConnectTimeout( 
            /* [retval][out] */ long *initialConnectTimeout) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setClientTransactionBufferSize( 
            /* [in] */ long size) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getClientTransactionBufferSize( 
            /* [retval][out] */ long *size) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getMinimizeSubscriberTraffic( 
            /* [retval][out] */ BOOL *minimizeTraffic) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setMinimizeSubscriberTraffic( 
            /* [in] */ BOOL minimizeTraffic) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSTopicConnectionFactoryVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSTopicConnectionFactory * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSTopicConnectionFactory * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSTopicConnectionFactory * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *initialize1 )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ BSTR brokerURL);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *initialize2 )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ BSTR brokerHostName,
            /* [in] */ int brokerPort,
            /* [in] */ BSTR brokerProtocol,
            /* [in] */ BSTR defaultUserName,
            /* [in] */ BSTR defaultPassword);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *initialize3 )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ BSTR brokerHostName,
            /* [in] */ int brokerPort,
            /* [in] */ BSTR brokerProtocol,
            /* [in] */ BSTR connectID,
            /* [in] */ BSTR defaultPassword);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *initialize4 )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ BSTR brokerURL,
            /* [in] */ BSTR connectID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *initialize5 )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ BSTR brokerURL,
            /* [in] */ BSTR defaultUserName,
            /* [in] */ BSTR defaultPassword);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *initialize6 )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ BSTR brokerURL,
            /* [in] */ BSTR connectID,
            /* [in] */ BSTR defaultUserName,
            /* [in] */ BSTR defaultPassword);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createTopicConnection )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ IJMSTopicConnection **ppConn);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createTopicConnection2 )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ BSTR username,
            /* [in] */ BSTR password,
            /* [retval][out] */ IJMSTopicConnection **ppConn);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBrokerHostName )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ BSTR *brokerHostName);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBrokerPort )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ int *brokerPort);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBrokerProtocol )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ BSTR *brokerProtocol);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBrokerURL )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ BSTR *brokerURL);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getClientID )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ BSTR *clientID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getConnectID )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ BSTR *connectID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getConnectionURLs )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ BSTR *connectionURLs);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDefaultPassword )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ BSTR *defaultPassword);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDefaultUser )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ BSTR *defaultUser);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getLoadBalancing )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ BOOL *loadBalancing);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getSequential )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ BOOL *sequential);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getTcpNoDelay )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ BOOL *tcpNoDelay);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setBrokerHostName )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ BSTR brokerHostName);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setBrokerPort )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ int brokerPort);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setBrokerProtocol )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ BSTR brokerProtocol);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setBrokerURL )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ BSTR brokerURL);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setClientID )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ BSTR clientID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setConnectID )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ BSTR connectID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setConnectionURLs )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ BSTR connectionURLs);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setLoadBalancing )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ BOOL loadBalancing);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setSequential )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ BOOL sequential);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setTcpNoDelay )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ BOOL tcpNoDelay);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setMonitorInterval )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ long interval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getMonitorInterval )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ long *interval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setDefaultTxnBatchSize )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ long txnBatchSize);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDefaultTxnBatchSize )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ long *txnBatchSize);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setTCPConnectionTimeout )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ long tcpTimeout);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getTCPConnectionTimeout )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ long *tcpTimeout);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setFaultTolerant )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ BOOL faultTolerant);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getFaultTolerant )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ BOOL *faultTolerant);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setFaultTolerantReconnectTimeout )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ long faultTolerantReconnectTimeout);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getFaultTolerantReconnectTimeout )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ long *faultTolerantReconnectTimeout);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setInitialConnectTimeout )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ long initialConnectTimeout);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getInitialConnectTimeout )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ long *initialConnectTimeout);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setClientTransactionBufferSize )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ long size);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getClientTransactionBufferSize )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ long *size);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getMinimizeSubscriberTraffic )( 
            IJMSTopicConnectionFactory * This,
            /* [retval][out] */ BOOL *minimizeTraffic);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setMinimizeSubscriberTraffic )( 
            IJMSTopicConnectionFactory * This,
            /* [in] */ BOOL minimizeTraffic);
        
        END_INTERFACE
    } IJMSTopicConnectionFactoryVtbl;

    interface IJMSTopicConnectionFactory
    {
        CONST_VTBL struct IJMSTopicConnectionFactoryVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSTopicConnectionFactory_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSTopicConnectionFactory_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSTopicConnectionFactory_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSTopicConnectionFactory_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSTopicConnectionFactory_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSTopicConnectionFactory_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSTopicConnectionFactory_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSTopicConnectionFactory_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSTopicConnectionFactory_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSTopicConnectionFactory_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSTopicConnectionFactory_initialize1(This,brokerURL)	\
    (This)->lpVtbl -> initialize1(This,brokerURL)

#define IJMSTopicConnectionFactory_initialize2(This,brokerHostName,brokerPort,brokerProtocol,defaultUserName,defaultPassword)	\
    (This)->lpVtbl -> initialize2(This,brokerHostName,brokerPort,brokerProtocol,defaultUserName,defaultPassword)

#define IJMSTopicConnectionFactory_initialize3(This,brokerHostName,brokerPort,brokerProtocol,connectID,defaultPassword)	\
    (This)->lpVtbl -> initialize3(This,brokerHostName,brokerPort,brokerProtocol,connectID,defaultPassword)

#define IJMSTopicConnectionFactory_initialize4(This,brokerURL,connectID)	\
    (This)->lpVtbl -> initialize4(This,brokerURL,connectID)

#define IJMSTopicConnectionFactory_initialize5(This,brokerURL,defaultUserName,defaultPassword)	\
    (This)->lpVtbl -> initialize5(This,brokerURL,defaultUserName,defaultPassword)

#define IJMSTopicConnectionFactory_initialize6(This,brokerURL,connectID,defaultUserName,defaultPassword)	\
    (This)->lpVtbl -> initialize6(This,brokerURL,connectID,defaultUserName,defaultPassword)

#define IJMSTopicConnectionFactory_createTopicConnection(This,ppConn)	\
    (This)->lpVtbl -> createTopicConnection(This,ppConn)

#define IJMSTopicConnectionFactory_createTopicConnection2(This,username,password,ppConn)	\
    (This)->lpVtbl -> createTopicConnection2(This,username,password,ppConn)

#define IJMSTopicConnectionFactory_getBrokerHostName(This,brokerHostName)	\
    (This)->lpVtbl -> getBrokerHostName(This,brokerHostName)

#define IJMSTopicConnectionFactory_getBrokerPort(This,brokerPort)	\
    (This)->lpVtbl -> getBrokerPort(This,brokerPort)

#define IJMSTopicConnectionFactory_getBrokerProtocol(This,brokerProtocol)	\
    (This)->lpVtbl -> getBrokerProtocol(This,brokerProtocol)

#define IJMSTopicConnectionFactory_getBrokerURL(This,brokerURL)	\
    (This)->lpVtbl -> getBrokerURL(This,brokerURL)

#define IJMSTopicConnectionFactory_getClientID(This,clientID)	\
    (This)->lpVtbl -> getClientID(This,clientID)

#define IJMSTopicConnectionFactory_getConnectID(This,connectID)	\
    (This)->lpVtbl -> getConnectID(This,connectID)

#define IJMSTopicConnectionFactory_getConnectionURLs(This,connectionURLs)	\
    (This)->lpVtbl -> getConnectionURLs(This,connectionURLs)

#define IJMSTopicConnectionFactory_getDefaultPassword(This,defaultPassword)	\
    (This)->lpVtbl -> getDefaultPassword(This,defaultPassword)

#define IJMSTopicConnectionFactory_getDefaultUser(This,defaultUser)	\
    (This)->lpVtbl -> getDefaultUser(This,defaultUser)

#define IJMSTopicConnectionFactory_getLoadBalancing(This,loadBalancing)	\
    (This)->lpVtbl -> getLoadBalancing(This,loadBalancing)

#define IJMSTopicConnectionFactory_getSequential(This,sequential)	\
    (This)->lpVtbl -> getSequential(This,sequential)

#define IJMSTopicConnectionFactory_getTcpNoDelay(This,tcpNoDelay)	\
    (This)->lpVtbl -> getTcpNoDelay(This,tcpNoDelay)

#define IJMSTopicConnectionFactory_setBrokerHostName(This,brokerHostName)	\
    (This)->lpVtbl -> setBrokerHostName(This,brokerHostName)

#define IJMSTopicConnectionFactory_setBrokerPort(This,brokerPort)	\
    (This)->lpVtbl -> setBrokerPort(This,brokerPort)

#define IJMSTopicConnectionFactory_setBrokerProtocol(This,brokerProtocol)	\
    (This)->lpVtbl -> setBrokerProtocol(This,brokerProtocol)

#define IJMSTopicConnectionFactory_setBrokerURL(This,brokerURL)	\
    (This)->lpVtbl -> setBrokerURL(This,brokerURL)

#define IJMSTopicConnectionFactory_setClientID(This,clientID)	\
    (This)->lpVtbl -> setClientID(This,clientID)

#define IJMSTopicConnectionFactory_setConnectID(This,connectID)	\
    (This)->lpVtbl -> setConnectID(This,connectID)

#define IJMSTopicConnectionFactory_setConnectionURLs(This,connectionURLs)	\
    (This)->lpVtbl -> setConnectionURLs(This,connectionURLs)

#define IJMSTopicConnectionFactory_setLoadBalancing(This,loadBalancing)	\
    (This)->lpVtbl -> setLoadBalancing(This,loadBalancing)

#define IJMSTopicConnectionFactory_setSequential(This,sequential)	\
    (This)->lpVtbl -> setSequential(This,sequential)

#define IJMSTopicConnectionFactory_setTcpNoDelay(This,tcpNoDelay)	\
    (This)->lpVtbl -> setTcpNoDelay(This,tcpNoDelay)

#define IJMSTopicConnectionFactory_setMonitorInterval(This,interval)	\
    (This)->lpVtbl -> setMonitorInterval(This,interval)

#define IJMSTopicConnectionFactory_getMonitorInterval(This,interval)	\
    (This)->lpVtbl -> getMonitorInterval(This,interval)

#define IJMSTopicConnectionFactory_setDefaultTxnBatchSize(This,txnBatchSize)	\
    (This)->lpVtbl -> setDefaultTxnBatchSize(This,txnBatchSize)

#define IJMSTopicConnectionFactory_getDefaultTxnBatchSize(This,txnBatchSize)	\
    (This)->lpVtbl -> getDefaultTxnBatchSize(This,txnBatchSize)

#define IJMSTopicConnectionFactory_setTCPConnectionTimeout(This,tcpTimeout)	\
    (This)->lpVtbl -> setTCPConnectionTimeout(This,tcpTimeout)

#define IJMSTopicConnectionFactory_getTCPConnectionTimeout(This,tcpTimeout)	\
    (This)->lpVtbl -> getTCPConnectionTimeout(This,tcpTimeout)

#define IJMSTopicConnectionFactory_setFaultTolerant(This,faultTolerant)	\
    (This)->lpVtbl -> setFaultTolerant(This,faultTolerant)

#define IJMSTopicConnectionFactory_getFaultTolerant(This,faultTolerant)	\
    (This)->lpVtbl -> getFaultTolerant(This,faultTolerant)

#define IJMSTopicConnectionFactory_setFaultTolerantReconnectTimeout(This,faultTolerantReconnectTimeout)	\
    (This)->lpVtbl -> setFaultTolerantReconnectTimeout(This,faultTolerantReconnectTimeout)

#define IJMSTopicConnectionFactory_getFaultTolerantReconnectTimeout(This,faultTolerantReconnectTimeout)	\
    (This)->lpVtbl -> getFaultTolerantReconnectTimeout(This,faultTolerantReconnectTimeout)

#define IJMSTopicConnectionFactory_setInitialConnectTimeout(This,initialConnectTimeout)	\
    (This)->lpVtbl -> setInitialConnectTimeout(This,initialConnectTimeout)

#define IJMSTopicConnectionFactory_getInitialConnectTimeout(This,initialConnectTimeout)	\
    (This)->lpVtbl -> getInitialConnectTimeout(This,initialConnectTimeout)

#define IJMSTopicConnectionFactory_setClientTransactionBufferSize(This,size)	\
    (This)->lpVtbl -> setClientTransactionBufferSize(This,size)

#define IJMSTopicConnectionFactory_getClientTransactionBufferSize(This,size)	\
    (This)->lpVtbl -> getClientTransactionBufferSize(This,size)

#define IJMSTopicConnectionFactory_getMinimizeSubscriberTraffic(This,minimizeTraffic)	\
    (This)->lpVtbl -> getMinimizeSubscriberTraffic(This,minimizeTraffic)

#define IJMSTopicConnectionFactory_setMinimizeSubscriberTraffic(This,minimizeTraffic)	\
    (This)->lpVtbl -> setMinimizeSubscriberTraffic(This,minimizeTraffic)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_initialize1_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ BSTR brokerURL);


void __RPC_STUB IJMSTopicConnectionFactory_initialize1_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_initialize2_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ BSTR brokerHostName,
    /* [in] */ int brokerPort,
    /* [in] */ BSTR brokerProtocol,
    /* [in] */ BSTR defaultUserName,
    /* [in] */ BSTR defaultPassword);


void __RPC_STUB IJMSTopicConnectionFactory_initialize2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_initialize3_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ BSTR brokerHostName,
    /* [in] */ int brokerPort,
    /* [in] */ BSTR brokerProtocol,
    /* [in] */ BSTR connectID,
    /* [in] */ BSTR defaultPassword);


void __RPC_STUB IJMSTopicConnectionFactory_initialize3_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_initialize4_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ BSTR brokerURL,
    /* [in] */ BSTR connectID);


void __RPC_STUB IJMSTopicConnectionFactory_initialize4_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_initialize5_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ BSTR brokerURL,
    /* [in] */ BSTR defaultUserName,
    /* [in] */ BSTR defaultPassword);


void __RPC_STUB IJMSTopicConnectionFactory_initialize5_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_initialize6_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ BSTR brokerURL,
    /* [in] */ BSTR connectID,
    /* [in] */ BSTR defaultUserName,
    /* [in] */ BSTR defaultPassword);


void __RPC_STUB IJMSTopicConnectionFactory_initialize6_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_createTopicConnection_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ IJMSTopicConnection **ppConn);


void __RPC_STUB IJMSTopicConnectionFactory_createTopicConnection_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_createTopicConnection2_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ BSTR username,
    /* [in] */ BSTR password,
    /* [retval][out] */ IJMSTopicConnection **ppConn);


void __RPC_STUB IJMSTopicConnectionFactory_createTopicConnection2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getBrokerHostName_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ BSTR *brokerHostName);


void __RPC_STUB IJMSTopicConnectionFactory_getBrokerHostName_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getBrokerPort_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ int *brokerPort);


void __RPC_STUB IJMSTopicConnectionFactory_getBrokerPort_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getBrokerProtocol_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ BSTR *brokerProtocol);


void __RPC_STUB IJMSTopicConnectionFactory_getBrokerProtocol_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getBrokerURL_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ BSTR *brokerURL);


void __RPC_STUB IJMSTopicConnectionFactory_getBrokerURL_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getClientID_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ BSTR *clientID);


void __RPC_STUB IJMSTopicConnectionFactory_getClientID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getConnectID_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ BSTR *connectID);


void __RPC_STUB IJMSTopicConnectionFactory_getConnectID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getConnectionURLs_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ BSTR *connectionURLs);


void __RPC_STUB IJMSTopicConnectionFactory_getConnectionURLs_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getDefaultPassword_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ BSTR *defaultPassword);


void __RPC_STUB IJMSTopicConnectionFactory_getDefaultPassword_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getDefaultUser_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ BSTR *defaultUser);


void __RPC_STUB IJMSTopicConnectionFactory_getDefaultUser_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getLoadBalancing_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ BOOL *loadBalancing);


void __RPC_STUB IJMSTopicConnectionFactory_getLoadBalancing_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getSequential_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ BOOL *sequential);


void __RPC_STUB IJMSTopicConnectionFactory_getSequential_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getTcpNoDelay_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ BOOL *tcpNoDelay);


void __RPC_STUB IJMSTopicConnectionFactory_getTcpNoDelay_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_setBrokerHostName_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ BSTR brokerHostName);


void __RPC_STUB IJMSTopicConnectionFactory_setBrokerHostName_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_setBrokerPort_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ int brokerPort);


void __RPC_STUB IJMSTopicConnectionFactory_setBrokerPort_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_setBrokerProtocol_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ BSTR brokerProtocol);


void __RPC_STUB IJMSTopicConnectionFactory_setBrokerProtocol_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_setBrokerURL_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ BSTR brokerURL);


void __RPC_STUB IJMSTopicConnectionFactory_setBrokerURL_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_setClientID_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ BSTR clientID);


void __RPC_STUB IJMSTopicConnectionFactory_setClientID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_setConnectID_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ BSTR connectID);


void __RPC_STUB IJMSTopicConnectionFactory_setConnectID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_setConnectionURLs_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ BSTR connectionURLs);


void __RPC_STUB IJMSTopicConnectionFactory_setConnectionURLs_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_setLoadBalancing_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ BOOL loadBalancing);


void __RPC_STUB IJMSTopicConnectionFactory_setLoadBalancing_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_setSequential_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ BOOL sequential);


void __RPC_STUB IJMSTopicConnectionFactory_setSequential_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_setTcpNoDelay_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ BOOL tcpNoDelay);


void __RPC_STUB IJMSTopicConnectionFactory_setTcpNoDelay_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_setMonitorInterval_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ long interval);


void __RPC_STUB IJMSTopicConnectionFactory_setMonitorInterval_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getMonitorInterval_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ long *interval);


void __RPC_STUB IJMSTopicConnectionFactory_getMonitorInterval_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_setDefaultTxnBatchSize_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ long txnBatchSize);


void __RPC_STUB IJMSTopicConnectionFactory_setDefaultTxnBatchSize_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getDefaultTxnBatchSize_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ long *txnBatchSize);


void __RPC_STUB IJMSTopicConnectionFactory_getDefaultTxnBatchSize_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_setTCPConnectionTimeout_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ long tcpTimeout);


void __RPC_STUB IJMSTopicConnectionFactory_setTCPConnectionTimeout_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getTCPConnectionTimeout_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ long *tcpTimeout);


void __RPC_STUB IJMSTopicConnectionFactory_getTCPConnectionTimeout_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_setFaultTolerant_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ BOOL faultTolerant);


void __RPC_STUB IJMSTopicConnectionFactory_setFaultTolerant_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getFaultTolerant_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ BOOL *faultTolerant);


void __RPC_STUB IJMSTopicConnectionFactory_getFaultTolerant_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_setFaultTolerantReconnectTimeout_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ long faultTolerantReconnectTimeout);


void __RPC_STUB IJMSTopicConnectionFactory_setFaultTolerantReconnectTimeout_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getFaultTolerantReconnectTimeout_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ long *faultTolerantReconnectTimeout);


void __RPC_STUB IJMSTopicConnectionFactory_getFaultTolerantReconnectTimeout_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_setInitialConnectTimeout_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ long initialConnectTimeout);


void __RPC_STUB IJMSTopicConnectionFactory_setInitialConnectTimeout_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getInitialConnectTimeout_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ long *initialConnectTimeout);


void __RPC_STUB IJMSTopicConnectionFactory_getInitialConnectTimeout_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_setClientTransactionBufferSize_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ long size);


void __RPC_STUB IJMSTopicConnectionFactory_setClientTransactionBufferSize_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getClientTransactionBufferSize_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ long *size);


void __RPC_STUB IJMSTopicConnectionFactory_getClientTransactionBufferSize_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_getMinimizeSubscriberTraffic_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [retval][out] */ BOOL *minimizeTraffic);


void __RPC_STUB IJMSTopicConnectionFactory_getMinimizeSubscriberTraffic_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnectionFactory_setMinimizeSubscriberTraffic_Proxy( 
    IJMSTopicConnectionFactory * This,
    /* [in] */ BOOL minimizeTraffic);


void __RPC_STUB IJMSTopicConnectionFactory_setMinimizeSubscriberTraffic_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSTopicConnectionFactory_INTERFACE_DEFINED__ */


#ifndef __IJMSDestination_INTERFACE_DEFINED__
#define __IJMSDestination_INTERFACE_DEFINED__

/* interface IJMSDestination */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSDestination;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("05B45050-90EE-4006-A3A7-94664706FA9D")
    IJMSDestination : public IJMSObject
    {
    public:
    };
    
#else 	/* C style interface */

    typedef struct IJMSDestinationVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSDestination * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSDestination * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSDestination * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSDestination * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSDestination * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSDestination * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSDestination * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSDestination * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSDestination * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSDestination * This,
            /* [retval][out] */ BSTR *objDesc);
        
        END_INTERFACE
    } IJMSDestinationVtbl;

    interface IJMSDestination
    {
        CONST_VTBL struct IJMSDestinationVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSDestination_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSDestination_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSDestination_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSDestination_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSDestination_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSDestination_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSDestination_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSDestination_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSDestination_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSDestination_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IJMSDestination_INTERFACE_DEFINED__ */


#ifndef __IJMSQueue_INTERFACE_DEFINED__
#define __IJMSQueue_INTERFACE_DEFINED__

/* interface IJMSQueue */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSQueue;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("6004D8DB-06A7-404E-A985-F91592032CDF")
    IJMSQueue : public IJMSDestination
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getQueueName( 
            /* [retval][out] */ BSTR *name) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSQueueVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSQueue * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSQueue * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSQueue * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSQueue * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSQueue * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSQueue * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSQueue * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSQueue * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSQueue * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSQueue * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getQueueName )( 
            IJMSQueue * This,
            /* [retval][out] */ BSTR *name);
        
        END_INTERFACE
    } IJMSQueueVtbl;

    interface IJMSQueue
    {
        CONST_VTBL struct IJMSQueueVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSQueue_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSQueue_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSQueue_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSQueue_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSQueue_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSQueue_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSQueue_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSQueue_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSQueue_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSQueue_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)



#define IJMSQueue_getQueueName(This,name)	\
    (This)->lpVtbl -> getQueueName(This,name)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueue_getQueueName_Proxy( 
    IJMSQueue * This,
    /* [retval][out] */ BSTR *name);


void __RPC_STUB IJMSQueue_getQueueName_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSQueue_INTERFACE_DEFINED__ */


#ifndef __IJMSTopic_INTERFACE_DEFINED__
#define __IJMSTopic_INTERFACE_DEFINED__

/* interface IJMSTopic */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSTopic;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("BA408552-F6E9-48A8-AB88-D8CB39A6BC71")
    IJMSTopic : public IJMSDestination
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getTopicName( 
            /* [retval][out] */ BSTR *name) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSTopicVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSTopic * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSTopic * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSTopic * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSTopic * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSTopic * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSTopic * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSTopic * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSTopic * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSTopic * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSTopic * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getTopicName )( 
            IJMSTopic * This,
            /* [retval][out] */ BSTR *name);
        
        END_INTERFACE
    } IJMSTopicVtbl;

    interface IJMSTopic
    {
        CONST_VTBL struct IJMSTopicVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSTopic_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSTopic_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSTopic_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSTopic_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSTopic_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSTopic_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSTopic_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSTopic_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSTopic_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSTopic_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)



#define IJMSTopic_getTopicName(This,name)	\
    (This)->lpVtbl -> getTopicName(This,name)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopic_getTopicName_Proxy( 
    IJMSTopic * This,
    /* [retval][out] */ BSTR *name);


void __RPC_STUB IJMSTopic_getTopicName_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSTopic_INTERFACE_DEFINED__ */


#ifndef __IJMSTemporaryQueue_INTERFACE_DEFINED__
#define __IJMSTemporaryQueue_INTERFACE_DEFINED__

/* interface IJMSTemporaryQueue */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSTemporaryQueue;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("331034AF-80B4-42E5-95DF-372F0E09CA76")
    IJMSTemporaryQueue : public IJMSQueue
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE deleteQueue( void) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSTemporaryQueueVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSTemporaryQueue * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSTemporaryQueue * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSTemporaryQueue * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSTemporaryQueue * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSTemporaryQueue * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSTemporaryQueue * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSTemporaryQueue * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSTemporaryQueue * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSTemporaryQueue * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSTemporaryQueue * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getQueueName )( 
            IJMSTemporaryQueue * This,
            /* [retval][out] */ BSTR *name);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *deleteQueue )( 
            IJMSTemporaryQueue * This);
        
        END_INTERFACE
    } IJMSTemporaryQueueVtbl;

    interface IJMSTemporaryQueue
    {
        CONST_VTBL struct IJMSTemporaryQueueVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSTemporaryQueue_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSTemporaryQueue_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSTemporaryQueue_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSTemporaryQueue_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSTemporaryQueue_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSTemporaryQueue_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSTemporaryQueue_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSTemporaryQueue_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSTemporaryQueue_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSTemporaryQueue_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)



#define IJMSTemporaryQueue_getQueueName(This,name)	\
    (This)->lpVtbl -> getQueueName(This,name)


#define IJMSTemporaryQueue_deleteQueue(This)	\
    (This)->lpVtbl -> deleteQueue(This)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTemporaryQueue_deleteQueue_Proxy( 
    IJMSTemporaryQueue * This);


void __RPC_STUB IJMSTemporaryQueue_deleteQueue_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSTemporaryQueue_INTERFACE_DEFINED__ */


#ifndef __IJMSTemporaryTopic_INTERFACE_DEFINED__
#define __IJMSTemporaryTopic_INTERFACE_DEFINED__

/* interface IJMSTemporaryTopic */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSTemporaryTopic;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("1A53D9B0-190D-466F-A6C2-F08FA735B117")
    IJMSTemporaryTopic : public IJMSTopic
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE deleteTopic( void) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSTemporaryTopicVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSTemporaryTopic * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSTemporaryTopic * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSTemporaryTopic * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSTemporaryTopic * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSTemporaryTopic * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSTemporaryTopic * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSTemporaryTopic * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSTemporaryTopic * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSTemporaryTopic * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSTemporaryTopic * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getTopicName )( 
            IJMSTemporaryTopic * This,
            /* [retval][out] */ BSTR *name);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *deleteTopic )( 
            IJMSTemporaryTopic * This);
        
        END_INTERFACE
    } IJMSTemporaryTopicVtbl;

    interface IJMSTemporaryTopic
    {
        CONST_VTBL struct IJMSTemporaryTopicVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSTemporaryTopic_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSTemporaryTopic_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSTemporaryTopic_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSTemporaryTopic_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSTemporaryTopic_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSTemporaryTopic_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSTemporaryTopic_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSTemporaryTopic_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSTemporaryTopic_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSTemporaryTopic_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)



#define IJMSTemporaryTopic_getTopicName(This,name)	\
    (This)->lpVtbl -> getTopicName(This,name)


#define IJMSTemporaryTopic_deleteTopic(This)	\
    (This)->lpVtbl -> deleteTopic(This)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTemporaryTopic_deleteTopic_Proxy( 
    IJMSTemporaryTopic * This);


void __RPC_STUB IJMSTemporaryTopic_deleteTopic_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSTemporaryTopic_INTERFACE_DEFINED__ */


#ifndef __IJMSMessage_INTERFACE_DEFINED__
#define __IJMSMessage_INTERFACE_DEFINED__

/* interface IJMSMessage */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSMessage;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("F6E076FB-B798-47CE-9D6A-6E767225B97C")
    IJMSMessage : public IJMSObject
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE acknowledge( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE clearBody( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSDestination( 
            /* [retval][out] */ IJMSDestination **ppDest) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setJMSDestination( 
            /* [in] */ IJMSDestination *pDest) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSDeliveryMode( 
            /* [retval][out] */ enum JMSDeliveryMode *pDelMode) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setJMSDeliveryMode( 
            /* [in] */ enum JMSDeliveryMode pDelMode) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSMessageID( 
            /* [retval][out] */ BSTR *pMessageID) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setJMSMessageID( 
            /* [in] */ BSTR messageID) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSTimestamp( 
            /* [retval][out] */ long *pTimestamp) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setJMSTimestamp( 
            /* [in] */ long timestamp) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSExpiration( 
            /* [retval][out] */ long *pExpiration) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setJMSExpiration( 
            /* [in] */ long expiration) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSRedelivered( 
            /* [retval][out] */ BOOL *pRedelivered) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setJMSRedelivered( 
            /* [in] */ BOOL redelivered) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSPriority( 
            /* [retval][out] */ int *pPriority) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setJMSPriority( 
            /* [in] */ int priority) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSReplyTo( 
            /* [retval][out] */ IJMSDestination **destination) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setJMSReplyTo( 
            /* [in] */ IJMSDestination *destination) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSCorrelationID( 
            /* [retval][out] */ BSTR *pCorrelationID) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setJMSCorrelationID( 
            /* [in] */ BSTR correlationID) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSCorrelationIDAsBytes( 
            /* [retval][out] */ VARIANT *pCorrelationID) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setJMSCorrelationIDAsBytes( 
            /* [in] */ VARIANT correlationID) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSType( 
            /* [retval][out] */ BSTR *pJMSType) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setJMSType( 
            /* [in] */ BSTR JMSType) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getStringProperty( 
            /* [in] */ BSTR propName,
            /* [retval][out] */ BSTR *pProperty) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setStringProperty( 
            /* [in] */ BSTR propName,
            /* [in] */ BSTR property) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getIntProperty( 
            /* [in] */ BSTR propName,
            /* [retval][out] */ int *pProperty) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setIntProperty( 
            /* [in] */ BSTR propName,
            /* [in] */ int property) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getBooleanProperty( 
            /* [in] */ BSTR propName,
            /* [retval][out] */ BOOL *pProperty) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setBooleanProperty( 
            /* [in] */ BSTR propName,
            /* [in] */ BOOL property) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getDoubleProperty( 
            /* [in] */ BSTR propName,
            /* [retval][out] */ double *pProperty) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setDoubleProperty( 
            /* [in] */ BSTR propName,
            /* [in] */ double property) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getFloatProperty( 
            /* [in] */ BSTR propName,
            /* [retval][out] */ float *pProperty) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setFloatProperty( 
            /* [in] */ BSTR propName,
            /* [in] */ float property) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getByteProperty( 
            /* [in] */ BSTR propName,
            /* [retval][out] */ VARIANT *pProperty) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setByteProperty( 
            /* [in] */ BSTR propName,
            /* [in] */ VARIANT property) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getLongProperty( 
            /* [in] */ BSTR propName,
            /* [retval][out] */ long *pProperty) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setLongProperty( 
            /* [in] */ BSTR propName,
            /* [in] */ long property) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getShortProperty( 
            /* [in] */ BSTR propName,
            /* [retval][out] */ short *pProperty) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setShortProperty( 
            /* [in] */ BSTR propName,
            /* [in] */ short property) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getObjectProperty( 
            /* [in] */ BSTR propName,
            /* [retval][out] */ VARIANT *pProperty) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setObjectProperty( 
            /* [in] */ BSTR propName,
            /* [in] */ VARIANT property) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE clearProperties( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getPropertyNames( 
            /* [retval][out] */ IJMSEnumeration **pPropertyNames) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE propertyExists( 
            /* [in] */ BSTR propName,
            /* [retval][out] */ BOOL *pExists) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getBodySize( 
            /* [retval][out] */ long *pSize) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSTimestampLow( 
            /* [retval][out] */ long *pTimestamp) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setJMSTimestampLow( 
            /* [in] */ long timestamp) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSExpirationLow( 
            /* [retval][out] */ long *pExpiration) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setJMSExpirationLow( 
            /* [in] */ long expiration) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSTimestampHigh( 
            /* [retval][out] */ long *pTimestamp) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setJMSTimestampHigh( 
            /* [in] */ long timestamp) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getJMSExpirationHigh( 
            /* [retval][out] */ long *pExpiration) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setJMSExpirationHigh( 
            /* [in] */ long expiration) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSMessageVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSMessage * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSMessage * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSMessage * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSMessage * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSMessage * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSMessage * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSMessage * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSMessage * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSMessage * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSMessage * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *acknowledge )( 
            IJMSMessage * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clearBody )( 
            IJMSMessage * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSDestination )( 
            IJMSMessage * This,
            /* [retval][out] */ IJMSDestination **ppDest);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSDestination )( 
            IJMSMessage * This,
            /* [in] */ IJMSDestination *pDest);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSDeliveryMode )( 
            IJMSMessage * This,
            /* [retval][out] */ enum JMSDeliveryMode *pDelMode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSDeliveryMode )( 
            IJMSMessage * This,
            /* [in] */ enum JMSDeliveryMode pDelMode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSMessageID )( 
            IJMSMessage * This,
            /* [retval][out] */ BSTR *pMessageID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSMessageID )( 
            IJMSMessage * This,
            /* [in] */ BSTR messageID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSTimestamp )( 
            IJMSMessage * This,
            /* [retval][out] */ long *pTimestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSTimestamp )( 
            IJMSMessage * This,
            /* [in] */ long timestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSExpiration )( 
            IJMSMessage * This,
            /* [retval][out] */ long *pExpiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSExpiration )( 
            IJMSMessage * This,
            /* [in] */ long expiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSRedelivered )( 
            IJMSMessage * This,
            /* [retval][out] */ BOOL *pRedelivered);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSRedelivered )( 
            IJMSMessage * This,
            /* [in] */ BOOL redelivered);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSPriority )( 
            IJMSMessage * This,
            /* [retval][out] */ int *pPriority);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSPriority )( 
            IJMSMessage * This,
            /* [in] */ int priority);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSReplyTo )( 
            IJMSMessage * This,
            /* [retval][out] */ IJMSDestination **destination);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSReplyTo )( 
            IJMSMessage * This,
            /* [in] */ IJMSDestination *destination);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSCorrelationID )( 
            IJMSMessage * This,
            /* [retval][out] */ BSTR *pCorrelationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSCorrelationID )( 
            IJMSMessage * This,
            /* [in] */ BSTR correlationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSCorrelationIDAsBytes )( 
            IJMSMessage * This,
            /* [retval][out] */ VARIANT *pCorrelationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSCorrelationIDAsBytes )( 
            IJMSMessage * This,
            /* [in] */ VARIANT correlationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSType )( 
            IJMSMessage * This,
            /* [retval][out] */ BSTR *pJMSType);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSType )( 
            IJMSMessage * This,
            /* [in] */ BSTR JMSType);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getStringProperty )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ BSTR *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setStringProperty )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ BSTR property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getIntProperty )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ int *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setIntProperty )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ int property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBooleanProperty )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ BOOL *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setBooleanProperty )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ BOOL property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDoubleProperty )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ double *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setDoubleProperty )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ double property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getFloatProperty )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ float *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setFloatProperty )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ float property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getByteProperty )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ VARIANT *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setByteProperty )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ VARIANT property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getLongProperty )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ long *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setLongProperty )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ long property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getShortProperty )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ short *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setShortProperty )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ short property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getObjectProperty )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ VARIANT *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setObjectProperty )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ VARIANT property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clearProperties )( 
            IJMSMessage * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getPropertyNames )( 
            IJMSMessage * This,
            /* [retval][out] */ IJMSEnumeration **pPropertyNames);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *propertyExists )( 
            IJMSMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ BOOL *pExists);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBodySize )( 
            IJMSMessage * This,
            /* [retval][out] */ long *pSize);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSTimestampLow )( 
            IJMSMessage * This,
            /* [retval][out] */ long *pTimestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSTimestampLow )( 
            IJMSMessage * This,
            /* [in] */ long timestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSExpirationLow )( 
            IJMSMessage * This,
            /* [retval][out] */ long *pExpiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSExpirationLow )( 
            IJMSMessage * This,
            /* [in] */ long expiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSTimestampHigh )( 
            IJMSMessage * This,
            /* [retval][out] */ long *pTimestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSTimestampHigh )( 
            IJMSMessage * This,
            /* [in] */ long timestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSExpirationHigh )( 
            IJMSMessage * This,
            /* [retval][out] */ long *pExpiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSExpirationHigh )( 
            IJMSMessage * This,
            /* [in] */ long expiration);
        
        END_INTERFACE
    } IJMSMessageVtbl;

    interface IJMSMessage
    {
        CONST_VTBL struct IJMSMessageVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSMessage_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSMessage_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSMessage_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSMessage_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSMessage_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSMessage_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSMessage_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSMessage_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSMessage_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSMessage_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSMessage_acknowledge(This)	\
    (This)->lpVtbl -> acknowledge(This)

#define IJMSMessage_clearBody(This)	\
    (This)->lpVtbl -> clearBody(This)

#define IJMSMessage_getJMSDestination(This,ppDest)	\
    (This)->lpVtbl -> getJMSDestination(This,ppDest)

#define IJMSMessage_setJMSDestination(This,pDest)	\
    (This)->lpVtbl -> setJMSDestination(This,pDest)

#define IJMSMessage_getJMSDeliveryMode(This,pDelMode)	\
    (This)->lpVtbl -> getJMSDeliveryMode(This,pDelMode)

#define IJMSMessage_setJMSDeliveryMode(This,pDelMode)	\
    (This)->lpVtbl -> setJMSDeliveryMode(This,pDelMode)

#define IJMSMessage_getJMSMessageID(This,pMessageID)	\
    (This)->lpVtbl -> getJMSMessageID(This,pMessageID)

#define IJMSMessage_setJMSMessageID(This,messageID)	\
    (This)->lpVtbl -> setJMSMessageID(This,messageID)

#define IJMSMessage_getJMSTimestamp(This,pTimestamp)	\
    (This)->lpVtbl -> getJMSTimestamp(This,pTimestamp)

#define IJMSMessage_setJMSTimestamp(This,timestamp)	\
    (This)->lpVtbl -> setJMSTimestamp(This,timestamp)

#define IJMSMessage_getJMSExpiration(This,pExpiration)	\
    (This)->lpVtbl -> getJMSExpiration(This,pExpiration)

#define IJMSMessage_setJMSExpiration(This,expiration)	\
    (This)->lpVtbl -> setJMSExpiration(This,expiration)

#define IJMSMessage_getJMSRedelivered(This,pRedelivered)	\
    (This)->lpVtbl -> getJMSRedelivered(This,pRedelivered)

#define IJMSMessage_setJMSRedelivered(This,redelivered)	\
    (This)->lpVtbl -> setJMSRedelivered(This,redelivered)

#define IJMSMessage_getJMSPriority(This,pPriority)	\
    (This)->lpVtbl -> getJMSPriority(This,pPriority)

#define IJMSMessage_setJMSPriority(This,priority)	\
    (This)->lpVtbl -> setJMSPriority(This,priority)

#define IJMSMessage_getJMSReplyTo(This,destination)	\
    (This)->lpVtbl -> getJMSReplyTo(This,destination)

#define IJMSMessage_setJMSReplyTo(This,destination)	\
    (This)->lpVtbl -> setJMSReplyTo(This,destination)

#define IJMSMessage_getJMSCorrelationID(This,pCorrelationID)	\
    (This)->lpVtbl -> getJMSCorrelationID(This,pCorrelationID)

#define IJMSMessage_setJMSCorrelationID(This,correlationID)	\
    (This)->lpVtbl -> setJMSCorrelationID(This,correlationID)

#define IJMSMessage_getJMSCorrelationIDAsBytes(This,pCorrelationID)	\
    (This)->lpVtbl -> getJMSCorrelationIDAsBytes(This,pCorrelationID)

#define IJMSMessage_setJMSCorrelationIDAsBytes(This,correlationID)	\
    (This)->lpVtbl -> setJMSCorrelationIDAsBytes(This,correlationID)

#define IJMSMessage_getJMSType(This,pJMSType)	\
    (This)->lpVtbl -> getJMSType(This,pJMSType)

#define IJMSMessage_setJMSType(This,JMSType)	\
    (This)->lpVtbl -> setJMSType(This,JMSType)

#define IJMSMessage_getStringProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getStringProperty(This,propName,pProperty)

#define IJMSMessage_setStringProperty(This,propName,property)	\
    (This)->lpVtbl -> setStringProperty(This,propName,property)

#define IJMSMessage_getIntProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getIntProperty(This,propName,pProperty)

#define IJMSMessage_setIntProperty(This,propName,property)	\
    (This)->lpVtbl -> setIntProperty(This,propName,property)

#define IJMSMessage_getBooleanProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getBooleanProperty(This,propName,pProperty)

#define IJMSMessage_setBooleanProperty(This,propName,property)	\
    (This)->lpVtbl -> setBooleanProperty(This,propName,property)

#define IJMSMessage_getDoubleProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getDoubleProperty(This,propName,pProperty)

#define IJMSMessage_setDoubleProperty(This,propName,property)	\
    (This)->lpVtbl -> setDoubleProperty(This,propName,property)

#define IJMSMessage_getFloatProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getFloatProperty(This,propName,pProperty)

#define IJMSMessage_setFloatProperty(This,propName,property)	\
    (This)->lpVtbl -> setFloatProperty(This,propName,property)

#define IJMSMessage_getByteProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getByteProperty(This,propName,pProperty)

#define IJMSMessage_setByteProperty(This,propName,property)	\
    (This)->lpVtbl -> setByteProperty(This,propName,property)

#define IJMSMessage_getLongProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getLongProperty(This,propName,pProperty)

#define IJMSMessage_setLongProperty(This,propName,property)	\
    (This)->lpVtbl -> setLongProperty(This,propName,property)

#define IJMSMessage_getShortProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getShortProperty(This,propName,pProperty)

#define IJMSMessage_setShortProperty(This,propName,property)	\
    (This)->lpVtbl -> setShortProperty(This,propName,property)

#define IJMSMessage_getObjectProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getObjectProperty(This,propName,pProperty)

#define IJMSMessage_setObjectProperty(This,propName,property)	\
    (This)->lpVtbl -> setObjectProperty(This,propName,property)

#define IJMSMessage_clearProperties(This)	\
    (This)->lpVtbl -> clearProperties(This)

#define IJMSMessage_getPropertyNames(This,pPropertyNames)	\
    (This)->lpVtbl -> getPropertyNames(This,pPropertyNames)

#define IJMSMessage_propertyExists(This,propName,pExists)	\
    (This)->lpVtbl -> propertyExists(This,propName,pExists)

#define IJMSMessage_getBodySize(This,pSize)	\
    (This)->lpVtbl -> getBodySize(This,pSize)

#define IJMSMessage_getJMSTimestampLow(This,pTimestamp)	\
    (This)->lpVtbl -> getJMSTimestampLow(This,pTimestamp)

#define IJMSMessage_setJMSTimestampLow(This,timestamp)	\
    (This)->lpVtbl -> setJMSTimestampLow(This,timestamp)

#define IJMSMessage_getJMSExpirationLow(This,pExpiration)	\
    (This)->lpVtbl -> getJMSExpirationLow(This,pExpiration)

#define IJMSMessage_setJMSExpirationLow(This,expiration)	\
    (This)->lpVtbl -> setJMSExpirationLow(This,expiration)

#define IJMSMessage_getJMSTimestampHigh(This,pTimestamp)	\
    (This)->lpVtbl -> getJMSTimestampHigh(This,pTimestamp)

#define IJMSMessage_setJMSTimestampHigh(This,timestamp)	\
    (This)->lpVtbl -> setJMSTimestampHigh(This,timestamp)

#define IJMSMessage_getJMSExpirationHigh(This,pExpiration)	\
    (This)->lpVtbl -> getJMSExpirationHigh(This,pExpiration)

#define IJMSMessage_setJMSExpirationHigh(This,expiration)	\
    (This)->lpVtbl -> setJMSExpirationHigh(This,expiration)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_acknowledge_Proxy( 
    IJMSMessage * This);


void __RPC_STUB IJMSMessage_acknowledge_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_clearBody_Proxy( 
    IJMSMessage * This);


void __RPC_STUB IJMSMessage_clearBody_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getJMSDestination_Proxy( 
    IJMSMessage * This,
    /* [retval][out] */ IJMSDestination **ppDest);


void __RPC_STUB IJMSMessage_getJMSDestination_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setJMSDestination_Proxy( 
    IJMSMessage * This,
    /* [in] */ IJMSDestination *pDest);


void __RPC_STUB IJMSMessage_setJMSDestination_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getJMSDeliveryMode_Proxy( 
    IJMSMessage * This,
    /* [retval][out] */ enum JMSDeliveryMode *pDelMode);


void __RPC_STUB IJMSMessage_getJMSDeliveryMode_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setJMSDeliveryMode_Proxy( 
    IJMSMessage * This,
    /* [in] */ enum JMSDeliveryMode pDelMode);


void __RPC_STUB IJMSMessage_setJMSDeliveryMode_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getJMSMessageID_Proxy( 
    IJMSMessage * This,
    /* [retval][out] */ BSTR *pMessageID);


void __RPC_STUB IJMSMessage_getJMSMessageID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setJMSMessageID_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR messageID);


void __RPC_STUB IJMSMessage_setJMSMessageID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getJMSTimestamp_Proxy( 
    IJMSMessage * This,
    /* [retval][out] */ long *pTimestamp);


void __RPC_STUB IJMSMessage_getJMSTimestamp_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setJMSTimestamp_Proxy( 
    IJMSMessage * This,
    /* [in] */ long timestamp);


void __RPC_STUB IJMSMessage_setJMSTimestamp_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getJMSExpiration_Proxy( 
    IJMSMessage * This,
    /* [retval][out] */ long *pExpiration);


void __RPC_STUB IJMSMessage_getJMSExpiration_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setJMSExpiration_Proxy( 
    IJMSMessage * This,
    /* [in] */ long expiration);


void __RPC_STUB IJMSMessage_setJMSExpiration_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getJMSRedelivered_Proxy( 
    IJMSMessage * This,
    /* [retval][out] */ BOOL *pRedelivered);


void __RPC_STUB IJMSMessage_getJMSRedelivered_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setJMSRedelivered_Proxy( 
    IJMSMessage * This,
    /* [in] */ BOOL redelivered);


void __RPC_STUB IJMSMessage_setJMSRedelivered_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getJMSPriority_Proxy( 
    IJMSMessage * This,
    /* [retval][out] */ int *pPriority);


void __RPC_STUB IJMSMessage_getJMSPriority_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setJMSPriority_Proxy( 
    IJMSMessage * This,
    /* [in] */ int priority);


void __RPC_STUB IJMSMessage_setJMSPriority_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getJMSReplyTo_Proxy( 
    IJMSMessage * This,
    /* [retval][out] */ IJMSDestination **destination);


void __RPC_STUB IJMSMessage_getJMSReplyTo_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setJMSReplyTo_Proxy( 
    IJMSMessage * This,
    /* [in] */ IJMSDestination *destination);


void __RPC_STUB IJMSMessage_setJMSReplyTo_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getJMSCorrelationID_Proxy( 
    IJMSMessage * This,
    /* [retval][out] */ BSTR *pCorrelationID);


void __RPC_STUB IJMSMessage_getJMSCorrelationID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setJMSCorrelationID_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR correlationID);


void __RPC_STUB IJMSMessage_setJMSCorrelationID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getJMSCorrelationIDAsBytes_Proxy( 
    IJMSMessage * This,
    /* [retval][out] */ VARIANT *pCorrelationID);


void __RPC_STUB IJMSMessage_getJMSCorrelationIDAsBytes_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setJMSCorrelationIDAsBytes_Proxy( 
    IJMSMessage * This,
    /* [in] */ VARIANT correlationID);


void __RPC_STUB IJMSMessage_setJMSCorrelationIDAsBytes_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getJMSType_Proxy( 
    IJMSMessage * This,
    /* [retval][out] */ BSTR *pJMSType);


void __RPC_STUB IJMSMessage_getJMSType_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setJMSType_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR JMSType);


void __RPC_STUB IJMSMessage_setJMSType_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getStringProperty_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [retval][out] */ BSTR *pProperty);


void __RPC_STUB IJMSMessage_getStringProperty_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setStringProperty_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [in] */ BSTR property);


void __RPC_STUB IJMSMessage_setStringProperty_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getIntProperty_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [retval][out] */ int *pProperty);


void __RPC_STUB IJMSMessage_getIntProperty_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setIntProperty_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [in] */ int property);


void __RPC_STUB IJMSMessage_setIntProperty_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getBooleanProperty_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [retval][out] */ BOOL *pProperty);


void __RPC_STUB IJMSMessage_getBooleanProperty_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setBooleanProperty_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [in] */ BOOL property);


void __RPC_STUB IJMSMessage_setBooleanProperty_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getDoubleProperty_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [retval][out] */ double *pProperty);


void __RPC_STUB IJMSMessage_getDoubleProperty_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setDoubleProperty_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [in] */ double property);


void __RPC_STUB IJMSMessage_setDoubleProperty_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getFloatProperty_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [retval][out] */ float *pProperty);


void __RPC_STUB IJMSMessage_getFloatProperty_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setFloatProperty_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [in] */ float property);


void __RPC_STUB IJMSMessage_setFloatProperty_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getByteProperty_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [retval][out] */ VARIANT *pProperty);


void __RPC_STUB IJMSMessage_getByteProperty_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setByteProperty_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [in] */ VARIANT property);


void __RPC_STUB IJMSMessage_setByteProperty_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getLongProperty_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [retval][out] */ long *pProperty);


void __RPC_STUB IJMSMessage_getLongProperty_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setLongProperty_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [in] */ long property);


void __RPC_STUB IJMSMessage_setLongProperty_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getShortProperty_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [retval][out] */ short *pProperty);


void __RPC_STUB IJMSMessage_getShortProperty_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setShortProperty_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [in] */ short property);


void __RPC_STUB IJMSMessage_setShortProperty_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getObjectProperty_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [retval][out] */ VARIANT *pProperty);


void __RPC_STUB IJMSMessage_getObjectProperty_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setObjectProperty_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [in] */ VARIANT property);


void __RPC_STUB IJMSMessage_setObjectProperty_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_clearProperties_Proxy( 
    IJMSMessage * This);


void __RPC_STUB IJMSMessage_clearProperties_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getPropertyNames_Proxy( 
    IJMSMessage * This,
    /* [retval][out] */ IJMSEnumeration **pPropertyNames);


void __RPC_STUB IJMSMessage_getPropertyNames_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_propertyExists_Proxy( 
    IJMSMessage * This,
    /* [in] */ BSTR propName,
    /* [retval][out] */ BOOL *pExists);


void __RPC_STUB IJMSMessage_propertyExists_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getBodySize_Proxy( 
    IJMSMessage * This,
    /* [retval][out] */ long *pSize);


void __RPC_STUB IJMSMessage_getBodySize_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getJMSTimestampLow_Proxy( 
    IJMSMessage * This,
    /* [retval][out] */ long *pTimestamp);


void __RPC_STUB IJMSMessage_getJMSTimestampLow_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setJMSTimestampLow_Proxy( 
    IJMSMessage * This,
    /* [in] */ long timestamp);


void __RPC_STUB IJMSMessage_setJMSTimestampLow_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getJMSExpirationLow_Proxy( 
    IJMSMessage * This,
    /* [retval][out] */ long *pExpiration);


void __RPC_STUB IJMSMessage_getJMSExpirationLow_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setJMSExpirationLow_Proxy( 
    IJMSMessage * This,
    /* [in] */ long expiration);


void __RPC_STUB IJMSMessage_setJMSExpirationLow_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getJMSTimestampHigh_Proxy( 
    IJMSMessage * This,
    /* [retval][out] */ long *pTimestamp);


void __RPC_STUB IJMSMessage_getJMSTimestampHigh_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setJMSTimestampHigh_Proxy( 
    IJMSMessage * This,
    /* [in] */ long timestamp);


void __RPC_STUB IJMSMessage_setJMSTimestampHigh_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_getJMSExpirationHigh_Proxy( 
    IJMSMessage * This,
    /* [retval][out] */ long *pExpiration);


void __RPC_STUB IJMSMessage_getJMSExpirationHigh_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessage_setJMSExpirationHigh_Proxy( 
    IJMSMessage * This,
    /* [in] */ long expiration);


void __RPC_STUB IJMSMessage_setJMSExpirationHigh_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSMessage_INTERFACE_DEFINED__ */


#ifndef __IJMSBytesMessage_INTERFACE_DEFINED__
#define __IJMSBytesMessage_INTERFACE_DEFINED__

/* interface IJMSBytesMessage */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSBytesMessage;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("2C8436FB-7A2D-4570-89EE-C8561A1814D8")
    IJMSBytesMessage : public IJMSMessage
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readByte( 
            /* [retval][out] */ VARIANT *pParam) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeByte( 
            /* [in] */ VARIANT param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readUnsignedByte( 
            /* [retval][out] */ int *pParam) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readBytes( 
            /* [in] */ int length,
            /* [retval][out] */ VARIANT *pParam) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeBytes( 
            /* [in] */ VARIANT param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeBytes2( 
            /* [in] */ VARIANT param,
            /* [in] */ int offset,
            /* [in] */ int length) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readBoolean( 
            /* [retval][out] */ BOOL *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeBoolean( 
            /* [in] */ BOOL param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readChar( 
            /* [retval][out] */ VARIANT *pParam) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeChar( 
            /* [in] */ VARIANT param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readShort( 
            /* [retval][out] */ short *pParam) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeShort( 
            /* [in] */ short param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readUnsignedShort( 
            /* [retval][out] */ int *pParam) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readInt( 
            /* [retval][out] */ int *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeInt( 
            /* [in] */ int param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeLong( 
            /* [in] */ long param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readLong( 
            /* [retval][out] */ long *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readFloat( 
            /* [retval][out] */ float *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeFloat( 
            /* [in] */ float param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readDouble( 
            /* [retval][out] */ double *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeDouble( 
            /* [in] */ double param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readUTF( 
            /* [retval][out] */ BSTR *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeUTF( 
            /* [in] */ BSTR param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeObject( 
            /* [in] */ VARIANT param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE reset( void) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSBytesMessageVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSBytesMessage * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSBytesMessage * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSBytesMessage * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSBytesMessage * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSBytesMessage * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSBytesMessage * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSBytesMessage * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSBytesMessage * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *acknowledge )( 
            IJMSBytesMessage * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clearBody )( 
            IJMSBytesMessage * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSDestination )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ IJMSDestination **ppDest);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSDestination )( 
            IJMSBytesMessage * This,
            /* [in] */ IJMSDestination *pDest);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSDeliveryMode )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ enum JMSDeliveryMode *pDelMode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSDeliveryMode )( 
            IJMSBytesMessage * This,
            /* [in] */ enum JMSDeliveryMode pDelMode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSMessageID )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ BSTR *pMessageID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSMessageID )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR messageID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSTimestamp )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ long *pTimestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSTimestamp )( 
            IJMSBytesMessage * This,
            /* [in] */ long timestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSExpiration )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ long *pExpiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSExpiration )( 
            IJMSBytesMessage * This,
            /* [in] */ long expiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSRedelivered )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ BOOL *pRedelivered);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSRedelivered )( 
            IJMSBytesMessage * This,
            /* [in] */ BOOL redelivered);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSPriority )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ int *pPriority);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSPriority )( 
            IJMSBytesMessage * This,
            /* [in] */ int priority);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSReplyTo )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ IJMSDestination **destination);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSReplyTo )( 
            IJMSBytesMessage * This,
            /* [in] */ IJMSDestination *destination);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSCorrelationID )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ BSTR *pCorrelationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSCorrelationID )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR correlationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSCorrelationIDAsBytes )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ VARIANT *pCorrelationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSCorrelationIDAsBytes )( 
            IJMSBytesMessage * This,
            /* [in] */ VARIANT correlationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSType )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ BSTR *pJMSType);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSType )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR JMSType);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getStringProperty )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ BSTR *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setStringProperty )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ BSTR property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getIntProperty )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ int *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setIntProperty )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ int property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBooleanProperty )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ BOOL *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setBooleanProperty )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ BOOL property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDoubleProperty )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ double *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setDoubleProperty )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ double property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getFloatProperty )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ float *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setFloatProperty )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ float property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getByteProperty )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ VARIANT *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setByteProperty )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ VARIANT property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getLongProperty )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ long *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setLongProperty )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ long property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getShortProperty )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ short *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setShortProperty )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ short property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getObjectProperty )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ VARIANT *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setObjectProperty )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ VARIANT property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clearProperties )( 
            IJMSBytesMessage * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getPropertyNames )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ IJMSEnumeration **pPropertyNames);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *propertyExists )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ BOOL *pExists);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBodySize )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ long *pSize);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSTimestampLow )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ long *pTimestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSTimestampLow )( 
            IJMSBytesMessage * This,
            /* [in] */ long timestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSExpirationLow )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ long *pExpiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSExpirationLow )( 
            IJMSBytesMessage * This,
            /* [in] */ long expiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSTimestampHigh )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ long *pTimestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSTimestampHigh )( 
            IJMSBytesMessage * This,
            /* [in] */ long timestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSExpirationHigh )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ long *pExpiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSExpirationHigh )( 
            IJMSBytesMessage * This,
            /* [in] */ long expiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readByte )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ VARIANT *pParam);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeByte )( 
            IJMSBytesMessage * This,
            /* [in] */ VARIANT param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readUnsignedByte )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ int *pParam);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readBytes )( 
            IJMSBytesMessage * This,
            /* [in] */ int length,
            /* [retval][out] */ VARIANT *pParam);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeBytes )( 
            IJMSBytesMessage * This,
            /* [in] */ VARIANT param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeBytes2 )( 
            IJMSBytesMessage * This,
            /* [in] */ VARIANT param,
            /* [in] */ int offset,
            /* [in] */ int length);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readBoolean )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ BOOL *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeBoolean )( 
            IJMSBytesMessage * This,
            /* [in] */ BOOL param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readChar )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ VARIANT *pParam);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeChar )( 
            IJMSBytesMessage * This,
            /* [in] */ VARIANT param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readShort )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ short *pParam);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeShort )( 
            IJMSBytesMessage * This,
            /* [in] */ short param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readUnsignedShort )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ int *pParam);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readInt )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ int *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeInt )( 
            IJMSBytesMessage * This,
            /* [in] */ int param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeLong )( 
            IJMSBytesMessage * This,
            /* [in] */ long param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readLong )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ long *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readFloat )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ float *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeFloat )( 
            IJMSBytesMessage * This,
            /* [in] */ float param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readDouble )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ double *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeDouble )( 
            IJMSBytesMessage * This,
            /* [in] */ double param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readUTF )( 
            IJMSBytesMessage * This,
            /* [retval][out] */ BSTR *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeUTF )( 
            IJMSBytesMessage * This,
            /* [in] */ BSTR param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeObject )( 
            IJMSBytesMessage * This,
            /* [in] */ VARIANT param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *reset )( 
            IJMSBytesMessage * This);
        
        END_INTERFACE
    } IJMSBytesMessageVtbl;

    interface IJMSBytesMessage
    {
        CONST_VTBL struct IJMSBytesMessageVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSBytesMessage_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSBytesMessage_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSBytesMessage_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSBytesMessage_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSBytesMessage_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSBytesMessage_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSBytesMessage_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSBytesMessage_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSBytesMessage_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSBytesMessage_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSBytesMessage_acknowledge(This)	\
    (This)->lpVtbl -> acknowledge(This)

#define IJMSBytesMessage_clearBody(This)	\
    (This)->lpVtbl -> clearBody(This)

#define IJMSBytesMessage_getJMSDestination(This,ppDest)	\
    (This)->lpVtbl -> getJMSDestination(This,ppDest)

#define IJMSBytesMessage_setJMSDestination(This,pDest)	\
    (This)->lpVtbl -> setJMSDestination(This,pDest)

#define IJMSBytesMessage_getJMSDeliveryMode(This,pDelMode)	\
    (This)->lpVtbl -> getJMSDeliveryMode(This,pDelMode)

#define IJMSBytesMessage_setJMSDeliveryMode(This,pDelMode)	\
    (This)->lpVtbl -> setJMSDeliveryMode(This,pDelMode)

#define IJMSBytesMessage_getJMSMessageID(This,pMessageID)	\
    (This)->lpVtbl -> getJMSMessageID(This,pMessageID)

#define IJMSBytesMessage_setJMSMessageID(This,messageID)	\
    (This)->lpVtbl -> setJMSMessageID(This,messageID)

#define IJMSBytesMessage_getJMSTimestamp(This,pTimestamp)	\
    (This)->lpVtbl -> getJMSTimestamp(This,pTimestamp)

#define IJMSBytesMessage_setJMSTimestamp(This,timestamp)	\
    (This)->lpVtbl -> setJMSTimestamp(This,timestamp)

#define IJMSBytesMessage_getJMSExpiration(This,pExpiration)	\
    (This)->lpVtbl -> getJMSExpiration(This,pExpiration)

#define IJMSBytesMessage_setJMSExpiration(This,expiration)	\
    (This)->lpVtbl -> setJMSExpiration(This,expiration)

#define IJMSBytesMessage_getJMSRedelivered(This,pRedelivered)	\
    (This)->lpVtbl -> getJMSRedelivered(This,pRedelivered)

#define IJMSBytesMessage_setJMSRedelivered(This,redelivered)	\
    (This)->lpVtbl -> setJMSRedelivered(This,redelivered)

#define IJMSBytesMessage_getJMSPriority(This,pPriority)	\
    (This)->lpVtbl -> getJMSPriority(This,pPriority)

#define IJMSBytesMessage_setJMSPriority(This,priority)	\
    (This)->lpVtbl -> setJMSPriority(This,priority)

#define IJMSBytesMessage_getJMSReplyTo(This,destination)	\
    (This)->lpVtbl -> getJMSReplyTo(This,destination)

#define IJMSBytesMessage_setJMSReplyTo(This,destination)	\
    (This)->lpVtbl -> setJMSReplyTo(This,destination)

#define IJMSBytesMessage_getJMSCorrelationID(This,pCorrelationID)	\
    (This)->lpVtbl -> getJMSCorrelationID(This,pCorrelationID)

#define IJMSBytesMessage_setJMSCorrelationID(This,correlationID)	\
    (This)->lpVtbl -> setJMSCorrelationID(This,correlationID)

#define IJMSBytesMessage_getJMSCorrelationIDAsBytes(This,pCorrelationID)	\
    (This)->lpVtbl -> getJMSCorrelationIDAsBytes(This,pCorrelationID)

#define IJMSBytesMessage_setJMSCorrelationIDAsBytes(This,correlationID)	\
    (This)->lpVtbl -> setJMSCorrelationIDAsBytes(This,correlationID)

#define IJMSBytesMessage_getJMSType(This,pJMSType)	\
    (This)->lpVtbl -> getJMSType(This,pJMSType)

#define IJMSBytesMessage_setJMSType(This,JMSType)	\
    (This)->lpVtbl -> setJMSType(This,JMSType)

#define IJMSBytesMessage_getStringProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getStringProperty(This,propName,pProperty)

#define IJMSBytesMessage_setStringProperty(This,propName,property)	\
    (This)->lpVtbl -> setStringProperty(This,propName,property)

#define IJMSBytesMessage_getIntProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getIntProperty(This,propName,pProperty)

#define IJMSBytesMessage_setIntProperty(This,propName,property)	\
    (This)->lpVtbl -> setIntProperty(This,propName,property)

#define IJMSBytesMessage_getBooleanProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getBooleanProperty(This,propName,pProperty)

#define IJMSBytesMessage_setBooleanProperty(This,propName,property)	\
    (This)->lpVtbl -> setBooleanProperty(This,propName,property)

#define IJMSBytesMessage_getDoubleProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getDoubleProperty(This,propName,pProperty)

#define IJMSBytesMessage_setDoubleProperty(This,propName,property)	\
    (This)->lpVtbl -> setDoubleProperty(This,propName,property)

#define IJMSBytesMessage_getFloatProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getFloatProperty(This,propName,pProperty)

#define IJMSBytesMessage_setFloatProperty(This,propName,property)	\
    (This)->lpVtbl -> setFloatProperty(This,propName,property)

#define IJMSBytesMessage_getByteProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getByteProperty(This,propName,pProperty)

#define IJMSBytesMessage_setByteProperty(This,propName,property)	\
    (This)->lpVtbl -> setByteProperty(This,propName,property)

#define IJMSBytesMessage_getLongProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getLongProperty(This,propName,pProperty)

#define IJMSBytesMessage_setLongProperty(This,propName,property)	\
    (This)->lpVtbl -> setLongProperty(This,propName,property)

#define IJMSBytesMessage_getShortProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getShortProperty(This,propName,pProperty)

#define IJMSBytesMessage_setShortProperty(This,propName,property)	\
    (This)->lpVtbl -> setShortProperty(This,propName,property)

#define IJMSBytesMessage_getObjectProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getObjectProperty(This,propName,pProperty)

#define IJMSBytesMessage_setObjectProperty(This,propName,property)	\
    (This)->lpVtbl -> setObjectProperty(This,propName,property)

#define IJMSBytesMessage_clearProperties(This)	\
    (This)->lpVtbl -> clearProperties(This)

#define IJMSBytesMessage_getPropertyNames(This,pPropertyNames)	\
    (This)->lpVtbl -> getPropertyNames(This,pPropertyNames)

#define IJMSBytesMessage_propertyExists(This,propName,pExists)	\
    (This)->lpVtbl -> propertyExists(This,propName,pExists)

#define IJMSBytesMessage_getBodySize(This,pSize)	\
    (This)->lpVtbl -> getBodySize(This,pSize)

#define IJMSBytesMessage_getJMSTimestampLow(This,pTimestamp)	\
    (This)->lpVtbl -> getJMSTimestampLow(This,pTimestamp)

#define IJMSBytesMessage_setJMSTimestampLow(This,timestamp)	\
    (This)->lpVtbl -> setJMSTimestampLow(This,timestamp)

#define IJMSBytesMessage_getJMSExpirationLow(This,pExpiration)	\
    (This)->lpVtbl -> getJMSExpirationLow(This,pExpiration)

#define IJMSBytesMessage_setJMSExpirationLow(This,expiration)	\
    (This)->lpVtbl -> setJMSExpirationLow(This,expiration)

#define IJMSBytesMessage_getJMSTimestampHigh(This,pTimestamp)	\
    (This)->lpVtbl -> getJMSTimestampHigh(This,pTimestamp)

#define IJMSBytesMessage_setJMSTimestampHigh(This,timestamp)	\
    (This)->lpVtbl -> setJMSTimestampHigh(This,timestamp)

#define IJMSBytesMessage_getJMSExpirationHigh(This,pExpiration)	\
    (This)->lpVtbl -> getJMSExpirationHigh(This,pExpiration)

#define IJMSBytesMessage_setJMSExpirationHigh(This,expiration)	\
    (This)->lpVtbl -> setJMSExpirationHigh(This,expiration)


#define IJMSBytesMessage_readByte(This,pParam)	\
    (This)->lpVtbl -> readByte(This,pParam)

#define IJMSBytesMessage_writeByte(This,param)	\
    (This)->lpVtbl -> writeByte(This,param)

#define IJMSBytesMessage_readUnsignedByte(This,pParam)	\
    (This)->lpVtbl -> readUnsignedByte(This,pParam)

#define IJMSBytesMessage_readBytes(This,length,pParam)	\
    (This)->lpVtbl -> readBytes(This,length,pParam)

#define IJMSBytesMessage_writeBytes(This,param)	\
    (This)->lpVtbl -> writeBytes(This,param)

#define IJMSBytesMessage_writeBytes2(This,param,offset,length)	\
    (This)->lpVtbl -> writeBytes2(This,param,offset,length)

#define IJMSBytesMessage_readBoolean(This,param)	\
    (This)->lpVtbl -> readBoolean(This,param)

#define IJMSBytesMessage_writeBoolean(This,param)	\
    (This)->lpVtbl -> writeBoolean(This,param)

#define IJMSBytesMessage_readChar(This,pParam)	\
    (This)->lpVtbl -> readChar(This,pParam)

#define IJMSBytesMessage_writeChar(This,param)	\
    (This)->lpVtbl -> writeChar(This,param)

#define IJMSBytesMessage_readShort(This,pParam)	\
    (This)->lpVtbl -> readShort(This,pParam)

#define IJMSBytesMessage_writeShort(This,param)	\
    (This)->lpVtbl -> writeShort(This,param)

#define IJMSBytesMessage_readUnsignedShort(This,pParam)	\
    (This)->lpVtbl -> readUnsignedShort(This,pParam)

#define IJMSBytesMessage_readInt(This,param)	\
    (This)->lpVtbl -> readInt(This,param)

#define IJMSBytesMessage_writeInt(This,param)	\
    (This)->lpVtbl -> writeInt(This,param)

#define IJMSBytesMessage_writeLong(This,param)	\
    (This)->lpVtbl -> writeLong(This,param)

#define IJMSBytesMessage_readLong(This,param)	\
    (This)->lpVtbl -> readLong(This,param)

#define IJMSBytesMessage_readFloat(This,param)	\
    (This)->lpVtbl -> readFloat(This,param)

#define IJMSBytesMessage_writeFloat(This,param)	\
    (This)->lpVtbl -> writeFloat(This,param)

#define IJMSBytesMessage_readDouble(This,param)	\
    (This)->lpVtbl -> readDouble(This,param)

#define IJMSBytesMessage_writeDouble(This,param)	\
    (This)->lpVtbl -> writeDouble(This,param)

#define IJMSBytesMessage_readUTF(This,param)	\
    (This)->lpVtbl -> readUTF(This,param)

#define IJMSBytesMessage_writeUTF(This,param)	\
    (This)->lpVtbl -> writeUTF(This,param)

#define IJMSBytesMessage_writeObject(This,param)	\
    (This)->lpVtbl -> writeObject(This,param)

#define IJMSBytesMessage_reset(This)	\
    (This)->lpVtbl -> reset(This)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_readByte_Proxy( 
    IJMSBytesMessage * This,
    /* [retval][out] */ VARIANT *pParam);


void __RPC_STUB IJMSBytesMessage_readByte_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_writeByte_Proxy( 
    IJMSBytesMessage * This,
    /* [in] */ VARIANT param);


void __RPC_STUB IJMSBytesMessage_writeByte_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_readUnsignedByte_Proxy( 
    IJMSBytesMessage * This,
    /* [retval][out] */ int *pParam);


void __RPC_STUB IJMSBytesMessage_readUnsignedByte_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_readBytes_Proxy( 
    IJMSBytesMessage * This,
    /* [in] */ int length,
    /* [retval][out] */ VARIANT *pParam);


void __RPC_STUB IJMSBytesMessage_readBytes_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_writeBytes_Proxy( 
    IJMSBytesMessage * This,
    /* [in] */ VARIANT param);


void __RPC_STUB IJMSBytesMessage_writeBytes_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_writeBytes2_Proxy( 
    IJMSBytesMessage * This,
    /* [in] */ VARIANT param,
    /* [in] */ int offset,
    /* [in] */ int length);


void __RPC_STUB IJMSBytesMessage_writeBytes2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_readBoolean_Proxy( 
    IJMSBytesMessage * This,
    /* [retval][out] */ BOOL *param);


void __RPC_STUB IJMSBytesMessage_readBoolean_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_writeBoolean_Proxy( 
    IJMSBytesMessage * This,
    /* [in] */ BOOL param);


void __RPC_STUB IJMSBytesMessage_writeBoolean_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_readChar_Proxy( 
    IJMSBytesMessage * This,
    /* [retval][out] */ VARIANT *pParam);


void __RPC_STUB IJMSBytesMessage_readChar_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_writeChar_Proxy( 
    IJMSBytesMessage * This,
    /* [in] */ VARIANT param);


void __RPC_STUB IJMSBytesMessage_writeChar_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_readShort_Proxy( 
    IJMSBytesMessage * This,
    /* [retval][out] */ short *pParam);


void __RPC_STUB IJMSBytesMessage_readShort_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_writeShort_Proxy( 
    IJMSBytesMessage * This,
    /* [in] */ short param);


void __RPC_STUB IJMSBytesMessage_writeShort_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_readUnsignedShort_Proxy( 
    IJMSBytesMessage * This,
    /* [retval][out] */ int *pParam);


void __RPC_STUB IJMSBytesMessage_readUnsignedShort_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_readInt_Proxy( 
    IJMSBytesMessage * This,
    /* [retval][out] */ int *param);


void __RPC_STUB IJMSBytesMessage_readInt_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_writeInt_Proxy( 
    IJMSBytesMessage * This,
    /* [in] */ int param);


void __RPC_STUB IJMSBytesMessage_writeInt_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_writeLong_Proxy( 
    IJMSBytesMessage * This,
    /* [in] */ long param);


void __RPC_STUB IJMSBytesMessage_writeLong_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_readLong_Proxy( 
    IJMSBytesMessage * This,
    /* [retval][out] */ long *param);


void __RPC_STUB IJMSBytesMessage_readLong_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_readFloat_Proxy( 
    IJMSBytesMessage * This,
    /* [retval][out] */ float *param);


void __RPC_STUB IJMSBytesMessage_readFloat_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_writeFloat_Proxy( 
    IJMSBytesMessage * This,
    /* [in] */ float param);


void __RPC_STUB IJMSBytesMessage_writeFloat_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_readDouble_Proxy( 
    IJMSBytesMessage * This,
    /* [retval][out] */ double *param);


void __RPC_STUB IJMSBytesMessage_readDouble_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_writeDouble_Proxy( 
    IJMSBytesMessage * This,
    /* [in] */ double param);


void __RPC_STUB IJMSBytesMessage_writeDouble_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_readUTF_Proxy( 
    IJMSBytesMessage * This,
    /* [retval][out] */ BSTR *param);


void __RPC_STUB IJMSBytesMessage_readUTF_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_writeUTF_Proxy( 
    IJMSBytesMessage * This,
    /* [in] */ BSTR param);


void __RPC_STUB IJMSBytesMessage_writeUTF_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_writeObject_Proxy( 
    IJMSBytesMessage * This,
    /* [in] */ VARIANT param);


void __RPC_STUB IJMSBytesMessage_writeObject_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSBytesMessage_reset_Proxy( 
    IJMSBytesMessage * This);


void __RPC_STUB IJMSBytesMessage_reset_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSBytesMessage_INTERFACE_DEFINED__ */


#ifndef __IJMSObjectMessage_INTERFACE_DEFINED__
#define __IJMSObjectMessage_INTERFACE_DEFINED__

/* interface IJMSObjectMessage */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSObjectMessage;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("3DA68ECE-54F3-4C63-B716-F371047C9CB7")
    IJMSObjectMessage : public IJMSMessage
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getObject( 
            /* [retval][out] */ VARIANT *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setObject( 
            /* [in] */ VARIANT param) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSObjectMessageVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSObjectMessage * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSObjectMessage * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSObjectMessage * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSObjectMessage * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSObjectMessage * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSObjectMessage * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSObjectMessage * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSObjectMessage * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *acknowledge )( 
            IJMSObjectMessage * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clearBody )( 
            IJMSObjectMessage * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSDestination )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ IJMSDestination **ppDest);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSDestination )( 
            IJMSObjectMessage * This,
            /* [in] */ IJMSDestination *pDest);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSDeliveryMode )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ enum JMSDeliveryMode *pDelMode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSDeliveryMode )( 
            IJMSObjectMessage * This,
            /* [in] */ enum JMSDeliveryMode pDelMode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSMessageID )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ BSTR *pMessageID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSMessageID )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR messageID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSTimestamp )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ long *pTimestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSTimestamp )( 
            IJMSObjectMessage * This,
            /* [in] */ long timestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSExpiration )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ long *pExpiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSExpiration )( 
            IJMSObjectMessage * This,
            /* [in] */ long expiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSRedelivered )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ BOOL *pRedelivered);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSRedelivered )( 
            IJMSObjectMessage * This,
            /* [in] */ BOOL redelivered);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSPriority )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ int *pPriority);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSPriority )( 
            IJMSObjectMessage * This,
            /* [in] */ int priority);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSReplyTo )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ IJMSDestination **destination);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSReplyTo )( 
            IJMSObjectMessage * This,
            /* [in] */ IJMSDestination *destination);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSCorrelationID )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ BSTR *pCorrelationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSCorrelationID )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR correlationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSCorrelationIDAsBytes )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ VARIANT *pCorrelationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSCorrelationIDAsBytes )( 
            IJMSObjectMessage * This,
            /* [in] */ VARIANT correlationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSType )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ BSTR *pJMSType);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSType )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR JMSType);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getStringProperty )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ BSTR *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setStringProperty )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ BSTR property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getIntProperty )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ int *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setIntProperty )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ int property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBooleanProperty )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ BOOL *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setBooleanProperty )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ BOOL property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDoubleProperty )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ double *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setDoubleProperty )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ double property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getFloatProperty )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ float *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setFloatProperty )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ float property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getByteProperty )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ VARIANT *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setByteProperty )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ VARIANT property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getLongProperty )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ long *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setLongProperty )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ long property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getShortProperty )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ short *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setShortProperty )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ short property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getObjectProperty )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ VARIANT *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setObjectProperty )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ VARIANT property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clearProperties )( 
            IJMSObjectMessage * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getPropertyNames )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ IJMSEnumeration **pPropertyNames);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *propertyExists )( 
            IJMSObjectMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ BOOL *pExists);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBodySize )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ long *pSize);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSTimestampLow )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ long *pTimestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSTimestampLow )( 
            IJMSObjectMessage * This,
            /* [in] */ long timestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSExpirationLow )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ long *pExpiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSExpirationLow )( 
            IJMSObjectMessage * This,
            /* [in] */ long expiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSTimestampHigh )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ long *pTimestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSTimestampHigh )( 
            IJMSObjectMessage * This,
            /* [in] */ long timestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSExpirationHigh )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ long *pExpiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSExpirationHigh )( 
            IJMSObjectMessage * This,
            /* [in] */ long expiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getObject )( 
            IJMSObjectMessage * This,
            /* [retval][out] */ VARIANT *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setObject )( 
            IJMSObjectMessage * This,
            /* [in] */ VARIANT param);
        
        END_INTERFACE
    } IJMSObjectMessageVtbl;

    interface IJMSObjectMessage
    {
        CONST_VTBL struct IJMSObjectMessageVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSObjectMessage_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSObjectMessage_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSObjectMessage_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSObjectMessage_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSObjectMessage_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSObjectMessage_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSObjectMessage_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSObjectMessage_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSObjectMessage_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSObjectMessage_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSObjectMessage_acknowledge(This)	\
    (This)->lpVtbl -> acknowledge(This)

#define IJMSObjectMessage_clearBody(This)	\
    (This)->lpVtbl -> clearBody(This)

#define IJMSObjectMessage_getJMSDestination(This,ppDest)	\
    (This)->lpVtbl -> getJMSDestination(This,ppDest)

#define IJMSObjectMessage_setJMSDestination(This,pDest)	\
    (This)->lpVtbl -> setJMSDestination(This,pDest)

#define IJMSObjectMessage_getJMSDeliveryMode(This,pDelMode)	\
    (This)->lpVtbl -> getJMSDeliveryMode(This,pDelMode)

#define IJMSObjectMessage_setJMSDeliveryMode(This,pDelMode)	\
    (This)->lpVtbl -> setJMSDeliveryMode(This,pDelMode)

#define IJMSObjectMessage_getJMSMessageID(This,pMessageID)	\
    (This)->lpVtbl -> getJMSMessageID(This,pMessageID)

#define IJMSObjectMessage_setJMSMessageID(This,messageID)	\
    (This)->lpVtbl -> setJMSMessageID(This,messageID)

#define IJMSObjectMessage_getJMSTimestamp(This,pTimestamp)	\
    (This)->lpVtbl -> getJMSTimestamp(This,pTimestamp)

#define IJMSObjectMessage_setJMSTimestamp(This,timestamp)	\
    (This)->lpVtbl -> setJMSTimestamp(This,timestamp)

#define IJMSObjectMessage_getJMSExpiration(This,pExpiration)	\
    (This)->lpVtbl -> getJMSExpiration(This,pExpiration)

#define IJMSObjectMessage_setJMSExpiration(This,expiration)	\
    (This)->lpVtbl -> setJMSExpiration(This,expiration)

#define IJMSObjectMessage_getJMSRedelivered(This,pRedelivered)	\
    (This)->lpVtbl -> getJMSRedelivered(This,pRedelivered)

#define IJMSObjectMessage_setJMSRedelivered(This,redelivered)	\
    (This)->lpVtbl -> setJMSRedelivered(This,redelivered)

#define IJMSObjectMessage_getJMSPriority(This,pPriority)	\
    (This)->lpVtbl -> getJMSPriority(This,pPriority)

#define IJMSObjectMessage_setJMSPriority(This,priority)	\
    (This)->lpVtbl -> setJMSPriority(This,priority)

#define IJMSObjectMessage_getJMSReplyTo(This,destination)	\
    (This)->lpVtbl -> getJMSReplyTo(This,destination)

#define IJMSObjectMessage_setJMSReplyTo(This,destination)	\
    (This)->lpVtbl -> setJMSReplyTo(This,destination)

#define IJMSObjectMessage_getJMSCorrelationID(This,pCorrelationID)	\
    (This)->lpVtbl -> getJMSCorrelationID(This,pCorrelationID)

#define IJMSObjectMessage_setJMSCorrelationID(This,correlationID)	\
    (This)->lpVtbl -> setJMSCorrelationID(This,correlationID)

#define IJMSObjectMessage_getJMSCorrelationIDAsBytes(This,pCorrelationID)	\
    (This)->lpVtbl -> getJMSCorrelationIDAsBytes(This,pCorrelationID)

#define IJMSObjectMessage_setJMSCorrelationIDAsBytes(This,correlationID)	\
    (This)->lpVtbl -> setJMSCorrelationIDAsBytes(This,correlationID)

#define IJMSObjectMessage_getJMSType(This,pJMSType)	\
    (This)->lpVtbl -> getJMSType(This,pJMSType)

#define IJMSObjectMessage_setJMSType(This,JMSType)	\
    (This)->lpVtbl -> setJMSType(This,JMSType)

#define IJMSObjectMessage_getStringProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getStringProperty(This,propName,pProperty)

#define IJMSObjectMessage_setStringProperty(This,propName,property)	\
    (This)->lpVtbl -> setStringProperty(This,propName,property)

#define IJMSObjectMessage_getIntProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getIntProperty(This,propName,pProperty)

#define IJMSObjectMessage_setIntProperty(This,propName,property)	\
    (This)->lpVtbl -> setIntProperty(This,propName,property)

#define IJMSObjectMessage_getBooleanProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getBooleanProperty(This,propName,pProperty)

#define IJMSObjectMessage_setBooleanProperty(This,propName,property)	\
    (This)->lpVtbl -> setBooleanProperty(This,propName,property)

#define IJMSObjectMessage_getDoubleProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getDoubleProperty(This,propName,pProperty)

#define IJMSObjectMessage_setDoubleProperty(This,propName,property)	\
    (This)->lpVtbl -> setDoubleProperty(This,propName,property)

#define IJMSObjectMessage_getFloatProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getFloatProperty(This,propName,pProperty)

#define IJMSObjectMessage_setFloatProperty(This,propName,property)	\
    (This)->lpVtbl -> setFloatProperty(This,propName,property)

#define IJMSObjectMessage_getByteProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getByteProperty(This,propName,pProperty)

#define IJMSObjectMessage_setByteProperty(This,propName,property)	\
    (This)->lpVtbl -> setByteProperty(This,propName,property)

#define IJMSObjectMessage_getLongProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getLongProperty(This,propName,pProperty)

#define IJMSObjectMessage_setLongProperty(This,propName,property)	\
    (This)->lpVtbl -> setLongProperty(This,propName,property)

#define IJMSObjectMessage_getShortProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getShortProperty(This,propName,pProperty)

#define IJMSObjectMessage_setShortProperty(This,propName,property)	\
    (This)->lpVtbl -> setShortProperty(This,propName,property)

#define IJMSObjectMessage_getObjectProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getObjectProperty(This,propName,pProperty)

#define IJMSObjectMessage_setObjectProperty(This,propName,property)	\
    (This)->lpVtbl -> setObjectProperty(This,propName,property)

#define IJMSObjectMessage_clearProperties(This)	\
    (This)->lpVtbl -> clearProperties(This)

#define IJMSObjectMessage_getPropertyNames(This,pPropertyNames)	\
    (This)->lpVtbl -> getPropertyNames(This,pPropertyNames)

#define IJMSObjectMessage_propertyExists(This,propName,pExists)	\
    (This)->lpVtbl -> propertyExists(This,propName,pExists)

#define IJMSObjectMessage_getBodySize(This,pSize)	\
    (This)->lpVtbl -> getBodySize(This,pSize)

#define IJMSObjectMessage_getJMSTimestampLow(This,pTimestamp)	\
    (This)->lpVtbl -> getJMSTimestampLow(This,pTimestamp)

#define IJMSObjectMessage_setJMSTimestampLow(This,timestamp)	\
    (This)->lpVtbl -> setJMSTimestampLow(This,timestamp)

#define IJMSObjectMessage_getJMSExpirationLow(This,pExpiration)	\
    (This)->lpVtbl -> getJMSExpirationLow(This,pExpiration)

#define IJMSObjectMessage_setJMSExpirationLow(This,expiration)	\
    (This)->lpVtbl -> setJMSExpirationLow(This,expiration)

#define IJMSObjectMessage_getJMSTimestampHigh(This,pTimestamp)	\
    (This)->lpVtbl -> getJMSTimestampHigh(This,pTimestamp)

#define IJMSObjectMessage_setJMSTimestampHigh(This,timestamp)	\
    (This)->lpVtbl -> setJMSTimestampHigh(This,timestamp)

#define IJMSObjectMessage_getJMSExpirationHigh(This,pExpiration)	\
    (This)->lpVtbl -> getJMSExpirationHigh(This,pExpiration)

#define IJMSObjectMessage_setJMSExpirationHigh(This,expiration)	\
    (This)->lpVtbl -> setJMSExpirationHigh(This,expiration)


#define IJMSObjectMessage_getObject(This,param)	\
    (This)->lpVtbl -> getObject(This,param)

#define IJMSObjectMessage_setObject(This,param)	\
    (This)->lpVtbl -> setObject(This,param)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSObjectMessage_getObject_Proxy( 
    IJMSObjectMessage * This,
    /* [retval][out] */ VARIANT *param);


void __RPC_STUB IJMSObjectMessage_getObject_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSObjectMessage_setObject_Proxy( 
    IJMSObjectMessage * This,
    /* [in] */ VARIANT param);


void __RPC_STUB IJMSObjectMessage_setObject_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSObjectMessage_INTERFACE_DEFINED__ */


#ifndef __IJMSStreamMessage_INTERFACE_DEFINED__
#define __IJMSStreamMessage_INTERFACE_DEFINED__

/* interface IJMSStreamMessage */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSStreamMessage;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("54512FD5-EECD-4F53-8A3A-9DF6FCC15EC9")
    IJMSStreamMessage : public IJMSMessage
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readByte( 
            /* [retval][out] */ VARIANT *pParam) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeByte( 
            /* [in] */ VARIANT param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readBytes( 
            /* [in] */ int length,
            /* [retval][out] */ VARIANT *pParam) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeBytes( 
            /* [in] */ VARIANT param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeBytes2( 
            /* [in] */ VARIANT param,
            /* [in] */ int offset,
            /* [in] */ int length) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readBoolean( 
            /* [retval][out] */ BOOL *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeBoolean( 
            /* [in] */ BOOL param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readChar( 
            /* [retval][out] */ VARIANT *pParam) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeChar( 
            /* [in] */ VARIANT param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readShort( 
            /* [retval][out] */ short *pParam) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeShort( 
            /* [in] */ short param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readInt( 
            /* [retval][out] */ int *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeInt( 
            /* [in] */ int param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeLong( 
            /* [in] */ long param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readLong( 
            /* [retval][out] */ long *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readFloat( 
            /* [retval][out] */ float *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeFloat( 
            /* [in] */ float param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readDouble( 
            /* [retval][out] */ double *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeDouble( 
            /* [in] */ double param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readObject( 
            /* [retval][out] */ VARIANT *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeObject( 
            /* [in] */ VARIANT param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE readString( 
            /* [retval][out] */ BSTR *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE writeString( 
            /* [in] */ BSTR param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE reset( void) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSStreamMessageVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSStreamMessage * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSStreamMessage * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSStreamMessage * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSStreamMessage * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSStreamMessage * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSStreamMessage * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSStreamMessage * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSStreamMessage * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *acknowledge )( 
            IJMSStreamMessage * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clearBody )( 
            IJMSStreamMessage * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSDestination )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ IJMSDestination **ppDest);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSDestination )( 
            IJMSStreamMessage * This,
            /* [in] */ IJMSDestination *pDest);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSDeliveryMode )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ enum JMSDeliveryMode *pDelMode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSDeliveryMode )( 
            IJMSStreamMessage * This,
            /* [in] */ enum JMSDeliveryMode pDelMode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSMessageID )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ BSTR *pMessageID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSMessageID )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR messageID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSTimestamp )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ long *pTimestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSTimestamp )( 
            IJMSStreamMessage * This,
            /* [in] */ long timestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSExpiration )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ long *pExpiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSExpiration )( 
            IJMSStreamMessage * This,
            /* [in] */ long expiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSRedelivered )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ BOOL *pRedelivered);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSRedelivered )( 
            IJMSStreamMessage * This,
            /* [in] */ BOOL redelivered);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSPriority )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ int *pPriority);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSPriority )( 
            IJMSStreamMessage * This,
            /* [in] */ int priority);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSReplyTo )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ IJMSDestination **destination);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSReplyTo )( 
            IJMSStreamMessage * This,
            /* [in] */ IJMSDestination *destination);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSCorrelationID )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ BSTR *pCorrelationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSCorrelationID )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR correlationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSCorrelationIDAsBytes )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ VARIANT *pCorrelationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSCorrelationIDAsBytes )( 
            IJMSStreamMessage * This,
            /* [in] */ VARIANT correlationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSType )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ BSTR *pJMSType);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSType )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR JMSType);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getStringProperty )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ BSTR *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setStringProperty )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ BSTR property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getIntProperty )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ int *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setIntProperty )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ int property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBooleanProperty )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ BOOL *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setBooleanProperty )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ BOOL property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDoubleProperty )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ double *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setDoubleProperty )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ double property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getFloatProperty )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ float *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setFloatProperty )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ float property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getByteProperty )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ VARIANT *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setByteProperty )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ VARIANT property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getLongProperty )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ long *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setLongProperty )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ long property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getShortProperty )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ short *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setShortProperty )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ short property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getObjectProperty )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ VARIANT *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setObjectProperty )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ VARIANT property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clearProperties )( 
            IJMSStreamMessage * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getPropertyNames )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ IJMSEnumeration **pPropertyNames);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *propertyExists )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ BOOL *pExists);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBodySize )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ long *pSize);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSTimestampLow )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ long *pTimestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSTimestampLow )( 
            IJMSStreamMessage * This,
            /* [in] */ long timestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSExpirationLow )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ long *pExpiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSExpirationLow )( 
            IJMSStreamMessage * This,
            /* [in] */ long expiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSTimestampHigh )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ long *pTimestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSTimestampHigh )( 
            IJMSStreamMessage * This,
            /* [in] */ long timestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSExpirationHigh )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ long *pExpiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSExpirationHigh )( 
            IJMSStreamMessage * This,
            /* [in] */ long expiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readByte )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ VARIANT *pParam);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeByte )( 
            IJMSStreamMessage * This,
            /* [in] */ VARIANT param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readBytes )( 
            IJMSStreamMessage * This,
            /* [in] */ int length,
            /* [retval][out] */ VARIANT *pParam);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeBytes )( 
            IJMSStreamMessage * This,
            /* [in] */ VARIANT param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeBytes2 )( 
            IJMSStreamMessage * This,
            /* [in] */ VARIANT param,
            /* [in] */ int offset,
            /* [in] */ int length);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readBoolean )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ BOOL *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeBoolean )( 
            IJMSStreamMessage * This,
            /* [in] */ BOOL param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readChar )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ VARIANT *pParam);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeChar )( 
            IJMSStreamMessage * This,
            /* [in] */ VARIANT param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readShort )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ short *pParam);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeShort )( 
            IJMSStreamMessage * This,
            /* [in] */ short param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readInt )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ int *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeInt )( 
            IJMSStreamMessage * This,
            /* [in] */ int param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeLong )( 
            IJMSStreamMessage * This,
            /* [in] */ long param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readLong )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ long *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readFloat )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ float *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeFloat )( 
            IJMSStreamMessage * This,
            /* [in] */ float param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readDouble )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ double *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeDouble )( 
            IJMSStreamMessage * This,
            /* [in] */ double param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readObject )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ VARIANT *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeObject )( 
            IJMSStreamMessage * This,
            /* [in] */ VARIANT param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *readString )( 
            IJMSStreamMessage * This,
            /* [retval][out] */ BSTR *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *writeString )( 
            IJMSStreamMessage * This,
            /* [in] */ BSTR param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *reset )( 
            IJMSStreamMessage * This);
        
        END_INTERFACE
    } IJMSStreamMessageVtbl;

    interface IJMSStreamMessage
    {
        CONST_VTBL struct IJMSStreamMessageVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSStreamMessage_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSStreamMessage_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSStreamMessage_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSStreamMessage_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSStreamMessage_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSStreamMessage_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSStreamMessage_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSStreamMessage_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSStreamMessage_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSStreamMessage_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSStreamMessage_acknowledge(This)	\
    (This)->lpVtbl -> acknowledge(This)

#define IJMSStreamMessage_clearBody(This)	\
    (This)->lpVtbl -> clearBody(This)

#define IJMSStreamMessage_getJMSDestination(This,ppDest)	\
    (This)->lpVtbl -> getJMSDestination(This,ppDest)

#define IJMSStreamMessage_setJMSDestination(This,pDest)	\
    (This)->lpVtbl -> setJMSDestination(This,pDest)

#define IJMSStreamMessage_getJMSDeliveryMode(This,pDelMode)	\
    (This)->lpVtbl -> getJMSDeliveryMode(This,pDelMode)

#define IJMSStreamMessage_setJMSDeliveryMode(This,pDelMode)	\
    (This)->lpVtbl -> setJMSDeliveryMode(This,pDelMode)

#define IJMSStreamMessage_getJMSMessageID(This,pMessageID)	\
    (This)->lpVtbl -> getJMSMessageID(This,pMessageID)

#define IJMSStreamMessage_setJMSMessageID(This,messageID)	\
    (This)->lpVtbl -> setJMSMessageID(This,messageID)

#define IJMSStreamMessage_getJMSTimestamp(This,pTimestamp)	\
    (This)->lpVtbl -> getJMSTimestamp(This,pTimestamp)

#define IJMSStreamMessage_setJMSTimestamp(This,timestamp)	\
    (This)->lpVtbl -> setJMSTimestamp(This,timestamp)

#define IJMSStreamMessage_getJMSExpiration(This,pExpiration)	\
    (This)->lpVtbl -> getJMSExpiration(This,pExpiration)

#define IJMSStreamMessage_setJMSExpiration(This,expiration)	\
    (This)->lpVtbl -> setJMSExpiration(This,expiration)

#define IJMSStreamMessage_getJMSRedelivered(This,pRedelivered)	\
    (This)->lpVtbl -> getJMSRedelivered(This,pRedelivered)

#define IJMSStreamMessage_setJMSRedelivered(This,redelivered)	\
    (This)->lpVtbl -> setJMSRedelivered(This,redelivered)

#define IJMSStreamMessage_getJMSPriority(This,pPriority)	\
    (This)->lpVtbl -> getJMSPriority(This,pPriority)

#define IJMSStreamMessage_setJMSPriority(This,priority)	\
    (This)->lpVtbl -> setJMSPriority(This,priority)

#define IJMSStreamMessage_getJMSReplyTo(This,destination)	\
    (This)->lpVtbl -> getJMSReplyTo(This,destination)

#define IJMSStreamMessage_setJMSReplyTo(This,destination)	\
    (This)->lpVtbl -> setJMSReplyTo(This,destination)

#define IJMSStreamMessage_getJMSCorrelationID(This,pCorrelationID)	\
    (This)->lpVtbl -> getJMSCorrelationID(This,pCorrelationID)

#define IJMSStreamMessage_setJMSCorrelationID(This,correlationID)	\
    (This)->lpVtbl -> setJMSCorrelationID(This,correlationID)

#define IJMSStreamMessage_getJMSCorrelationIDAsBytes(This,pCorrelationID)	\
    (This)->lpVtbl -> getJMSCorrelationIDAsBytes(This,pCorrelationID)

#define IJMSStreamMessage_setJMSCorrelationIDAsBytes(This,correlationID)	\
    (This)->lpVtbl -> setJMSCorrelationIDAsBytes(This,correlationID)

#define IJMSStreamMessage_getJMSType(This,pJMSType)	\
    (This)->lpVtbl -> getJMSType(This,pJMSType)

#define IJMSStreamMessage_setJMSType(This,JMSType)	\
    (This)->lpVtbl -> setJMSType(This,JMSType)

#define IJMSStreamMessage_getStringProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getStringProperty(This,propName,pProperty)

#define IJMSStreamMessage_setStringProperty(This,propName,property)	\
    (This)->lpVtbl -> setStringProperty(This,propName,property)

#define IJMSStreamMessage_getIntProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getIntProperty(This,propName,pProperty)

#define IJMSStreamMessage_setIntProperty(This,propName,property)	\
    (This)->lpVtbl -> setIntProperty(This,propName,property)

#define IJMSStreamMessage_getBooleanProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getBooleanProperty(This,propName,pProperty)

#define IJMSStreamMessage_setBooleanProperty(This,propName,property)	\
    (This)->lpVtbl -> setBooleanProperty(This,propName,property)

#define IJMSStreamMessage_getDoubleProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getDoubleProperty(This,propName,pProperty)

#define IJMSStreamMessage_setDoubleProperty(This,propName,property)	\
    (This)->lpVtbl -> setDoubleProperty(This,propName,property)

#define IJMSStreamMessage_getFloatProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getFloatProperty(This,propName,pProperty)

#define IJMSStreamMessage_setFloatProperty(This,propName,property)	\
    (This)->lpVtbl -> setFloatProperty(This,propName,property)

#define IJMSStreamMessage_getByteProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getByteProperty(This,propName,pProperty)

#define IJMSStreamMessage_setByteProperty(This,propName,property)	\
    (This)->lpVtbl -> setByteProperty(This,propName,property)

#define IJMSStreamMessage_getLongProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getLongProperty(This,propName,pProperty)

#define IJMSStreamMessage_setLongProperty(This,propName,property)	\
    (This)->lpVtbl -> setLongProperty(This,propName,property)

#define IJMSStreamMessage_getShortProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getShortProperty(This,propName,pProperty)

#define IJMSStreamMessage_setShortProperty(This,propName,property)	\
    (This)->lpVtbl -> setShortProperty(This,propName,property)

#define IJMSStreamMessage_getObjectProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getObjectProperty(This,propName,pProperty)

#define IJMSStreamMessage_setObjectProperty(This,propName,property)	\
    (This)->lpVtbl -> setObjectProperty(This,propName,property)

#define IJMSStreamMessage_clearProperties(This)	\
    (This)->lpVtbl -> clearProperties(This)

#define IJMSStreamMessage_getPropertyNames(This,pPropertyNames)	\
    (This)->lpVtbl -> getPropertyNames(This,pPropertyNames)

#define IJMSStreamMessage_propertyExists(This,propName,pExists)	\
    (This)->lpVtbl -> propertyExists(This,propName,pExists)

#define IJMSStreamMessage_getBodySize(This,pSize)	\
    (This)->lpVtbl -> getBodySize(This,pSize)

#define IJMSStreamMessage_getJMSTimestampLow(This,pTimestamp)	\
    (This)->lpVtbl -> getJMSTimestampLow(This,pTimestamp)

#define IJMSStreamMessage_setJMSTimestampLow(This,timestamp)	\
    (This)->lpVtbl -> setJMSTimestampLow(This,timestamp)

#define IJMSStreamMessage_getJMSExpirationLow(This,pExpiration)	\
    (This)->lpVtbl -> getJMSExpirationLow(This,pExpiration)

#define IJMSStreamMessage_setJMSExpirationLow(This,expiration)	\
    (This)->lpVtbl -> setJMSExpirationLow(This,expiration)

#define IJMSStreamMessage_getJMSTimestampHigh(This,pTimestamp)	\
    (This)->lpVtbl -> getJMSTimestampHigh(This,pTimestamp)

#define IJMSStreamMessage_setJMSTimestampHigh(This,timestamp)	\
    (This)->lpVtbl -> setJMSTimestampHigh(This,timestamp)

#define IJMSStreamMessage_getJMSExpirationHigh(This,pExpiration)	\
    (This)->lpVtbl -> getJMSExpirationHigh(This,pExpiration)

#define IJMSStreamMessage_setJMSExpirationHigh(This,expiration)	\
    (This)->lpVtbl -> setJMSExpirationHigh(This,expiration)


#define IJMSStreamMessage_readByte(This,pParam)	\
    (This)->lpVtbl -> readByte(This,pParam)

#define IJMSStreamMessage_writeByte(This,param)	\
    (This)->lpVtbl -> writeByte(This,param)

#define IJMSStreamMessage_readBytes(This,length,pParam)	\
    (This)->lpVtbl -> readBytes(This,length,pParam)

#define IJMSStreamMessage_writeBytes(This,param)	\
    (This)->lpVtbl -> writeBytes(This,param)

#define IJMSStreamMessage_writeBytes2(This,param,offset,length)	\
    (This)->lpVtbl -> writeBytes2(This,param,offset,length)

#define IJMSStreamMessage_readBoolean(This,param)	\
    (This)->lpVtbl -> readBoolean(This,param)

#define IJMSStreamMessage_writeBoolean(This,param)	\
    (This)->lpVtbl -> writeBoolean(This,param)

#define IJMSStreamMessage_readChar(This,pParam)	\
    (This)->lpVtbl -> readChar(This,pParam)

#define IJMSStreamMessage_writeChar(This,param)	\
    (This)->lpVtbl -> writeChar(This,param)

#define IJMSStreamMessage_readShort(This,pParam)	\
    (This)->lpVtbl -> readShort(This,pParam)

#define IJMSStreamMessage_writeShort(This,param)	\
    (This)->lpVtbl -> writeShort(This,param)

#define IJMSStreamMessage_readInt(This,param)	\
    (This)->lpVtbl -> readInt(This,param)

#define IJMSStreamMessage_writeInt(This,param)	\
    (This)->lpVtbl -> writeInt(This,param)

#define IJMSStreamMessage_writeLong(This,param)	\
    (This)->lpVtbl -> writeLong(This,param)

#define IJMSStreamMessage_readLong(This,param)	\
    (This)->lpVtbl -> readLong(This,param)

#define IJMSStreamMessage_readFloat(This,param)	\
    (This)->lpVtbl -> readFloat(This,param)

#define IJMSStreamMessage_writeFloat(This,param)	\
    (This)->lpVtbl -> writeFloat(This,param)

#define IJMSStreamMessage_readDouble(This,param)	\
    (This)->lpVtbl -> readDouble(This,param)

#define IJMSStreamMessage_writeDouble(This,param)	\
    (This)->lpVtbl -> writeDouble(This,param)

#define IJMSStreamMessage_readObject(This,param)	\
    (This)->lpVtbl -> readObject(This,param)

#define IJMSStreamMessage_writeObject(This,param)	\
    (This)->lpVtbl -> writeObject(This,param)

#define IJMSStreamMessage_readString(This,param)	\
    (This)->lpVtbl -> readString(This,param)

#define IJMSStreamMessage_writeString(This,param)	\
    (This)->lpVtbl -> writeString(This,param)

#define IJMSStreamMessage_reset(This)	\
    (This)->lpVtbl -> reset(This)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_readByte_Proxy( 
    IJMSStreamMessage * This,
    /* [retval][out] */ VARIANT *pParam);


void __RPC_STUB IJMSStreamMessage_readByte_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_writeByte_Proxy( 
    IJMSStreamMessage * This,
    /* [in] */ VARIANT param);


void __RPC_STUB IJMSStreamMessage_writeByte_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_readBytes_Proxy( 
    IJMSStreamMessage * This,
    /* [in] */ int length,
    /* [retval][out] */ VARIANT *pParam);


void __RPC_STUB IJMSStreamMessage_readBytes_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_writeBytes_Proxy( 
    IJMSStreamMessage * This,
    /* [in] */ VARIANT param);


void __RPC_STUB IJMSStreamMessage_writeBytes_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_writeBytes2_Proxy( 
    IJMSStreamMessage * This,
    /* [in] */ VARIANT param,
    /* [in] */ int offset,
    /* [in] */ int length);


void __RPC_STUB IJMSStreamMessage_writeBytes2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_readBoolean_Proxy( 
    IJMSStreamMessage * This,
    /* [retval][out] */ BOOL *param);


void __RPC_STUB IJMSStreamMessage_readBoolean_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_writeBoolean_Proxy( 
    IJMSStreamMessage * This,
    /* [in] */ BOOL param);


void __RPC_STUB IJMSStreamMessage_writeBoolean_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_readChar_Proxy( 
    IJMSStreamMessage * This,
    /* [retval][out] */ VARIANT *pParam);


void __RPC_STUB IJMSStreamMessage_readChar_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_writeChar_Proxy( 
    IJMSStreamMessage * This,
    /* [in] */ VARIANT param);


void __RPC_STUB IJMSStreamMessage_writeChar_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_readShort_Proxy( 
    IJMSStreamMessage * This,
    /* [retval][out] */ short *pParam);


void __RPC_STUB IJMSStreamMessage_readShort_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_writeShort_Proxy( 
    IJMSStreamMessage * This,
    /* [in] */ short param);


void __RPC_STUB IJMSStreamMessage_writeShort_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_readInt_Proxy( 
    IJMSStreamMessage * This,
    /* [retval][out] */ int *param);


void __RPC_STUB IJMSStreamMessage_readInt_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_writeInt_Proxy( 
    IJMSStreamMessage * This,
    /* [in] */ int param);


void __RPC_STUB IJMSStreamMessage_writeInt_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_writeLong_Proxy( 
    IJMSStreamMessage * This,
    /* [in] */ long param);


void __RPC_STUB IJMSStreamMessage_writeLong_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_readLong_Proxy( 
    IJMSStreamMessage * This,
    /* [retval][out] */ long *param);


void __RPC_STUB IJMSStreamMessage_readLong_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_readFloat_Proxy( 
    IJMSStreamMessage * This,
    /* [retval][out] */ float *param);


void __RPC_STUB IJMSStreamMessage_readFloat_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_writeFloat_Proxy( 
    IJMSStreamMessage * This,
    /* [in] */ float param);


void __RPC_STUB IJMSStreamMessage_writeFloat_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_readDouble_Proxy( 
    IJMSStreamMessage * This,
    /* [retval][out] */ double *param);


void __RPC_STUB IJMSStreamMessage_readDouble_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_writeDouble_Proxy( 
    IJMSStreamMessage * This,
    /* [in] */ double param);


void __RPC_STUB IJMSStreamMessage_writeDouble_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_readObject_Proxy( 
    IJMSStreamMessage * This,
    /* [retval][out] */ VARIANT *param);


void __RPC_STUB IJMSStreamMessage_readObject_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_writeObject_Proxy( 
    IJMSStreamMessage * This,
    /* [in] */ VARIANT param);


void __RPC_STUB IJMSStreamMessage_writeObject_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_readString_Proxy( 
    IJMSStreamMessage * This,
    /* [retval][out] */ BSTR *param);


void __RPC_STUB IJMSStreamMessage_readString_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_writeString_Proxy( 
    IJMSStreamMessage * This,
    /* [in] */ BSTR param);


void __RPC_STUB IJMSStreamMessage_writeString_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSStreamMessage_reset_Proxy( 
    IJMSStreamMessage * This);


void __RPC_STUB IJMSStreamMessage_reset_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSStreamMessage_INTERFACE_DEFINED__ */


#ifndef __IJMSTextMessage_INTERFACE_DEFINED__
#define __IJMSTextMessage_INTERFACE_DEFINED__

/* interface IJMSTextMessage */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSTextMessage;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("84D7498D-1083-4FF1-8A9B-4C8DA93784A7")
    IJMSTextMessage : public IJMSMessage
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getText( 
            /* [retval][out] */ BSTR *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setText( 
            /* [in] */ BSTR param) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSTextMessageVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSTextMessage * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSTextMessage * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSTextMessage * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSTextMessage * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSTextMessage * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSTextMessage * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSTextMessage * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSTextMessage * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSTextMessage * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSTextMessage * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *acknowledge )( 
            IJMSTextMessage * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clearBody )( 
            IJMSTextMessage * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSDestination )( 
            IJMSTextMessage * This,
            /* [retval][out] */ IJMSDestination **ppDest);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSDestination )( 
            IJMSTextMessage * This,
            /* [in] */ IJMSDestination *pDest);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSDeliveryMode )( 
            IJMSTextMessage * This,
            /* [retval][out] */ enum JMSDeliveryMode *pDelMode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSDeliveryMode )( 
            IJMSTextMessage * This,
            /* [in] */ enum JMSDeliveryMode pDelMode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSMessageID )( 
            IJMSTextMessage * This,
            /* [retval][out] */ BSTR *pMessageID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSMessageID )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR messageID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSTimestamp )( 
            IJMSTextMessage * This,
            /* [retval][out] */ long *pTimestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSTimestamp )( 
            IJMSTextMessage * This,
            /* [in] */ long timestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSExpiration )( 
            IJMSTextMessage * This,
            /* [retval][out] */ long *pExpiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSExpiration )( 
            IJMSTextMessage * This,
            /* [in] */ long expiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSRedelivered )( 
            IJMSTextMessage * This,
            /* [retval][out] */ BOOL *pRedelivered);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSRedelivered )( 
            IJMSTextMessage * This,
            /* [in] */ BOOL redelivered);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSPriority )( 
            IJMSTextMessage * This,
            /* [retval][out] */ int *pPriority);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSPriority )( 
            IJMSTextMessage * This,
            /* [in] */ int priority);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSReplyTo )( 
            IJMSTextMessage * This,
            /* [retval][out] */ IJMSDestination **destination);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSReplyTo )( 
            IJMSTextMessage * This,
            /* [in] */ IJMSDestination *destination);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSCorrelationID )( 
            IJMSTextMessage * This,
            /* [retval][out] */ BSTR *pCorrelationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSCorrelationID )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR correlationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSCorrelationIDAsBytes )( 
            IJMSTextMessage * This,
            /* [retval][out] */ VARIANT *pCorrelationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSCorrelationIDAsBytes )( 
            IJMSTextMessage * This,
            /* [in] */ VARIANT correlationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSType )( 
            IJMSTextMessage * This,
            /* [retval][out] */ BSTR *pJMSType);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSType )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR JMSType);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getStringProperty )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ BSTR *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setStringProperty )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ BSTR property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getIntProperty )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ int *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setIntProperty )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ int property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBooleanProperty )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ BOOL *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setBooleanProperty )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ BOOL property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDoubleProperty )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ double *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setDoubleProperty )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ double property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getFloatProperty )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ float *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setFloatProperty )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ float property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getByteProperty )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ VARIANT *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setByteProperty )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ VARIANT property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getLongProperty )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ long *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setLongProperty )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ long property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getShortProperty )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ short *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setShortProperty )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ short property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getObjectProperty )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ VARIANT *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setObjectProperty )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ VARIANT property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clearProperties )( 
            IJMSTextMessage * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getPropertyNames )( 
            IJMSTextMessage * This,
            /* [retval][out] */ IJMSEnumeration **pPropertyNames);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *propertyExists )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ BOOL *pExists);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBodySize )( 
            IJMSTextMessage * This,
            /* [retval][out] */ long *pSize);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSTimestampLow )( 
            IJMSTextMessage * This,
            /* [retval][out] */ long *pTimestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSTimestampLow )( 
            IJMSTextMessage * This,
            /* [in] */ long timestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSExpirationLow )( 
            IJMSTextMessage * This,
            /* [retval][out] */ long *pExpiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSExpirationLow )( 
            IJMSTextMessage * This,
            /* [in] */ long expiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSTimestampHigh )( 
            IJMSTextMessage * This,
            /* [retval][out] */ long *pTimestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSTimestampHigh )( 
            IJMSTextMessage * This,
            /* [in] */ long timestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSExpirationHigh )( 
            IJMSTextMessage * This,
            /* [retval][out] */ long *pExpiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSExpirationHigh )( 
            IJMSTextMessage * This,
            /* [in] */ long expiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getText )( 
            IJMSTextMessage * This,
            /* [retval][out] */ BSTR *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setText )( 
            IJMSTextMessage * This,
            /* [in] */ BSTR param);
        
        END_INTERFACE
    } IJMSTextMessageVtbl;

    interface IJMSTextMessage
    {
        CONST_VTBL struct IJMSTextMessageVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSTextMessage_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSTextMessage_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSTextMessage_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSTextMessage_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSTextMessage_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSTextMessage_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSTextMessage_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSTextMessage_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSTextMessage_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSTextMessage_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSTextMessage_acknowledge(This)	\
    (This)->lpVtbl -> acknowledge(This)

#define IJMSTextMessage_clearBody(This)	\
    (This)->lpVtbl -> clearBody(This)

#define IJMSTextMessage_getJMSDestination(This,ppDest)	\
    (This)->lpVtbl -> getJMSDestination(This,ppDest)

#define IJMSTextMessage_setJMSDestination(This,pDest)	\
    (This)->lpVtbl -> setJMSDestination(This,pDest)

#define IJMSTextMessage_getJMSDeliveryMode(This,pDelMode)	\
    (This)->lpVtbl -> getJMSDeliveryMode(This,pDelMode)

#define IJMSTextMessage_setJMSDeliveryMode(This,pDelMode)	\
    (This)->lpVtbl -> setJMSDeliveryMode(This,pDelMode)

#define IJMSTextMessage_getJMSMessageID(This,pMessageID)	\
    (This)->lpVtbl -> getJMSMessageID(This,pMessageID)

#define IJMSTextMessage_setJMSMessageID(This,messageID)	\
    (This)->lpVtbl -> setJMSMessageID(This,messageID)

#define IJMSTextMessage_getJMSTimestamp(This,pTimestamp)	\
    (This)->lpVtbl -> getJMSTimestamp(This,pTimestamp)

#define IJMSTextMessage_setJMSTimestamp(This,timestamp)	\
    (This)->lpVtbl -> setJMSTimestamp(This,timestamp)

#define IJMSTextMessage_getJMSExpiration(This,pExpiration)	\
    (This)->lpVtbl -> getJMSExpiration(This,pExpiration)

#define IJMSTextMessage_setJMSExpiration(This,expiration)	\
    (This)->lpVtbl -> setJMSExpiration(This,expiration)

#define IJMSTextMessage_getJMSRedelivered(This,pRedelivered)	\
    (This)->lpVtbl -> getJMSRedelivered(This,pRedelivered)

#define IJMSTextMessage_setJMSRedelivered(This,redelivered)	\
    (This)->lpVtbl -> setJMSRedelivered(This,redelivered)

#define IJMSTextMessage_getJMSPriority(This,pPriority)	\
    (This)->lpVtbl -> getJMSPriority(This,pPriority)

#define IJMSTextMessage_setJMSPriority(This,priority)	\
    (This)->lpVtbl -> setJMSPriority(This,priority)

#define IJMSTextMessage_getJMSReplyTo(This,destination)	\
    (This)->lpVtbl -> getJMSReplyTo(This,destination)

#define IJMSTextMessage_setJMSReplyTo(This,destination)	\
    (This)->lpVtbl -> setJMSReplyTo(This,destination)

#define IJMSTextMessage_getJMSCorrelationID(This,pCorrelationID)	\
    (This)->lpVtbl -> getJMSCorrelationID(This,pCorrelationID)

#define IJMSTextMessage_setJMSCorrelationID(This,correlationID)	\
    (This)->lpVtbl -> setJMSCorrelationID(This,correlationID)

#define IJMSTextMessage_getJMSCorrelationIDAsBytes(This,pCorrelationID)	\
    (This)->lpVtbl -> getJMSCorrelationIDAsBytes(This,pCorrelationID)

#define IJMSTextMessage_setJMSCorrelationIDAsBytes(This,correlationID)	\
    (This)->lpVtbl -> setJMSCorrelationIDAsBytes(This,correlationID)

#define IJMSTextMessage_getJMSType(This,pJMSType)	\
    (This)->lpVtbl -> getJMSType(This,pJMSType)

#define IJMSTextMessage_setJMSType(This,JMSType)	\
    (This)->lpVtbl -> setJMSType(This,JMSType)

#define IJMSTextMessage_getStringProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getStringProperty(This,propName,pProperty)

#define IJMSTextMessage_setStringProperty(This,propName,property)	\
    (This)->lpVtbl -> setStringProperty(This,propName,property)

#define IJMSTextMessage_getIntProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getIntProperty(This,propName,pProperty)

#define IJMSTextMessage_setIntProperty(This,propName,property)	\
    (This)->lpVtbl -> setIntProperty(This,propName,property)

#define IJMSTextMessage_getBooleanProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getBooleanProperty(This,propName,pProperty)

#define IJMSTextMessage_setBooleanProperty(This,propName,property)	\
    (This)->lpVtbl -> setBooleanProperty(This,propName,property)

#define IJMSTextMessage_getDoubleProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getDoubleProperty(This,propName,pProperty)

#define IJMSTextMessage_setDoubleProperty(This,propName,property)	\
    (This)->lpVtbl -> setDoubleProperty(This,propName,property)

#define IJMSTextMessage_getFloatProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getFloatProperty(This,propName,pProperty)

#define IJMSTextMessage_setFloatProperty(This,propName,property)	\
    (This)->lpVtbl -> setFloatProperty(This,propName,property)

#define IJMSTextMessage_getByteProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getByteProperty(This,propName,pProperty)

#define IJMSTextMessage_setByteProperty(This,propName,property)	\
    (This)->lpVtbl -> setByteProperty(This,propName,property)

#define IJMSTextMessage_getLongProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getLongProperty(This,propName,pProperty)

#define IJMSTextMessage_setLongProperty(This,propName,property)	\
    (This)->lpVtbl -> setLongProperty(This,propName,property)

#define IJMSTextMessage_getShortProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getShortProperty(This,propName,pProperty)

#define IJMSTextMessage_setShortProperty(This,propName,property)	\
    (This)->lpVtbl -> setShortProperty(This,propName,property)

#define IJMSTextMessage_getObjectProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getObjectProperty(This,propName,pProperty)

#define IJMSTextMessage_setObjectProperty(This,propName,property)	\
    (This)->lpVtbl -> setObjectProperty(This,propName,property)

#define IJMSTextMessage_clearProperties(This)	\
    (This)->lpVtbl -> clearProperties(This)

#define IJMSTextMessage_getPropertyNames(This,pPropertyNames)	\
    (This)->lpVtbl -> getPropertyNames(This,pPropertyNames)

#define IJMSTextMessage_propertyExists(This,propName,pExists)	\
    (This)->lpVtbl -> propertyExists(This,propName,pExists)

#define IJMSTextMessage_getBodySize(This,pSize)	\
    (This)->lpVtbl -> getBodySize(This,pSize)

#define IJMSTextMessage_getJMSTimestampLow(This,pTimestamp)	\
    (This)->lpVtbl -> getJMSTimestampLow(This,pTimestamp)

#define IJMSTextMessage_setJMSTimestampLow(This,timestamp)	\
    (This)->lpVtbl -> setJMSTimestampLow(This,timestamp)

#define IJMSTextMessage_getJMSExpirationLow(This,pExpiration)	\
    (This)->lpVtbl -> getJMSExpirationLow(This,pExpiration)

#define IJMSTextMessage_setJMSExpirationLow(This,expiration)	\
    (This)->lpVtbl -> setJMSExpirationLow(This,expiration)

#define IJMSTextMessage_getJMSTimestampHigh(This,pTimestamp)	\
    (This)->lpVtbl -> getJMSTimestampHigh(This,pTimestamp)

#define IJMSTextMessage_setJMSTimestampHigh(This,timestamp)	\
    (This)->lpVtbl -> setJMSTimestampHigh(This,timestamp)

#define IJMSTextMessage_getJMSExpirationHigh(This,pExpiration)	\
    (This)->lpVtbl -> getJMSExpirationHigh(This,pExpiration)

#define IJMSTextMessage_setJMSExpirationHigh(This,expiration)	\
    (This)->lpVtbl -> setJMSExpirationHigh(This,expiration)


#define IJMSTextMessage_getText(This,param)	\
    (This)->lpVtbl -> getText(This,param)

#define IJMSTextMessage_setText(This,param)	\
    (This)->lpVtbl -> setText(This,param)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTextMessage_getText_Proxy( 
    IJMSTextMessage * This,
    /* [retval][out] */ BSTR *param);


void __RPC_STUB IJMSTextMessage_getText_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTextMessage_setText_Proxy( 
    IJMSTextMessage * This,
    /* [in] */ BSTR param);


void __RPC_STUB IJMSTextMessage_setText_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSTextMessage_INTERFACE_DEFINED__ */


#ifndef __IJMSConnection_INTERFACE_DEFINED__
#define __IJMSConnection_INTERFACE_DEFINED__

/* interface IJMSConnection */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSConnection;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("3A85B68C-CECB-4732-B61C-E487C2BECF0A")
    IJMSConnection : public IJMSObject
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE start( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE stop( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE close( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getClientID( 
            /* [retval][out] */ BSTR *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setClientID( 
            /* [in] */ BSTR param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getMetaData( 
            /* [retval][out] */ IJMSConnectionMetaData **ppMetaData) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setExceptionListener( 
            /* [in] */ IUnknown *pIUnknown) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setExceptionListener2( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE resetExceptionListener( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setPingInterval( 
            /* [in] */ long interval) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getBrokerURL( 
            /* [retval][out] */ BSTR *url) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setConnectionStateChangeListener( 
            /* [in] */ IUnknown *pIUnknown) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getConnectionStateChangeListener( 
            /* [retval][out] */ IJMSConnectionStateChangeListener **pIUnknown) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE isFaultTolerant( 
            /* [retval][out] */ BOOL *ft) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getConnectionState( 
            /* [retval][out] */ int *state) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getBrokerReconnectURLsSize( 
            /* [retval][out] */ int *size) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getBrokerReconnectURLsItem( 
            /* [in] */ int item,
            /* [retval][out] */ BSTR *url) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getStandbyBrokerReconnectURLsSize( 
            /* [retval][out] */ int *size) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getStandbyBrokerReconnectURLsItem( 
            /* [in] */ int item,
            /* [retval][out] */ BSTR *url) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE registerConnectionStateChangeListener( void) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSConnectionVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSConnection * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSConnection * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSConnection * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSConnection * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSConnection * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSConnection * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSConnection * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSConnection * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSConnection * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSConnection * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *start )( 
            IJMSConnection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *stop )( 
            IJMSConnection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *close )( 
            IJMSConnection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getClientID )( 
            IJMSConnection * This,
            /* [retval][out] */ BSTR *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setClientID )( 
            IJMSConnection * This,
            /* [in] */ BSTR param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getMetaData )( 
            IJMSConnection * This,
            /* [retval][out] */ IJMSConnectionMetaData **ppMetaData);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setExceptionListener )( 
            IJMSConnection * This,
            /* [in] */ IUnknown *pIUnknown);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setExceptionListener2 )( 
            IJMSConnection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *resetExceptionListener )( 
            IJMSConnection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setPingInterval )( 
            IJMSConnection * This,
            /* [in] */ long interval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBrokerURL )( 
            IJMSConnection * This,
            /* [retval][out] */ BSTR *url);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setConnectionStateChangeListener )( 
            IJMSConnection * This,
            /* [in] */ IUnknown *pIUnknown);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getConnectionStateChangeListener )( 
            IJMSConnection * This,
            /* [retval][out] */ IJMSConnectionStateChangeListener **pIUnknown);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *isFaultTolerant )( 
            IJMSConnection * This,
            /* [retval][out] */ BOOL *ft);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getConnectionState )( 
            IJMSConnection * This,
            /* [retval][out] */ int *state);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBrokerReconnectURLsSize )( 
            IJMSConnection * This,
            /* [retval][out] */ int *size);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBrokerReconnectURLsItem )( 
            IJMSConnection * This,
            /* [in] */ int item,
            /* [retval][out] */ BSTR *url);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getStandbyBrokerReconnectURLsSize )( 
            IJMSConnection * This,
            /* [retval][out] */ int *size);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getStandbyBrokerReconnectURLsItem )( 
            IJMSConnection * This,
            /* [in] */ int item,
            /* [retval][out] */ BSTR *url);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *registerConnectionStateChangeListener )( 
            IJMSConnection * This);
        
        END_INTERFACE
    } IJMSConnectionVtbl;

    interface IJMSConnection
    {
        CONST_VTBL struct IJMSConnectionVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSConnection_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSConnection_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSConnection_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSConnection_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSConnection_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSConnection_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSConnection_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSConnection_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSConnection_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSConnection_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSConnection_start(This)	\
    (This)->lpVtbl -> start(This)

#define IJMSConnection_stop(This)	\
    (This)->lpVtbl -> stop(This)

#define IJMSConnection_close(This)	\
    (This)->lpVtbl -> close(This)

#define IJMSConnection_getClientID(This,param)	\
    (This)->lpVtbl -> getClientID(This,param)

#define IJMSConnection_setClientID(This,param)	\
    (This)->lpVtbl -> setClientID(This,param)

#define IJMSConnection_getMetaData(This,ppMetaData)	\
    (This)->lpVtbl -> getMetaData(This,ppMetaData)

#define IJMSConnection_setExceptionListener(This,pIUnknown)	\
    (This)->lpVtbl -> setExceptionListener(This,pIUnknown)

#define IJMSConnection_setExceptionListener2(This)	\
    (This)->lpVtbl -> setExceptionListener2(This)

#define IJMSConnection_resetExceptionListener(This)	\
    (This)->lpVtbl -> resetExceptionListener(This)

#define IJMSConnection_setPingInterval(This,interval)	\
    (This)->lpVtbl -> setPingInterval(This,interval)

#define IJMSConnection_getBrokerURL(This,url)	\
    (This)->lpVtbl -> getBrokerURL(This,url)

#define IJMSConnection_setConnectionStateChangeListener(This,pIUnknown)	\
    (This)->lpVtbl -> setConnectionStateChangeListener(This,pIUnknown)

#define IJMSConnection_getConnectionStateChangeListener(This,pIUnknown)	\
    (This)->lpVtbl -> getConnectionStateChangeListener(This,pIUnknown)

#define IJMSConnection_isFaultTolerant(This,ft)	\
    (This)->lpVtbl -> isFaultTolerant(This,ft)

#define IJMSConnection_getConnectionState(This,state)	\
    (This)->lpVtbl -> getConnectionState(This,state)

#define IJMSConnection_getBrokerReconnectURLsSize(This,size)	\
    (This)->lpVtbl -> getBrokerReconnectURLsSize(This,size)

#define IJMSConnection_getBrokerReconnectURLsItem(This,item,url)	\
    (This)->lpVtbl -> getBrokerReconnectURLsItem(This,item,url)

#define IJMSConnection_getStandbyBrokerReconnectURLsSize(This,size)	\
    (This)->lpVtbl -> getStandbyBrokerReconnectURLsSize(This,size)

#define IJMSConnection_getStandbyBrokerReconnectURLsItem(This,item,url)	\
    (This)->lpVtbl -> getStandbyBrokerReconnectURLsItem(This,item,url)

#define IJMSConnection_registerConnectionStateChangeListener(This)	\
    (This)->lpVtbl -> registerConnectionStateChangeListener(This)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_start_Proxy( 
    IJMSConnection * This);


void __RPC_STUB IJMSConnection_start_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_stop_Proxy( 
    IJMSConnection * This);


void __RPC_STUB IJMSConnection_stop_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_close_Proxy( 
    IJMSConnection * This);


void __RPC_STUB IJMSConnection_close_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_getClientID_Proxy( 
    IJMSConnection * This,
    /* [retval][out] */ BSTR *param);


void __RPC_STUB IJMSConnection_getClientID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_setClientID_Proxy( 
    IJMSConnection * This,
    /* [in] */ BSTR param);


void __RPC_STUB IJMSConnection_setClientID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_getMetaData_Proxy( 
    IJMSConnection * This,
    /* [retval][out] */ IJMSConnectionMetaData **ppMetaData);


void __RPC_STUB IJMSConnection_getMetaData_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_setExceptionListener_Proxy( 
    IJMSConnection * This,
    /* [in] */ IUnknown *pIUnknown);


void __RPC_STUB IJMSConnection_setExceptionListener_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_setExceptionListener2_Proxy( 
    IJMSConnection * This);


void __RPC_STUB IJMSConnection_setExceptionListener2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_resetExceptionListener_Proxy( 
    IJMSConnection * This);


void __RPC_STUB IJMSConnection_resetExceptionListener_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_setPingInterval_Proxy( 
    IJMSConnection * This,
    /* [in] */ long interval);


void __RPC_STUB IJMSConnection_setPingInterval_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_getBrokerURL_Proxy( 
    IJMSConnection * This,
    /* [retval][out] */ BSTR *url);


void __RPC_STUB IJMSConnection_getBrokerURL_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_setConnectionStateChangeListener_Proxy( 
    IJMSConnection * This,
    /* [in] */ IUnknown *pIUnknown);


void __RPC_STUB IJMSConnection_setConnectionStateChangeListener_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_getConnectionStateChangeListener_Proxy( 
    IJMSConnection * This,
    /* [retval][out] */ IJMSConnectionStateChangeListener **pIUnknown);


void __RPC_STUB IJMSConnection_getConnectionStateChangeListener_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_isFaultTolerant_Proxy( 
    IJMSConnection * This,
    /* [retval][out] */ BOOL *ft);


void __RPC_STUB IJMSConnection_isFaultTolerant_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_getConnectionState_Proxy( 
    IJMSConnection * This,
    /* [retval][out] */ int *state);


void __RPC_STUB IJMSConnection_getConnectionState_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_getBrokerReconnectURLsSize_Proxy( 
    IJMSConnection * This,
    /* [retval][out] */ int *size);


void __RPC_STUB IJMSConnection_getBrokerReconnectURLsSize_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_getBrokerReconnectURLsItem_Proxy( 
    IJMSConnection * This,
    /* [in] */ int item,
    /* [retval][out] */ BSTR *url);


void __RPC_STUB IJMSConnection_getBrokerReconnectURLsItem_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_getStandbyBrokerReconnectURLsSize_Proxy( 
    IJMSConnection * This,
    /* [retval][out] */ int *size);


void __RPC_STUB IJMSConnection_getStandbyBrokerReconnectURLsSize_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_getStandbyBrokerReconnectURLsItem_Proxy( 
    IJMSConnection * This,
    /* [in] */ int item,
    /* [retval][out] */ BSTR *url);


void __RPC_STUB IJMSConnection_getStandbyBrokerReconnectURLsItem_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnection_registerConnectionStateChangeListener_Proxy( 
    IJMSConnection * This);


void __RPC_STUB IJMSConnection_registerConnectionStateChangeListener_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSConnection_INTERFACE_DEFINED__ */


#ifndef __IJMSQueueConnection_INTERFACE_DEFINED__
#define __IJMSQueueConnection_INTERFACE_DEFINED__

/* interface IJMSQueueConnection */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSQueueConnection;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("E2ACC010-8DB7-4F4D-9D83-C39EE9BAC48F")
    IJMSQueueConnection : public IJMSConnection
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createQueueSession( 
            /* [in] */ BOOL transactedVar,
            /* [in] */ int acknowledgeMode,
            /* [retval][out] */ IJMSQueueSession **ppSess) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSQueueConnectionVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSQueueConnection * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSQueueConnection * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSQueueConnection * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSQueueConnection * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSQueueConnection * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSQueueConnection * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSQueueConnection * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSQueueConnection * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSQueueConnection * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSQueueConnection * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *start )( 
            IJMSQueueConnection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *stop )( 
            IJMSQueueConnection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *close )( 
            IJMSQueueConnection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getClientID )( 
            IJMSQueueConnection * This,
            /* [retval][out] */ BSTR *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setClientID )( 
            IJMSQueueConnection * This,
            /* [in] */ BSTR param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getMetaData )( 
            IJMSQueueConnection * This,
            /* [retval][out] */ IJMSConnectionMetaData **ppMetaData);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setExceptionListener )( 
            IJMSQueueConnection * This,
            /* [in] */ IUnknown *pIUnknown);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setExceptionListener2 )( 
            IJMSQueueConnection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *resetExceptionListener )( 
            IJMSQueueConnection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setPingInterval )( 
            IJMSQueueConnection * This,
            /* [in] */ long interval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBrokerURL )( 
            IJMSQueueConnection * This,
            /* [retval][out] */ BSTR *url);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setConnectionStateChangeListener )( 
            IJMSQueueConnection * This,
            /* [in] */ IUnknown *pIUnknown);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getConnectionStateChangeListener )( 
            IJMSQueueConnection * This,
            /* [retval][out] */ IJMSConnectionStateChangeListener **pIUnknown);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *isFaultTolerant )( 
            IJMSQueueConnection * This,
            /* [retval][out] */ BOOL *ft);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getConnectionState )( 
            IJMSQueueConnection * This,
            /* [retval][out] */ int *state);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBrokerReconnectURLsSize )( 
            IJMSQueueConnection * This,
            /* [retval][out] */ int *size);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBrokerReconnectURLsItem )( 
            IJMSQueueConnection * This,
            /* [in] */ int item,
            /* [retval][out] */ BSTR *url);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getStandbyBrokerReconnectURLsSize )( 
            IJMSQueueConnection * This,
            /* [retval][out] */ int *size);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getStandbyBrokerReconnectURLsItem )( 
            IJMSQueueConnection * This,
            /* [in] */ int item,
            /* [retval][out] */ BSTR *url);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *registerConnectionStateChangeListener )( 
            IJMSQueueConnection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createQueueSession )( 
            IJMSQueueConnection * This,
            /* [in] */ BOOL transactedVar,
            /* [in] */ int acknowledgeMode,
            /* [retval][out] */ IJMSQueueSession **ppSess);
        
        END_INTERFACE
    } IJMSQueueConnectionVtbl;

    interface IJMSQueueConnection
    {
        CONST_VTBL struct IJMSQueueConnectionVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSQueueConnection_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSQueueConnection_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSQueueConnection_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSQueueConnection_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSQueueConnection_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSQueueConnection_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSQueueConnection_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSQueueConnection_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSQueueConnection_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSQueueConnection_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSQueueConnection_start(This)	\
    (This)->lpVtbl -> start(This)

#define IJMSQueueConnection_stop(This)	\
    (This)->lpVtbl -> stop(This)

#define IJMSQueueConnection_close(This)	\
    (This)->lpVtbl -> close(This)

#define IJMSQueueConnection_getClientID(This,param)	\
    (This)->lpVtbl -> getClientID(This,param)

#define IJMSQueueConnection_setClientID(This,param)	\
    (This)->lpVtbl -> setClientID(This,param)

#define IJMSQueueConnection_getMetaData(This,ppMetaData)	\
    (This)->lpVtbl -> getMetaData(This,ppMetaData)

#define IJMSQueueConnection_setExceptionListener(This,pIUnknown)	\
    (This)->lpVtbl -> setExceptionListener(This,pIUnknown)

#define IJMSQueueConnection_setExceptionListener2(This)	\
    (This)->lpVtbl -> setExceptionListener2(This)

#define IJMSQueueConnection_resetExceptionListener(This)	\
    (This)->lpVtbl -> resetExceptionListener(This)

#define IJMSQueueConnection_setPingInterval(This,interval)	\
    (This)->lpVtbl -> setPingInterval(This,interval)

#define IJMSQueueConnection_getBrokerURL(This,url)	\
    (This)->lpVtbl -> getBrokerURL(This,url)

#define IJMSQueueConnection_setConnectionStateChangeListener(This,pIUnknown)	\
    (This)->lpVtbl -> setConnectionStateChangeListener(This,pIUnknown)

#define IJMSQueueConnection_getConnectionStateChangeListener(This,pIUnknown)	\
    (This)->lpVtbl -> getConnectionStateChangeListener(This,pIUnknown)

#define IJMSQueueConnection_isFaultTolerant(This,ft)	\
    (This)->lpVtbl -> isFaultTolerant(This,ft)

#define IJMSQueueConnection_getConnectionState(This,state)	\
    (This)->lpVtbl -> getConnectionState(This,state)

#define IJMSQueueConnection_getBrokerReconnectURLsSize(This,size)	\
    (This)->lpVtbl -> getBrokerReconnectURLsSize(This,size)

#define IJMSQueueConnection_getBrokerReconnectURLsItem(This,item,url)	\
    (This)->lpVtbl -> getBrokerReconnectURLsItem(This,item,url)

#define IJMSQueueConnection_getStandbyBrokerReconnectURLsSize(This,size)	\
    (This)->lpVtbl -> getStandbyBrokerReconnectURLsSize(This,size)

#define IJMSQueueConnection_getStandbyBrokerReconnectURLsItem(This,item,url)	\
    (This)->lpVtbl -> getStandbyBrokerReconnectURLsItem(This,item,url)

#define IJMSQueueConnection_registerConnectionStateChangeListener(This)	\
    (This)->lpVtbl -> registerConnectionStateChangeListener(This)


#define IJMSQueueConnection_createQueueSession(This,transactedVar,acknowledgeMode,ppSess)	\
    (This)->lpVtbl -> createQueueSession(This,transactedVar,acknowledgeMode,ppSess)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueConnection_createQueueSession_Proxy( 
    IJMSQueueConnection * This,
    /* [in] */ BOOL transactedVar,
    /* [in] */ int acknowledgeMode,
    /* [retval][out] */ IJMSQueueSession **ppSess);


void __RPC_STUB IJMSQueueConnection_createQueueSession_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSQueueConnection_INTERFACE_DEFINED__ */


#ifndef __IJMSTopicConnection_INTERFACE_DEFINED__
#define __IJMSTopicConnection_INTERFACE_DEFINED__

/* interface IJMSTopicConnection */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSTopicConnection;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("E9C84791-034F-44CC-8A4F-45F8ACA83809")
    IJMSTopicConnection : public IJMSConnection
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createTopicSession( 
            /* [in] */ BOOL transactedVar,
            /* [in] */ int acknowledgeMode,
            /* [retval][out] */ IJMSTopicSession **ppSess) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSTopicConnectionVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSTopicConnection * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSTopicConnection * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSTopicConnection * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSTopicConnection * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSTopicConnection * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSTopicConnection * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSTopicConnection * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSTopicConnection * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSTopicConnection * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSTopicConnection * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *start )( 
            IJMSTopicConnection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *stop )( 
            IJMSTopicConnection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *close )( 
            IJMSTopicConnection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getClientID )( 
            IJMSTopicConnection * This,
            /* [retval][out] */ BSTR *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setClientID )( 
            IJMSTopicConnection * This,
            /* [in] */ BSTR param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getMetaData )( 
            IJMSTopicConnection * This,
            /* [retval][out] */ IJMSConnectionMetaData **ppMetaData);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setExceptionListener )( 
            IJMSTopicConnection * This,
            /* [in] */ IUnknown *pIUnknown);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setExceptionListener2 )( 
            IJMSTopicConnection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *resetExceptionListener )( 
            IJMSTopicConnection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setPingInterval )( 
            IJMSTopicConnection * This,
            /* [in] */ long interval);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBrokerURL )( 
            IJMSTopicConnection * This,
            /* [retval][out] */ BSTR *url);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setConnectionStateChangeListener )( 
            IJMSTopicConnection * This,
            /* [in] */ IUnknown *pIUnknown);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getConnectionStateChangeListener )( 
            IJMSTopicConnection * This,
            /* [retval][out] */ IJMSConnectionStateChangeListener **pIUnknown);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *isFaultTolerant )( 
            IJMSTopicConnection * This,
            /* [retval][out] */ BOOL *ft);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getConnectionState )( 
            IJMSTopicConnection * This,
            /* [retval][out] */ int *state);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBrokerReconnectURLsSize )( 
            IJMSTopicConnection * This,
            /* [retval][out] */ int *size);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBrokerReconnectURLsItem )( 
            IJMSTopicConnection * This,
            /* [in] */ int item,
            /* [retval][out] */ BSTR *url);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getStandbyBrokerReconnectURLsSize )( 
            IJMSTopicConnection * This,
            /* [retval][out] */ int *size);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getStandbyBrokerReconnectURLsItem )( 
            IJMSTopicConnection * This,
            /* [in] */ int item,
            /* [retval][out] */ BSTR *url);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *registerConnectionStateChangeListener )( 
            IJMSTopicConnection * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createTopicSession )( 
            IJMSTopicConnection * This,
            /* [in] */ BOOL transactedVar,
            /* [in] */ int acknowledgeMode,
            /* [retval][out] */ IJMSTopicSession **ppSess);
        
        END_INTERFACE
    } IJMSTopicConnectionVtbl;

    interface IJMSTopicConnection
    {
        CONST_VTBL struct IJMSTopicConnectionVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSTopicConnection_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSTopicConnection_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSTopicConnection_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSTopicConnection_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSTopicConnection_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSTopicConnection_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSTopicConnection_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSTopicConnection_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSTopicConnection_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSTopicConnection_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSTopicConnection_start(This)	\
    (This)->lpVtbl -> start(This)

#define IJMSTopicConnection_stop(This)	\
    (This)->lpVtbl -> stop(This)

#define IJMSTopicConnection_close(This)	\
    (This)->lpVtbl -> close(This)

#define IJMSTopicConnection_getClientID(This,param)	\
    (This)->lpVtbl -> getClientID(This,param)

#define IJMSTopicConnection_setClientID(This,param)	\
    (This)->lpVtbl -> setClientID(This,param)

#define IJMSTopicConnection_getMetaData(This,ppMetaData)	\
    (This)->lpVtbl -> getMetaData(This,ppMetaData)

#define IJMSTopicConnection_setExceptionListener(This,pIUnknown)	\
    (This)->lpVtbl -> setExceptionListener(This,pIUnknown)

#define IJMSTopicConnection_setExceptionListener2(This)	\
    (This)->lpVtbl -> setExceptionListener2(This)

#define IJMSTopicConnection_resetExceptionListener(This)	\
    (This)->lpVtbl -> resetExceptionListener(This)

#define IJMSTopicConnection_setPingInterval(This,interval)	\
    (This)->lpVtbl -> setPingInterval(This,interval)

#define IJMSTopicConnection_getBrokerURL(This,url)	\
    (This)->lpVtbl -> getBrokerURL(This,url)

#define IJMSTopicConnection_setConnectionStateChangeListener(This,pIUnknown)	\
    (This)->lpVtbl -> setConnectionStateChangeListener(This,pIUnknown)

#define IJMSTopicConnection_getConnectionStateChangeListener(This,pIUnknown)	\
    (This)->lpVtbl -> getConnectionStateChangeListener(This,pIUnknown)

#define IJMSTopicConnection_isFaultTolerant(This,ft)	\
    (This)->lpVtbl -> isFaultTolerant(This,ft)

#define IJMSTopicConnection_getConnectionState(This,state)	\
    (This)->lpVtbl -> getConnectionState(This,state)

#define IJMSTopicConnection_getBrokerReconnectURLsSize(This,size)	\
    (This)->lpVtbl -> getBrokerReconnectURLsSize(This,size)

#define IJMSTopicConnection_getBrokerReconnectURLsItem(This,item,url)	\
    (This)->lpVtbl -> getBrokerReconnectURLsItem(This,item,url)

#define IJMSTopicConnection_getStandbyBrokerReconnectURLsSize(This,size)	\
    (This)->lpVtbl -> getStandbyBrokerReconnectURLsSize(This,size)

#define IJMSTopicConnection_getStandbyBrokerReconnectURLsItem(This,item,url)	\
    (This)->lpVtbl -> getStandbyBrokerReconnectURLsItem(This,item,url)

#define IJMSTopicConnection_registerConnectionStateChangeListener(This)	\
    (This)->lpVtbl -> registerConnectionStateChangeListener(This)


#define IJMSTopicConnection_createTopicSession(This,transactedVar,acknowledgeMode,ppSess)	\
    (This)->lpVtbl -> createTopicSession(This,transactedVar,acknowledgeMode,ppSess)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicConnection_createTopicSession_Proxy( 
    IJMSTopicConnection * This,
    /* [in] */ BOOL transactedVar,
    /* [in] */ int acknowledgeMode,
    /* [retval][out] */ IJMSTopicSession **ppSess);


void __RPC_STUB IJMSTopicConnection_createTopicSession_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSTopicConnection_INTERFACE_DEFINED__ */


#ifndef __IJMSMessageConsumer_INTERFACE_DEFINED__
#define __IJMSMessageConsumer_INTERFACE_DEFINED__

/* interface IJMSMessageConsumer */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSMessageConsumer;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("32AD7D42-F3A8-4494-BE76-D92D44F0EBB6")
    IJMSMessageConsumer : public IJMSObject
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE close( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setMessageListener( 
            /* [in] */ IUnknown *pMessageListener) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setMessageListener2( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE resetMessageListener( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getMessageSelector( 
            /* [retval][out] */ BSTR *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE receive( 
            /* [retval][out] */ IJMSMessage **ppMessage) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE receive2( 
            /* [in] */ long timeout,
            /* [retval][out] */ IJMSMessage **ppMessage) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE receiveNoWait( 
            /* [retval][out] */ IJMSMessage **ppMessage) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSMessageConsumerVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSMessageConsumer * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSMessageConsumer * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSMessageConsumer * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSMessageConsumer * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSMessageConsumer * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSMessageConsumer * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSMessageConsumer * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSMessageConsumer * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSMessageConsumer * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSMessageConsumer * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *close )( 
            IJMSMessageConsumer * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setMessageListener )( 
            IJMSMessageConsumer * This,
            /* [in] */ IUnknown *pMessageListener);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setMessageListener2 )( 
            IJMSMessageConsumer * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *resetMessageListener )( 
            IJMSMessageConsumer * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getMessageSelector )( 
            IJMSMessageConsumer * This,
            /* [retval][out] */ BSTR *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *receive )( 
            IJMSMessageConsumer * This,
            /* [retval][out] */ IJMSMessage **ppMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *receive2 )( 
            IJMSMessageConsumer * This,
            /* [in] */ long timeout,
            /* [retval][out] */ IJMSMessage **ppMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *receiveNoWait )( 
            IJMSMessageConsumer * This,
            /* [retval][out] */ IJMSMessage **ppMessage);
        
        END_INTERFACE
    } IJMSMessageConsumerVtbl;

    interface IJMSMessageConsumer
    {
        CONST_VTBL struct IJMSMessageConsumerVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSMessageConsumer_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSMessageConsumer_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSMessageConsumer_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSMessageConsumer_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSMessageConsumer_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSMessageConsumer_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSMessageConsumer_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSMessageConsumer_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSMessageConsumer_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSMessageConsumer_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSMessageConsumer_close(This)	\
    (This)->lpVtbl -> close(This)

#define IJMSMessageConsumer_setMessageListener(This,pMessageListener)	\
    (This)->lpVtbl -> setMessageListener(This,pMessageListener)

#define IJMSMessageConsumer_setMessageListener2(This)	\
    (This)->lpVtbl -> setMessageListener2(This)

#define IJMSMessageConsumer_resetMessageListener(This)	\
    (This)->lpVtbl -> resetMessageListener(This)

#define IJMSMessageConsumer_getMessageSelector(This,param)	\
    (This)->lpVtbl -> getMessageSelector(This,param)

#define IJMSMessageConsumer_receive(This,ppMessage)	\
    (This)->lpVtbl -> receive(This,ppMessage)

#define IJMSMessageConsumer_receive2(This,timeout,ppMessage)	\
    (This)->lpVtbl -> receive2(This,timeout,ppMessage)

#define IJMSMessageConsumer_receiveNoWait(This,ppMessage)	\
    (This)->lpVtbl -> receiveNoWait(This,ppMessage)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageConsumer_close_Proxy( 
    IJMSMessageConsumer * This);


void __RPC_STUB IJMSMessageConsumer_close_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageConsumer_setMessageListener_Proxy( 
    IJMSMessageConsumer * This,
    /* [in] */ IUnknown *pMessageListener);


void __RPC_STUB IJMSMessageConsumer_setMessageListener_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageConsumer_setMessageListener2_Proxy( 
    IJMSMessageConsumer * This);


void __RPC_STUB IJMSMessageConsumer_setMessageListener2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageConsumer_resetMessageListener_Proxy( 
    IJMSMessageConsumer * This);


void __RPC_STUB IJMSMessageConsumer_resetMessageListener_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageConsumer_getMessageSelector_Proxy( 
    IJMSMessageConsumer * This,
    /* [retval][out] */ BSTR *param);


void __RPC_STUB IJMSMessageConsumer_getMessageSelector_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageConsumer_receive_Proxy( 
    IJMSMessageConsumer * This,
    /* [retval][out] */ IJMSMessage **ppMessage);


void __RPC_STUB IJMSMessageConsumer_receive_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageConsumer_receive2_Proxy( 
    IJMSMessageConsumer * This,
    /* [in] */ long timeout,
    /* [retval][out] */ IJMSMessage **ppMessage);


void __RPC_STUB IJMSMessageConsumer_receive2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageConsumer_receiveNoWait_Proxy( 
    IJMSMessageConsumer * This,
    /* [retval][out] */ IJMSMessage **ppMessage);


void __RPC_STUB IJMSMessageConsumer_receiveNoWait_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSMessageConsumer_INTERFACE_DEFINED__ */


#ifndef __IJMSQueueReceiver_INTERFACE_DEFINED__
#define __IJMSQueueReceiver_INTERFACE_DEFINED__

/* interface IJMSQueueReceiver */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSQueueReceiver;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("9E39AAAE-D95B-4017-A36D-226F39E072A5")
    IJMSQueueReceiver : public IJMSMessageConsumer
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getQueue( 
            /* [retval][out] */ IJMSQueue **ppQueue) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setPrefetchCount( 
            /* [in] */ int count) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getPrefetchCount( 
            /* [retval][out] */ int *count) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getPrefetchThreshold( 
            /* [retval][out] */ int *threshold) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setPrefetchThreshold( 
            /* [in] */ int threshold) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSQueueReceiverVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSQueueReceiver * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSQueueReceiver * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSQueueReceiver * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSQueueReceiver * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSQueueReceiver * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSQueueReceiver * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSQueueReceiver * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSQueueReceiver * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSQueueReceiver * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSQueueReceiver * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *close )( 
            IJMSQueueReceiver * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setMessageListener )( 
            IJMSQueueReceiver * This,
            /* [in] */ IUnknown *pMessageListener);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setMessageListener2 )( 
            IJMSQueueReceiver * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *resetMessageListener )( 
            IJMSQueueReceiver * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getMessageSelector )( 
            IJMSQueueReceiver * This,
            /* [retval][out] */ BSTR *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *receive )( 
            IJMSQueueReceiver * This,
            /* [retval][out] */ IJMSMessage **ppMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *receive2 )( 
            IJMSQueueReceiver * This,
            /* [in] */ long timeout,
            /* [retval][out] */ IJMSMessage **ppMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *receiveNoWait )( 
            IJMSQueueReceiver * This,
            /* [retval][out] */ IJMSMessage **ppMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getQueue )( 
            IJMSQueueReceiver * This,
            /* [retval][out] */ IJMSQueue **ppQueue);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setPrefetchCount )( 
            IJMSQueueReceiver * This,
            /* [in] */ int count);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getPrefetchCount )( 
            IJMSQueueReceiver * This,
            /* [retval][out] */ int *count);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getPrefetchThreshold )( 
            IJMSQueueReceiver * This,
            /* [retval][out] */ int *threshold);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setPrefetchThreshold )( 
            IJMSQueueReceiver * This,
            /* [in] */ int threshold);
        
        END_INTERFACE
    } IJMSQueueReceiverVtbl;

    interface IJMSQueueReceiver
    {
        CONST_VTBL struct IJMSQueueReceiverVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSQueueReceiver_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSQueueReceiver_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSQueueReceiver_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSQueueReceiver_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSQueueReceiver_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSQueueReceiver_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSQueueReceiver_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSQueueReceiver_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSQueueReceiver_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSQueueReceiver_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSQueueReceiver_close(This)	\
    (This)->lpVtbl -> close(This)

#define IJMSQueueReceiver_setMessageListener(This,pMessageListener)	\
    (This)->lpVtbl -> setMessageListener(This,pMessageListener)

#define IJMSQueueReceiver_setMessageListener2(This)	\
    (This)->lpVtbl -> setMessageListener2(This)

#define IJMSQueueReceiver_resetMessageListener(This)	\
    (This)->lpVtbl -> resetMessageListener(This)

#define IJMSQueueReceiver_getMessageSelector(This,param)	\
    (This)->lpVtbl -> getMessageSelector(This,param)

#define IJMSQueueReceiver_receive(This,ppMessage)	\
    (This)->lpVtbl -> receive(This,ppMessage)

#define IJMSQueueReceiver_receive2(This,timeout,ppMessage)	\
    (This)->lpVtbl -> receive2(This,timeout,ppMessage)

#define IJMSQueueReceiver_receiveNoWait(This,ppMessage)	\
    (This)->lpVtbl -> receiveNoWait(This,ppMessage)


#define IJMSQueueReceiver_getQueue(This,ppQueue)	\
    (This)->lpVtbl -> getQueue(This,ppQueue)

#define IJMSQueueReceiver_setPrefetchCount(This,count)	\
    (This)->lpVtbl -> setPrefetchCount(This,count)

#define IJMSQueueReceiver_getPrefetchCount(This,count)	\
    (This)->lpVtbl -> getPrefetchCount(This,count)

#define IJMSQueueReceiver_getPrefetchThreshold(This,threshold)	\
    (This)->lpVtbl -> getPrefetchThreshold(This,threshold)

#define IJMSQueueReceiver_setPrefetchThreshold(This,threshold)	\
    (This)->lpVtbl -> setPrefetchThreshold(This,threshold)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueReceiver_getQueue_Proxy( 
    IJMSQueueReceiver * This,
    /* [retval][out] */ IJMSQueue **ppQueue);


void __RPC_STUB IJMSQueueReceiver_getQueue_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueReceiver_setPrefetchCount_Proxy( 
    IJMSQueueReceiver * This,
    /* [in] */ int count);


void __RPC_STUB IJMSQueueReceiver_setPrefetchCount_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueReceiver_getPrefetchCount_Proxy( 
    IJMSQueueReceiver * This,
    /* [retval][out] */ int *count);


void __RPC_STUB IJMSQueueReceiver_getPrefetchCount_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueReceiver_getPrefetchThreshold_Proxy( 
    IJMSQueueReceiver * This,
    /* [retval][out] */ int *threshold);


void __RPC_STUB IJMSQueueReceiver_getPrefetchThreshold_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueReceiver_setPrefetchThreshold_Proxy( 
    IJMSQueueReceiver * This,
    /* [in] */ int threshold);


void __RPC_STUB IJMSQueueReceiver_setPrefetchThreshold_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSQueueReceiver_INTERFACE_DEFINED__ */


#ifndef __IJMSTopicSubscriber_INTERFACE_DEFINED__
#define __IJMSTopicSubscriber_INTERFACE_DEFINED__

/* interface IJMSTopicSubscriber */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSTopicSubscriber;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("08E3393E-0D10-47A4-9827-E043B6F0F2ED")
    IJMSTopicSubscriber : public IJMSMessageConsumer
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getTopic( 
            /* [retval][out] */ IJMSTopic **ppTopic) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getNoLocal( 
            /* [retval][out] */ BOOL *pNoLocal) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSTopicSubscriberVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSTopicSubscriber * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSTopicSubscriber * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSTopicSubscriber * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSTopicSubscriber * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSTopicSubscriber * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSTopicSubscriber * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSTopicSubscriber * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSTopicSubscriber * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSTopicSubscriber * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSTopicSubscriber * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *close )( 
            IJMSTopicSubscriber * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setMessageListener )( 
            IJMSTopicSubscriber * This,
            /* [in] */ IUnknown *pMessageListener);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setMessageListener2 )( 
            IJMSTopicSubscriber * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *resetMessageListener )( 
            IJMSTopicSubscriber * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getMessageSelector )( 
            IJMSTopicSubscriber * This,
            /* [retval][out] */ BSTR *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *receive )( 
            IJMSTopicSubscriber * This,
            /* [retval][out] */ IJMSMessage **ppMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *receive2 )( 
            IJMSTopicSubscriber * This,
            /* [in] */ long timeout,
            /* [retval][out] */ IJMSMessage **ppMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *receiveNoWait )( 
            IJMSTopicSubscriber * This,
            /* [retval][out] */ IJMSMessage **ppMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getTopic )( 
            IJMSTopicSubscriber * This,
            /* [retval][out] */ IJMSTopic **ppTopic);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getNoLocal )( 
            IJMSTopicSubscriber * This,
            /* [retval][out] */ BOOL *pNoLocal);
        
        END_INTERFACE
    } IJMSTopicSubscriberVtbl;

    interface IJMSTopicSubscriber
    {
        CONST_VTBL struct IJMSTopicSubscriberVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSTopicSubscriber_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSTopicSubscriber_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSTopicSubscriber_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSTopicSubscriber_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSTopicSubscriber_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSTopicSubscriber_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSTopicSubscriber_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSTopicSubscriber_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSTopicSubscriber_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSTopicSubscriber_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSTopicSubscriber_close(This)	\
    (This)->lpVtbl -> close(This)

#define IJMSTopicSubscriber_setMessageListener(This,pMessageListener)	\
    (This)->lpVtbl -> setMessageListener(This,pMessageListener)

#define IJMSTopicSubscriber_setMessageListener2(This)	\
    (This)->lpVtbl -> setMessageListener2(This)

#define IJMSTopicSubscriber_resetMessageListener(This)	\
    (This)->lpVtbl -> resetMessageListener(This)

#define IJMSTopicSubscriber_getMessageSelector(This,param)	\
    (This)->lpVtbl -> getMessageSelector(This,param)

#define IJMSTopicSubscriber_receive(This,ppMessage)	\
    (This)->lpVtbl -> receive(This,ppMessage)

#define IJMSTopicSubscriber_receive2(This,timeout,ppMessage)	\
    (This)->lpVtbl -> receive2(This,timeout,ppMessage)

#define IJMSTopicSubscriber_receiveNoWait(This,ppMessage)	\
    (This)->lpVtbl -> receiveNoWait(This,ppMessage)


#define IJMSTopicSubscriber_getTopic(This,ppTopic)	\
    (This)->lpVtbl -> getTopic(This,ppTopic)

#define IJMSTopicSubscriber_getNoLocal(This,pNoLocal)	\
    (This)->lpVtbl -> getNoLocal(This,pNoLocal)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicSubscriber_getTopic_Proxy( 
    IJMSTopicSubscriber * This,
    /* [retval][out] */ IJMSTopic **ppTopic);


void __RPC_STUB IJMSTopicSubscriber_getTopic_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicSubscriber_getNoLocal_Proxy( 
    IJMSTopicSubscriber * This,
    /* [retval][out] */ BOOL *pNoLocal);


void __RPC_STUB IJMSTopicSubscriber_getNoLocal_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSTopicSubscriber_INTERFACE_DEFINED__ */


#ifndef __IJMSMessageProducer_INTERFACE_DEFINED__
#define __IJMSMessageProducer_INTERFACE_DEFINED__

/* interface IJMSMessageProducer */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSMessageProducer;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("06AE534D-C51C-4CD9-9DF3-E11D14AE9FE2")
    IJMSMessageProducer : public IJMSObject
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE close( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getDisableMessageID( 
            /* [retval][out] */ BOOL *pDisable) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setDisableMessageID( 
            /* [in] */ BOOL disableBOOL) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getDisableMessageTimestamp( 
            /* [retval][out] */ BOOL *pDisable) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setDisableMessageTimestamp( 
            /* [in] */ BOOL disableBOOL) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setDeliveryMode( 
            /* [in] */ enum JMSDeliveryMode mode) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getPriority( 
            /* [retval][out] */ int *pPriority) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setPriority( 
            /* [in] */ int priority) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getTimeToLive( 
            /* [retval][out] */ long *pTimeToLive) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setTimeToLive( 
            /* [in] */ long timeToLive) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSMessageProducerVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSMessageProducer * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSMessageProducer * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSMessageProducer * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSMessageProducer * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSMessageProducer * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSMessageProducer * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSMessageProducer * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSMessageProducer * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSMessageProducer * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSMessageProducer * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *close )( 
            IJMSMessageProducer * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDisableMessageID )( 
            IJMSMessageProducer * This,
            /* [retval][out] */ BOOL *pDisable);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setDisableMessageID )( 
            IJMSMessageProducer * This,
            /* [in] */ BOOL disableBOOL);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDisableMessageTimestamp )( 
            IJMSMessageProducer * This,
            /* [retval][out] */ BOOL *pDisable);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setDisableMessageTimestamp )( 
            IJMSMessageProducer * This,
            /* [in] */ BOOL disableBOOL);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setDeliveryMode )( 
            IJMSMessageProducer * This,
            /* [in] */ enum JMSDeliveryMode mode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getPriority )( 
            IJMSMessageProducer * This,
            /* [retval][out] */ int *pPriority);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setPriority )( 
            IJMSMessageProducer * This,
            /* [in] */ int priority);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getTimeToLive )( 
            IJMSMessageProducer * This,
            /* [retval][out] */ long *pTimeToLive);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setTimeToLive )( 
            IJMSMessageProducer * This,
            /* [in] */ long timeToLive);
        
        END_INTERFACE
    } IJMSMessageProducerVtbl;

    interface IJMSMessageProducer
    {
        CONST_VTBL struct IJMSMessageProducerVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSMessageProducer_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSMessageProducer_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSMessageProducer_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSMessageProducer_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSMessageProducer_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSMessageProducer_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSMessageProducer_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSMessageProducer_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSMessageProducer_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSMessageProducer_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSMessageProducer_close(This)	\
    (This)->lpVtbl -> close(This)

#define IJMSMessageProducer_getDisableMessageID(This,pDisable)	\
    (This)->lpVtbl -> getDisableMessageID(This,pDisable)

#define IJMSMessageProducer_setDisableMessageID(This,disableBOOL)	\
    (This)->lpVtbl -> setDisableMessageID(This,disableBOOL)

#define IJMSMessageProducer_getDisableMessageTimestamp(This,pDisable)	\
    (This)->lpVtbl -> getDisableMessageTimestamp(This,pDisable)

#define IJMSMessageProducer_setDisableMessageTimestamp(This,disableBOOL)	\
    (This)->lpVtbl -> setDisableMessageTimestamp(This,disableBOOL)

#define IJMSMessageProducer_setDeliveryMode(This,mode)	\
    (This)->lpVtbl -> setDeliveryMode(This,mode)

#define IJMSMessageProducer_getPriority(This,pPriority)	\
    (This)->lpVtbl -> getPriority(This,pPriority)

#define IJMSMessageProducer_setPriority(This,priority)	\
    (This)->lpVtbl -> setPriority(This,priority)

#define IJMSMessageProducer_getTimeToLive(This,pTimeToLive)	\
    (This)->lpVtbl -> getTimeToLive(This,pTimeToLive)

#define IJMSMessageProducer_setTimeToLive(This,timeToLive)	\
    (This)->lpVtbl -> setTimeToLive(This,timeToLive)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageProducer_close_Proxy( 
    IJMSMessageProducer * This);


void __RPC_STUB IJMSMessageProducer_close_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageProducer_getDisableMessageID_Proxy( 
    IJMSMessageProducer * This,
    /* [retval][out] */ BOOL *pDisable);


void __RPC_STUB IJMSMessageProducer_getDisableMessageID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageProducer_setDisableMessageID_Proxy( 
    IJMSMessageProducer * This,
    /* [in] */ BOOL disableBOOL);


void __RPC_STUB IJMSMessageProducer_setDisableMessageID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageProducer_getDisableMessageTimestamp_Proxy( 
    IJMSMessageProducer * This,
    /* [retval][out] */ BOOL *pDisable);


void __RPC_STUB IJMSMessageProducer_getDisableMessageTimestamp_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageProducer_setDisableMessageTimestamp_Proxy( 
    IJMSMessageProducer * This,
    /* [in] */ BOOL disableBOOL);


void __RPC_STUB IJMSMessageProducer_setDisableMessageTimestamp_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageProducer_setDeliveryMode_Proxy( 
    IJMSMessageProducer * This,
    /* [in] */ enum JMSDeliveryMode mode);


void __RPC_STUB IJMSMessageProducer_setDeliveryMode_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageProducer_getPriority_Proxy( 
    IJMSMessageProducer * This,
    /* [retval][out] */ int *pPriority);


void __RPC_STUB IJMSMessageProducer_getPriority_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageProducer_setPriority_Proxy( 
    IJMSMessageProducer * This,
    /* [in] */ int priority);


void __RPC_STUB IJMSMessageProducer_setPriority_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageProducer_getTimeToLive_Proxy( 
    IJMSMessageProducer * This,
    /* [retval][out] */ long *pTimeToLive);


void __RPC_STUB IJMSMessageProducer_getTimeToLive_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageProducer_setTimeToLive_Proxy( 
    IJMSMessageProducer * This,
    /* [in] */ long timeToLive);


void __RPC_STUB IJMSMessageProducer_setTimeToLive_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSMessageProducer_INTERFACE_DEFINED__ */


#ifndef __IJMSQueueSender_INTERFACE_DEFINED__
#define __IJMSQueueSender_INTERFACE_DEFINED__

/* interface IJMSQueueSender */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSQueueSender;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C4A341D0-68D7-403E-8ECF-A73271DE8DFD")
    IJMSQueueSender : public IJMSMessageProducer
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getQueue( 
            /* [retval][out] */ IJMSQueue **ppQueue) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE send( 
            /* [in] */ IJMSMessage *pMessage) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE send2( 
            /* [in] */ IJMSMessage *pMessage,
            /* [in] */ int mode,
            /* [in] */ int priority,
            /* [in] */ int ttl) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE send3( 
            /* [in] */ IJMSQueue *pQueue,
            /* [in] */ IJMSMessage *pMessage) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE send4( 
            /* [in] */ IJMSQueue *pQueue,
            /* [in] */ IJMSMessage *pMessage,
            /* [in] */ int mode,
            /* [in] */ int priority,
            /* [in] */ int ttl) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSQueueSenderVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSQueueSender * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSQueueSender * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSQueueSender * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSQueueSender * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSQueueSender * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSQueueSender * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSQueueSender * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSQueueSender * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSQueueSender * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSQueueSender * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *close )( 
            IJMSQueueSender * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDisableMessageID )( 
            IJMSQueueSender * This,
            /* [retval][out] */ BOOL *pDisable);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setDisableMessageID )( 
            IJMSQueueSender * This,
            /* [in] */ BOOL disableBOOL);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDisableMessageTimestamp )( 
            IJMSQueueSender * This,
            /* [retval][out] */ BOOL *pDisable);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setDisableMessageTimestamp )( 
            IJMSQueueSender * This,
            /* [in] */ BOOL disableBOOL);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setDeliveryMode )( 
            IJMSQueueSender * This,
            /* [in] */ enum JMSDeliveryMode mode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getPriority )( 
            IJMSQueueSender * This,
            /* [retval][out] */ int *pPriority);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setPriority )( 
            IJMSQueueSender * This,
            /* [in] */ int priority);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getTimeToLive )( 
            IJMSQueueSender * This,
            /* [retval][out] */ long *pTimeToLive);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setTimeToLive )( 
            IJMSQueueSender * This,
            /* [in] */ long timeToLive);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getQueue )( 
            IJMSQueueSender * This,
            /* [retval][out] */ IJMSQueue **ppQueue);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *send )( 
            IJMSQueueSender * This,
            /* [in] */ IJMSMessage *pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *send2 )( 
            IJMSQueueSender * This,
            /* [in] */ IJMSMessage *pMessage,
            /* [in] */ int mode,
            /* [in] */ int priority,
            /* [in] */ int ttl);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *send3 )( 
            IJMSQueueSender * This,
            /* [in] */ IJMSQueue *pQueue,
            /* [in] */ IJMSMessage *pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *send4 )( 
            IJMSQueueSender * This,
            /* [in] */ IJMSQueue *pQueue,
            /* [in] */ IJMSMessage *pMessage,
            /* [in] */ int mode,
            /* [in] */ int priority,
            /* [in] */ int ttl);
        
        END_INTERFACE
    } IJMSQueueSenderVtbl;

    interface IJMSQueueSender
    {
        CONST_VTBL struct IJMSQueueSenderVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSQueueSender_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSQueueSender_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSQueueSender_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSQueueSender_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSQueueSender_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSQueueSender_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSQueueSender_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSQueueSender_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSQueueSender_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSQueueSender_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSQueueSender_close(This)	\
    (This)->lpVtbl -> close(This)

#define IJMSQueueSender_getDisableMessageID(This,pDisable)	\
    (This)->lpVtbl -> getDisableMessageID(This,pDisable)

#define IJMSQueueSender_setDisableMessageID(This,disableBOOL)	\
    (This)->lpVtbl -> setDisableMessageID(This,disableBOOL)

#define IJMSQueueSender_getDisableMessageTimestamp(This,pDisable)	\
    (This)->lpVtbl -> getDisableMessageTimestamp(This,pDisable)

#define IJMSQueueSender_setDisableMessageTimestamp(This,disableBOOL)	\
    (This)->lpVtbl -> setDisableMessageTimestamp(This,disableBOOL)

#define IJMSQueueSender_setDeliveryMode(This,mode)	\
    (This)->lpVtbl -> setDeliveryMode(This,mode)

#define IJMSQueueSender_getPriority(This,pPriority)	\
    (This)->lpVtbl -> getPriority(This,pPriority)

#define IJMSQueueSender_setPriority(This,priority)	\
    (This)->lpVtbl -> setPriority(This,priority)

#define IJMSQueueSender_getTimeToLive(This,pTimeToLive)	\
    (This)->lpVtbl -> getTimeToLive(This,pTimeToLive)

#define IJMSQueueSender_setTimeToLive(This,timeToLive)	\
    (This)->lpVtbl -> setTimeToLive(This,timeToLive)


#define IJMSQueueSender_getQueue(This,ppQueue)	\
    (This)->lpVtbl -> getQueue(This,ppQueue)

#define IJMSQueueSender_send(This,pMessage)	\
    (This)->lpVtbl -> send(This,pMessage)

#define IJMSQueueSender_send2(This,pMessage,mode,priority,ttl)	\
    (This)->lpVtbl -> send2(This,pMessage,mode,priority,ttl)

#define IJMSQueueSender_send3(This,pQueue,pMessage)	\
    (This)->lpVtbl -> send3(This,pQueue,pMessage)

#define IJMSQueueSender_send4(This,pQueue,pMessage,mode,priority,ttl)	\
    (This)->lpVtbl -> send4(This,pQueue,pMessage,mode,priority,ttl)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueSender_getQueue_Proxy( 
    IJMSQueueSender * This,
    /* [retval][out] */ IJMSQueue **ppQueue);


void __RPC_STUB IJMSQueueSender_getQueue_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueSender_send_Proxy( 
    IJMSQueueSender * This,
    /* [in] */ IJMSMessage *pMessage);


void __RPC_STUB IJMSQueueSender_send_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueSender_send2_Proxy( 
    IJMSQueueSender * This,
    /* [in] */ IJMSMessage *pMessage,
    /* [in] */ int mode,
    /* [in] */ int priority,
    /* [in] */ int ttl);


void __RPC_STUB IJMSQueueSender_send2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueSender_send3_Proxy( 
    IJMSQueueSender * This,
    /* [in] */ IJMSQueue *pQueue,
    /* [in] */ IJMSMessage *pMessage);


void __RPC_STUB IJMSQueueSender_send3_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueSender_send4_Proxy( 
    IJMSQueueSender * This,
    /* [in] */ IJMSQueue *pQueue,
    /* [in] */ IJMSMessage *pMessage,
    /* [in] */ int mode,
    /* [in] */ int priority,
    /* [in] */ int ttl);


void __RPC_STUB IJMSQueueSender_send4_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSQueueSender_INTERFACE_DEFINED__ */


#ifndef __IJMSTopicPublisher_INTERFACE_DEFINED__
#define __IJMSTopicPublisher_INTERFACE_DEFINED__

/* interface IJMSTopicPublisher */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSTopicPublisher;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("ED6FBAE8-B5FD-4A53-8EF1-486EB7AD1EBF")
    IJMSTopicPublisher : public IJMSMessageProducer
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getTopic( 
            /* [retval][out] */ IJMSTopic **ppTopic) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE publish( 
            /* [in] */ IJMSMessage *pMessage) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE publish2( 
            /* [in] */ IJMSMessage *pMessage,
            /* [in] */ int mode,
            /* [in] */ int priority,
            /* [in] */ int ttl) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE publish3( 
            /* [in] */ IJMSTopic *pTopic,
            /* [in] */ IJMSMessage *pMessage) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE publish4( 
            /* [in] */ IJMSTopic *pTopic,
            /* [in] */ IJMSMessage *pMessage,
            /* [in] */ int mode,
            /* [in] */ int priority,
            /* [in] */ int ttl) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSTopicPublisherVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSTopicPublisher * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSTopicPublisher * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSTopicPublisher * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSTopicPublisher * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSTopicPublisher * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSTopicPublisher * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSTopicPublisher * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSTopicPublisher * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSTopicPublisher * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSTopicPublisher * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *close )( 
            IJMSTopicPublisher * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDisableMessageID )( 
            IJMSTopicPublisher * This,
            /* [retval][out] */ BOOL *pDisable);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setDisableMessageID )( 
            IJMSTopicPublisher * This,
            /* [in] */ BOOL disableBOOL);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDisableMessageTimestamp )( 
            IJMSTopicPublisher * This,
            /* [retval][out] */ BOOL *pDisable);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setDisableMessageTimestamp )( 
            IJMSTopicPublisher * This,
            /* [in] */ BOOL disableBOOL);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setDeliveryMode )( 
            IJMSTopicPublisher * This,
            /* [in] */ enum JMSDeliveryMode mode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getPriority )( 
            IJMSTopicPublisher * This,
            /* [retval][out] */ int *pPriority);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setPriority )( 
            IJMSTopicPublisher * This,
            /* [in] */ int priority);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getTimeToLive )( 
            IJMSTopicPublisher * This,
            /* [retval][out] */ long *pTimeToLive);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setTimeToLive )( 
            IJMSTopicPublisher * This,
            /* [in] */ long timeToLive);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getTopic )( 
            IJMSTopicPublisher * This,
            /* [retval][out] */ IJMSTopic **ppTopic);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *publish )( 
            IJMSTopicPublisher * This,
            /* [in] */ IJMSMessage *pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *publish2 )( 
            IJMSTopicPublisher * This,
            /* [in] */ IJMSMessage *pMessage,
            /* [in] */ int mode,
            /* [in] */ int priority,
            /* [in] */ int ttl);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *publish3 )( 
            IJMSTopicPublisher * This,
            /* [in] */ IJMSTopic *pTopic,
            /* [in] */ IJMSMessage *pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *publish4 )( 
            IJMSTopicPublisher * This,
            /* [in] */ IJMSTopic *pTopic,
            /* [in] */ IJMSMessage *pMessage,
            /* [in] */ int mode,
            /* [in] */ int priority,
            /* [in] */ int ttl);
        
        END_INTERFACE
    } IJMSTopicPublisherVtbl;

    interface IJMSTopicPublisher
    {
        CONST_VTBL struct IJMSTopicPublisherVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSTopicPublisher_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSTopicPublisher_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSTopicPublisher_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSTopicPublisher_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSTopicPublisher_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSTopicPublisher_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSTopicPublisher_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSTopicPublisher_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSTopicPublisher_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSTopicPublisher_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSTopicPublisher_close(This)	\
    (This)->lpVtbl -> close(This)

#define IJMSTopicPublisher_getDisableMessageID(This,pDisable)	\
    (This)->lpVtbl -> getDisableMessageID(This,pDisable)

#define IJMSTopicPublisher_setDisableMessageID(This,disableBOOL)	\
    (This)->lpVtbl -> setDisableMessageID(This,disableBOOL)

#define IJMSTopicPublisher_getDisableMessageTimestamp(This,pDisable)	\
    (This)->lpVtbl -> getDisableMessageTimestamp(This,pDisable)

#define IJMSTopicPublisher_setDisableMessageTimestamp(This,disableBOOL)	\
    (This)->lpVtbl -> setDisableMessageTimestamp(This,disableBOOL)

#define IJMSTopicPublisher_setDeliveryMode(This,mode)	\
    (This)->lpVtbl -> setDeliveryMode(This,mode)

#define IJMSTopicPublisher_getPriority(This,pPriority)	\
    (This)->lpVtbl -> getPriority(This,pPriority)

#define IJMSTopicPublisher_setPriority(This,priority)	\
    (This)->lpVtbl -> setPriority(This,priority)

#define IJMSTopicPublisher_getTimeToLive(This,pTimeToLive)	\
    (This)->lpVtbl -> getTimeToLive(This,pTimeToLive)

#define IJMSTopicPublisher_setTimeToLive(This,timeToLive)	\
    (This)->lpVtbl -> setTimeToLive(This,timeToLive)


#define IJMSTopicPublisher_getTopic(This,ppTopic)	\
    (This)->lpVtbl -> getTopic(This,ppTopic)

#define IJMSTopicPublisher_publish(This,pMessage)	\
    (This)->lpVtbl -> publish(This,pMessage)

#define IJMSTopicPublisher_publish2(This,pMessage,mode,priority,ttl)	\
    (This)->lpVtbl -> publish2(This,pMessage,mode,priority,ttl)

#define IJMSTopicPublisher_publish3(This,pTopic,pMessage)	\
    (This)->lpVtbl -> publish3(This,pTopic,pMessage)

#define IJMSTopicPublisher_publish4(This,pTopic,pMessage,mode,priority,ttl)	\
    (This)->lpVtbl -> publish4(This,pTopic,pMessage,mode,priority,ttl)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicPublisher_getTopic_Proxy( 
    IJMSTopicPublisher * This,
    /* [retval][out] */ IJMSTopic **ppTopic);


void __RPC_STUB IJMSTopicPublisher_getTopic_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicPublisher_publish_Proxy( 
    IJMSTopicPublisher * This,
    /* [in] */ IJMSMessage *pMessage);


void __RPC_STUB IJMSTopicPublisher_publish_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicPublisher_publish2_Proxy( 
    IJMSTopicPublisher * This,
    /* [in] */ IJMSMessage *pMessage,
    /* [in] */ int mode,
    /* [in] */ int priority,
    /* [in] */ int ttl);


void __RPC_STUB IJMSTopicPublisher_publish2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicPublisher_publish3_Proxy( 
    IJMSTopicPublisher * This,
    /* [in] */ IJMSTopic *pTopic,
    /* [in] */ IJMSMessage *pMessage);


void __RPC_STUB IJMSTopicPublisher_publish3_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicPublisher_publish4_Proxy( 
    IJMSTopicPublisher * This,
    /* [in] */ IJMSTopic *pTopic,
    /* [in] */ IJMSMessage *pMessage,
    /* [in] */ int mode,
    /* [in] */ int priority,
    /* [in] */ int ttl);


void __RPC_STUB IJMSTopicPublisher_publish4_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSTopicPublisher_INTERFACE_DEFINED__ */


#ifndef __IJMSSession_INTERFACE_DEFINED__
#define __IJMSSession_INTERFACE_DEFINED__

/* interface IJMSSession */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSSession;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("E1F2A9F2-52AA-459C-88E5-DDD9C67AF0E9")
    IJMSSession : public IJMSObject
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE run( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE commit( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE close( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE rollback( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE recover( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createMessage( 
            /* [retval][out] */ IJMSMessage **ppMessage) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createBytesMessage( 
            /* [retval][out] */ IJMSBytesMessage **pMessage) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createObjectMessage( 
            /* [retval][out] */ IJMSObjectMessage **pMessage) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createObjectMessage2( 
            /* [in] */ VARIANT inputObject,
            /* [retval][out] */ IJMSObjectMessage **pMessage) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createStreamMessage( 
            /* [retval][out] */ IJMSStreamMessage **pMessage) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createTextMessage( 
            /* [retval][out] */ IJMSTextMessage **pMessage) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createTextMessage2( 
            /* [in] */ BSTR inputText,
            /* [retval][out] */ IJMSTextMessage **pMessage) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getTransacted( 
            /* [retval][out] */ BOOL *transacted) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setMessageListener( 
            /* [in] */ IUnknown *pListener) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setMessageListener2( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE resetMessageListener( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getTxnBatchSize( 
            /* [retval][out] */ long *transactionBatchBufferSize) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setTxnBatchSize( 
            /* [in] */ long transactionBatchBufferSize) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSSessionVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSSession * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSSession * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSSession * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSSession * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSSession * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSSession * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSSession * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSSession * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSSession * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSSession * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *run )( 
            IJMSSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *commit )( 
            IJMSSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *close )( 
            IJMSSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *rollback )( 
            IJMSSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *recover )( 
            IJMSSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createMessage )( 
            IJMSSession * This,
            /* [retval][out] */ IJMSMessage **ppMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createBytesMessage )( 
            IJMSSession * This,
            /* [retval][out] */ IJMSBytesMessage **pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createObjectMessage )( 
            IJMSSession * This,
            /* [retval][out] */ IJMSObjectMessage **pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createObjectMessage2 )( 
            IJMSSession * This,
            /* [in] */ VARIANT inputObject,
            /* [retval][out] */ IJMSObjectMessage **pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createStreamMessage )( 
            IJMSSession * This,
            /* [retval][out] */ IJMSStreamMessage **pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createTextMessage )( 
            IJMSSession * This,
            /* [retval][out] */ IJMSTextMessage **pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createTextMessage2 )( 
            IJMSSession * This,
            /* [in] */ BSTR inputText,
            /* [retval][out] */ IJMSTextMessage **pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getTransacted )( 
            IJMSSession * This,
            /* [retval][out] */ BOOL *transacted);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setMessageListener )( 
            IJMSSession * This,
            /* [in] */ IUnknown *pListener);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setMessageListener2 )( 
            IJMSSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *resetMessageListener )( 
            IJMSSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getTxnBatchSize )( 
            IJMSSession * This,
            /* [retval][out] */ long *transactionBatchBufferSize);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setTxnBatchSize )( 
            IJMSSession * This,
            /* [in] */ long transactionBatchBufferSize);
        
        END_INTERFACE
    } IJMSSessionVtbl;

    interface IJMSSession
    {
        CONST_VTBL struct IJMSSessionVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSSession_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSSession_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSSession_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSSession_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSSession_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSSession_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSSession_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSSession_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSSession_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSSession_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSSession_run(This)	\
    (This)->lpVtbl -> run(This)

#define IJMSSession_commit(This)	\
    (This)->lpVtbl -> commit(This)

#define IJMSSession_close(This)	\
    (This)->lpVtbl -> close(This)

#define IJMSSession_rollback(This)	\
    (This)->lpVtbl -> rollback(This)

#define IJMSSession_recover(This)	\
    (This)->lpVtbl -> recover(This)

#define IJMSSession_createMessage(This,ppMessage)	\
    (This)->lpVtbl -> createMessage(This,ppMessage)

#define IJMSSession_createBytesMessage(This,pMessage)	\
    (This)->lpVtbl -> createBytesMessage(This,pMessage)

#define IJMSSession_createObjectMessage(This,pMessage)	\
    (This)->lpVtbl -> createObjectMessage(This,pMessage)

#define IJMSSession_createObjectMessage2(This,inputObject,pMessage)	\
    (This)->lpVtbl -> createObjectMessage2(This,inputObject,pMessage)

#define IJMSSession_createStreamMessage(This,pMessage)	\
    (This)->lpVtbl -> createStreamMessage(This,pMessage)

#define IJMSSession_createTextMessage(This,pMessage)	\
    (This)->lpVtbl -> createTextMessage(This,pMessage)

#define IJMSSession_createTextMessage2(This,inputText,pMessage)	\
    (This)->lpVtbl -> createTextMessage2(This,inputText,pMessage)

#define IJMSSession_getTransacted(This,transacted)	\
    (This)->lpVtbl -> getTransacted(This,transacted)

#define IJMSSession_setMessageListener(This,pListener)	\
    (This)->lpVtbl -> setMessageListener(This,pListener)

#define IJMSSession_setMessageListener2(This)	\
    (This)->lpVtbl -> setMessageListener2(This)

#define IJMSSession_resetMessageListener(This)	\
    (This)->lpVtbl -> resetMessageListener(This)

#define IJMSSession_getTxnBatchSize(This,transactionBatchBufferSize)	\
    (This)->lpVtbl -> getTxnBatchSize(This,transactionBatchBufferSize)

#define IJMSSession_setTxnBatchSize(This,transactionBatchBufferSize)	\
    (This)->lpVtbl -> setTxnBatchSize(This,transactionBatchBufferSize)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSSession_run_Proxy( 
    IJMSSession * This);


void __RPC_STUB IJMSSession_run_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSSession_commit_Proxy( 
    IJMSSession * This);


void __RPC_STUB IJMSSession_commit_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSSession_close_Proxy( 
    IJMSSession * This);


void __RPC_STUB IJMSSession_close_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSSession_rollback_Proxy( 
    IJMSSession * This);


void __RPC_STUB IJMSSession_rollback_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSSession_recover_Proxy( 
    IJMSSession * This);


void __RPC_STUB IJMSSession_recover_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSSession_createMessage_Proxy( 
    IJMSSession * This,
    /* [retval][out] */ IJMSMessage **ppMessage);


void __RPC_STUB IJMSSession_createMessage_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSSession_createBytesMessage_Proxy( 
    IJMSSession * This,
    /* [retval][out] */ IJMSBytesMessage **pMessage);


void __RPC_STUB IJMSSession_createBytesMessage_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSSession_createObjectMessage_Proxy( 
    IJMSSession * This,
    /* [retval][out] */ IJMSObjectMessage **pMessage);


void __RPC_STUB IJMSSession_createObjectMessage_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSSession_createObjectMessage2_Proxy( 
    IJMSSession * This,
    /* [in] */ VARIANT inputObject,
    /* [retval][out] */ IJMSObjectMessage **pMessage);


void __RPC_STUB IJMSSession_createObjectMessage2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSSession_createStreamMessage_Proxy( 
    IJMSSession * This,
    /* [retval][out] */ IJMSStreamMessage **pMessage);


void __RPC_STUB IJMSSession_createStreamMessage_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSSession_createTextMessage_Proxy( 
    IJMSSession * This,
    /* [retval][out] */ IJMSTextMessage **pMessage);


void __RPC_STUB IJMSSession_createTextMessage_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSSession_createTextMessage2_Proxy( 
    IJMSSession * This,
    /* [in] */ BSTR inputText,
    /* [retval][out] */ IJMSTextMessage **pMessage);


void __RPC_STUB IJMSSession_createTextMessage2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSSession_getTransacted_Proxy( 
    IJMSSession * This,
    /* [retval][out] */ BOOL *transacted);


void __RPC_STUB IJMSSession_getTransacted_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSSession_setMessageListener_Proxy( 
    IJMSSession * This,
    /* [in] */ IUnknown *pListener);


void __RPC_STUB IJMSSession_setMessageListener_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSSession_setMessageListener2_Proxy( 
    IJMSSession * This);


void __RPC_STUB IJMSSession_setMessageListener2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSSession_resetMessageListener_Proxy( 
    IJMSSession * This);


void __RPC_STUB IJMSSession_resetMessageListener_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSSession_getTxnBatchSize_Proxy( 
    IJMSSession * This,
    /* [retval][out] */ long *transactionBatchBufferSize);


void __RPC_STUB IJMSSession_getTxnBatchSize_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSSession_setTxnBatchSize_Proxy( 
    IJMSSession * This,
    /* [in] */ long transactionBatchBufferSize);


void __RPC_STUB IJMSSession_setTxnBatchSize_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSSession_INTERFACE_DEFINED__ */


#ifndef __IJMSQueueSession_INTERFACE_DEFINED__
#define __IJMSQueueSession_INTERFACE_DEFINED__

/* interface IJMSQueueSession */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSQueueSession;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("2AD85D65-7457-4029-BF7B-D96661F20C85")
    IJMSQueueSession : public IJMSSession
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createQueue( 
            /* [in] */ BSTR queueName,
            /* [retval][out] */ IJMSQueue **ppQueue) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createReceiver( 
            /* [in] */ IJMSQueue *pQueue,
            /* [retval][out] */ IJMSQueueReceiver **ppReceiver) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createReceiver2( 
            /* [in] */ IJMSQueue *pQueue,
            /* [in] */ BSTR Selector,
            /* [retval][out] */ IJMSQueueReceiver **ppReceiver) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createSender( 
            /* [in] */ IJMSQueue *pQueue,
            /* [retval][out] */ IJMSQueueSender **ppSender) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createSender2( 
            /* [retval][out] */ IJMSQueueSender **ppSender) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createBrowser( 
            /* [in] */ IJMSQueue *pQueue,
            /* [retval][out] */ IJMSQueueBrowser **ppBrowser) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createBrowser2( 
            /* [in] */ IJMSQueue *pQueue,
            /* [in] */ BSTR Selector,
            /* [retval][out] */ IJMSQueueBrowser **ppBrowser) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createTemporaryQueue( 
            /* [retval][out] */ IJMSTemporaryQueue **ppTempQueue) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setFlowControlDisabled( 
            /* [in] */ BOOL disabled) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSQueueSessionVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSQueueSession * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSQueueSession * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSQueueSession * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSQueueSession * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSQueueSession * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSQueueSession * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSQueueSession * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSQueueSession * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSQueueSession * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSQueueSession * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *run )( 
            IJMSQueueSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *commit )( 
            IJMSQueueSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *close )( 
            IJMSQueueSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *rollback )( 
            IJMSQueueSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *recover )( 
            IJMSQueueSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createMessage )( 
            IJMSQueueSession * This,
            /* [retval][out] */ IJMSMessage **ppMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createBytesMessage )( 
            IJMSQueueSession * This,
            /* [retval][out] */ IJMSBytesMessage **pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createObjectMessage )( 
            IJMSQueueSession * This,
            /* [retval][out] */ IJMSObjectMessage **pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createObjectMessage2 )( 
            IJMSQueueSession * This,
            /* [in] */ VARIANT inputObject,
            /* [retval][out] */ IJMSObjectMessage **pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createStreamMessage )( 
            IJMSQueueSession * This,
            /* [retval][out] */ IJMSStreamMessage **pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createTextMessage )( 
            IJMSQueueSession * This,
            /* [retval][out] */ IJMSTextMessage **pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createTextMessage2 )( 
            IJMSQueueSession * This,
            /* [in] */ BSTR inputText,
            /* [retval][out] */ IJMSTextMessage **pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getTransacted )( 
            IJMSQueueSession * This,
            /* [retval][out] */ BOOL *transacted);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setMessageListener )( 
            IJMSQueueSession * This,
            /* [in] */ IUnknown *pListener);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setMessageListener2 )( 
            IJMSQueueSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *resetMessageListener )( 
            IJMSQueueSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getTxnBatchSize )( 
            IJMSQueueSession * This,
            /* [retval][out] */ long *transactionBatchBufferSize);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setTxnBatchSize )( 
            IJMSQueueSession * This,
            /* [in] */ long transactionBatchBufferSize);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createQueue )( 
            IJMSQueueSession * This,
            /* [in] */ BSTR queueName,
            /* [retval][out] */ IJMSQueue **ppQueue);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createReceiver )( 
            IJMSQueueSession * This,
            /* [in] */ IJMSQueue *pQueue,
            /* [retval][out] */ IJMSQueueReceiver **ppReceiver);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createReceiver2 )( 
            IJMSQueueSession * This,
            /* [in] */ IJMSQueue *pQueue,
            /* [in] */ BSTR Selector,
            /* [retval][out] */ IJMSQueueReceiver **ppReceiver);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createSender )( 
            IJMSQueueSession * This,
            /* [in] */ IJMSQueue *pQueue,
            /* [retval][out] */ IJMSQueueSender **ppSender);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createSender2 )( 
            IJMSQueueSession * This,
            /* [retval][out] */ IJMSQueueSender **ppSender);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createBrowser )( 
            IJMSQueueSession * This,
            /* [in] */ IJMSQueue *pQueue,
            /* [retval][out] */ IJMSQueueBrowser **ppBrowser);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createBrowser2 )( 
            IJMSQueueSession * This,
            /* [in] */ IJMSQueue *pQueue,
            /* [in] */ BSTR Selector,
            /* [retval][out] */ IJMSQueueBrowser **ppBrowser);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createTemporaryQueue )( 
            IJMSQueueSession * This,
            /* [retval][out] */ IJMSTemporaryQueue **ppTempQueue);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setFlowControlDisabled )( 
            IJMSQueueSession * This,
            /* [in] */ BOOL disabled);
        
        END_INTERFACE
    } IJMSQueueSessionVtbl;

    interface IJMSQueueSession
    {
        CONST_VTBL struct IJMSQueueSessionVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSQueueSession_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSQueueSession_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSQueueSession_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSQueueSession_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSQueueSession_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSQueueSession_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSQueueSession_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSQueueSession_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSQueueSession_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSQueueSession_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSQueueSession_run(This)	\
    (This)->lpVtbl -> run(This)

#define IJMSQueueSession_commit(This)	\
    (This)->lpVtbl -> commit(This)

#define IJMSQueueSession_close(This)	\
    (This)->lpVtbl -> close(This)

#define IJMSQueueSession_rollback(This)	\
    (This)->lpVtbl -> rollback(This)

#define IJMSQueueSession_recover(This)	\
    (This)->lpVtbl -> recover(This)

#define IJMSQueueSession_createMessage(This,ppMessage)	\
    (This)->lpVtbl -> createMessage(This,ppMessage)

#define IJMSQueueSession_createBytesMessage(This,pMessage)	\
    (This)->lpVtbl -> createBytesMessage(This,pMessage)

#define IJMSQueueSession_createObjectMessage(This,pMessage)	\
    (This)->lpVtbl -> createObjectMessage(This,pMessage)

#define IJMSQueueSession_createObjectMessage2(This,inputObject,pMessage)	\
    (This)->lpVtbl -> createObjectMessage2(This,inputObject,pMessage)

#define IJMSQueueSession_createStreamMessage(This,pMessage)	\
    (This)->lpVtbl -> createStreamMessage(This,pMessage)

#define IJMSQueueSession_createTextMessage(This,pMessage)	\
    (This)->lpVtbl -> createTextMessage(This,pMessage)

#define IJMSQueueSession_createTextMessage2(This,inputText,pMessage)	\
    (This)->lpVtbl -> createTextMessage2(This,inputText,pMessage)

#define IJMSQueueSession_getTransacted(This,transacted)	\
    (This)->lpVtbl -> getTransacted(This,transacted)

#define IJMSQueueSession_setMessageListener(This,pListener)	\
    (This)->lpVtbl -> setMessageListener(This,pListener)

#define IJMSQueueSession_setMessageListener2(This)	\
    (This)->lpVtbl -> setMessageListener2(This)

#define IJMSQueueSession_resetMessageListener(This)	\
    (This)->lpVtbl -> resetMessageListener(This)

#define IJMSQueueSession_getTxnBatchSize(This,transactionBatchBufferSize)	\
    (This)->lpVtbl -> getTxnBatchSize(This,transactionBatchBufferSize)

#define IJMSQueueSession_setTxnBatchSize(This,transactionBatchBufferSize)	\
    (This)->lpVtbl -> setTxnBatchSize(This,transactionBatchBufferSize)


#define IJMSQueueSession_createQueue(This,queueName,ppQueue)	\
    (This)->lpVtbl -> createQueue(This,queueName,ppQueue)

#define IJMSQueueSession_createReceiver(This,pQueue,ppReceiver)	\
    (This)->lpVtbl -> createReceiver(This,pQueue,ppReceiver)

#define IJMSQueueSession_createReceiver2(This,pQueue,Selector,ppReceiver)	\
    (This)->lpVtbl -> createReceiver2(This,pQueue,Selector,ppReceiver)

#define IJMSQueueSession_createSender(This,pQueue,ppSender)	\
    (This)->lpVtbl -> createSender(This,pQueue,ppSender)

#define IJMSQueueSession_createSender2(This,ppSender)	\
    (This)->lpVtbl -> createSender2(This,ppSender)

#define IJMSQueueSession_createBrowser(This,pQueue,ppBrowser)	\
    (This)->lpVtbl -> createBrowser(This,pQueue,ppBrowser)

#define IJMSQueueSession_createBrowser2(This,pQueue,Selector,ppBrowser)	\
    (This)->lpVtbl -> createBrowser2(This,pQueue,Selector,ppBrowser)

#define IJMSQueueSession_createTemporaryQueue(This,ppTempQueue)	\
    (This)->lpVtbl -> createTemporaryQueue(This,ppTempQueue)

#define IJMSQueueSession_setFlowControlDisabled(This,disabled)	\
    (This)->lpVtbl -> setFlowControlDisabled(This,disabled)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueSession_createQueue_Proxy( 
    IJMSQueueSession * This,
    /* [in] */ BSTR queueName,
    /* [retval][out] */ IJMSQueue **ppQueue);


void __RPC_STUB IJMSQueueSession_createQueue_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueSession_createReceiver_Proxy( 
    IJMSQueueSession * This,
    /* [in] */ IJMSQueue *pQueue,
    /* [retval][out] */ IJMSQueueReceiver **ppReceiver);


void __RPC_STUB IJMSQueueSession_createReceiver_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueSession_createReceiver2_Proxy( 
    IJMSQueueSession * This,
    /* [in] */ IJMSQueue *pQueue,
    /* [in] */ BSTR Selector,
    /* [retval][out] */ IJMSQueueReceiver **ppReceiver);


void __RPC_STUB IJMSQueueSession_createReceiver2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueSession_createSender_Proxy( 
    IJMSQueueSession * This,
    /* [in] */ IJMSQueue *pQueue,
    /* [retval][out] */ IJMSQueueSender **ppSender);


void __RPC_STUB IJMSQueueSession_createSender_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueSession_createSender2_Proxy( 
    IJMSQueueSession * This,
    /* [retval][out] */ IJMSQueueSender **ppSender);


void __RPC_STUB IJMSQueueSession_createSender2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueSession_createBrowser_Proxy( 
    IJMSQueueSession * This,
    /* [in] */ IJMSQueue *pQueue,
    /* [retval][out] */ IJMSQueueBrowser **ppBrowser);


void __RPC_STUB IJMSQueueSession_createBrowser_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueSession_createBrowser2_Proxy( 
    IJMSQueueSession * This,
    /* [in] */ IJMSQueue *pQueue,
    /* [in] */ BSTR Selector,
    /* [retval][out] */ IJMSQueueBrowser **ppBrowser);


void __RPC_STUB IJMSQueueSession_createBrowser2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueSession_createTemporaryQueue_Proxy( 
    IJMSQueueSession * This,
    /* [retval][out] */ IJMSTemporaryQueue **ppTempQueue);


void __RPC_STUB IJMSQueueSession_createTemporaryQueue_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueSession_setFlowControlDisabled_Proxy( 
    IJMSQueueSession * This,
    /* [in] */ BOOL disabled);


void __RPC_STUB IJMSQueueSession_setFlowControlDisabled_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSQueueSession_INTERFACE_DEFINED__ */


#ifndef __IJMSTopicSession_INTERFACE_DEFINED__
#define __IJMSTopicSession_INTERFACE_DEFINED__

/* interface IJMSTopicSession */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSTopicSession;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("E243AA1D-3567-4E1D-8177-2015C010FFA1")
    IJMSTopicSession : public IJMSSession
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createTopic( 
            /* [in] */ BSTR topicName,
            /* [retval][out] */ IJMSTopic **ppTopic) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createSubscriber( 
            /* [in] */ IJMSTopic *pTopic,
            /* [retval][out] */ IJMSTopicSubscriber **ppSubscriber) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createSubscriber2( 
            /* [in] */ IJMSTopic *pTopic,
            /* [in] */ BSTR Selector,
            /* [in] */ BOOL noLocal,
            /* [retval][out] */ IJMSTopicSubscriber **ppSubscriber) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createDurableSubscriber( 
            /* [in] */ IJMSTopic *pTopic,
            /* [in] */ BSTR name,
            /* [retval][out] */ IJMSDurableSubscriber **ppSubscriber) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createDurableSubscriber2( 
            /* [in] */ IJMSTopic *pTopic,
            /* [in] */ BSTR name,
            /* [in] */ BSTR Selector,
            /* [in] */ BOOL noLocal,
            /* [retval][out] */ IJMSDurableSubscriber **ppSubscriber) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createPublisher( 
            /* [in] */ IJMSTopic *pTopic,
            /* [retval][out] */ IJMSTopicPublisher **ppPublisher) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createPublisher2( 
            /* [retval][out] */ IJMSTopicPublisher **ppPublisher) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE createTemporaryTopic( 
            /* [retval][out] */ IJMSTemporaryTopic **ppTempTopic) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE unsubscribe( 
            /* [in] */ BSTR subjName) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setFlowControlDisabled( 
            /* [in] */ BOOL disabled) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSTopicSessionVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSTopicSession * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSTopicSession * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSTopicSession * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSTopicSession * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSTopicSession * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSTopicSession * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSTopicSession * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSTopicSession * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSTopicSession * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSTopicSession * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *run )( 
            IJMSTopicSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *commit )( 
            IJMSTopicSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *close )( 
            IJMSTopicSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *rollback )( 
            IJMSTopicSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *recover )( 
            IJMSTopicSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createMessage )( 
            IJMSTopicSession * This,
            /* [retval][out] */ IJMSMessage **ppMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createBytesMessage )( 
            IJMSTopicSession * This,
            /* [retval][out] */ IJMSBytesMessage **pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createObjectMessage )( 
            IJMSTopicSession * This,
            /* [retval][out] */ IJMSObjectMessage **pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createObjectMessage2 )( 
            IJMSTopicSession * This,
            /* [in] */ VARIANT inputObject,
            /* [retval][out] */ IJMSObjectMessage **pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createStreamMessage )( 
            IJMSTopicSession * This,
            /* [retval][out] */ IJMSStreamMessage **pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createTextMessage )( 
            IJMSTopicSession * This,
            /* [retval][out] */ IJMSTextMessage **pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createTextMessage2 )( 
            IJMSTopicSession * This,
            /* [in] */ BSTR inputText,
            /* [retval][out] */ IJMSTextMessage **pMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getTransacted )( 
            IJMSTopicSession * This,
            /* [retval][out] */ BOOL *transacted);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setMessageListener )( 
            IJMSTopicSession * This,
            /* [in] */ IUnknown *pListener);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setMessageListener2 )( 
            IJMSTopicSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *resetMessageListener )( 
            IJMSTopicSession * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getTxnBatchSize )( 
            IJMSTopicSession * This,
            /* [retval][out] */ long *transactionBatchBufferSize);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setTxnBatchSize )( 
            IJMSTopicSession * This,
            /* [in] */ long transactionBatchBufferSize);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createTopic )( 
            IJMSTopicSession * This,
            /* [in] */ BSTR topicName,
            /* [retval][out] */ IJMSTopic **ppTopic);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createSubscriber )( 
            IJMSTopicSession * This,
            /* [in] */ IJMSTopic *pTopic,
            /* [retval][out] */ IJMSTopicSubscriber **ppSubscriber);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createSubscriber2 )( 
            IJMSTopicSession * This,
            /* [in] */ IJMSTopic *pTopic,
            /* [in] */ BSTR Selector,
            /* [in] */ BOOL noLocal,
            /* [retval][out] */ IJMSTopicSubscriber **ppSubscriber);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createDurableSubscriber )( 
            IJMSTopicSession * This,
            /* [in] */ IJMSTopic *pTopic,
            /* [in] */ BSTR name,
            /* [retval][out] */ IJMSDurableSubscriber **ppSubscriber);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createDurableSubscriber2 )( 
            IJMSTopicSession * This,
            /* [in] */ IJMSTopic *pTopic,
            /* [in] */ BSTR name,
            /* [in] */ BSTR Selector,
            /* [in] */ BOOL noLocal,
            /* [retval][out] */ IJMSDurableSubscriber **ppSubscriber);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createPublisher )( 
            IJMSTopicSession * This,
            /* [in] */ IJMSTopic *pTopic,
            /* [retval][out] */ IJMSTopicPublisher **ppPublisher);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createPublisher2 )( 
            IJMSTopicSession * This,
            /* [retval][out] */ IJMSTopicPublisher **ppPublisher);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *createTemporaryTopic )( 
            IJMSTopicSession * This,
            /* [retval][out] */ IJMSTemporaryTopic **ppTempTopic);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *unsubscribe )( 
            IJMSTopicSession * This,
            /* [in] */ BSTR subjName);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setFlowControlDisabled )( 
            IJMSTopicSession * This,
            /* [in] */ BOOL disabled);
        
        END_INTERFACE
    } IJMSTopicSessionVtbl;

    interface IJMSTopicSession
    {
        CONST_VTBL struct IJMSTopicSessionVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSTopicSession_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSTopicSession_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSTopicSession_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSTopicSession_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSTopicSession_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSTopicSession_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSTopicSession_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSTopicSession_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSTopicSession_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSTopicSession_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSTopicSession_run(This)	\
    (This)->lpVtbl -> run(This)

#define IJMSTopicSession_commit(This)	\
    (This)->lpVtbl -> commit(This)

#define IJMSTopicSession_close(This)	\
    (This)->lpVtbl -> close(This)

#define IJMSTopicSession_rollback(This)	\
    (This)->lpVtbl -> rollback(This)

#define IJMSTopicSession_recover(This)	\
    (This)->lpVtbl -> recover(This)

#define IJMSTopicSession_createMessage(This,ppMessage)	\
    (This)->lpVtbl -> createMessage(This,ppMessage)

#define IJMSTopicSession_createBytesMessage(This,pMessage)	\
    (This)->lpVtbl -> createBytesMessage(This,pMessage)

#define IJMSTopicSession_createObjectMessage(This,pMessage)	\
    (This)->lpVtbl -> createObjectMessage(This,pMessage)

#define IJMSTopicSession_createObjectMessage2(This,inputObject,pMessage)	\
    (This)->lpVtbl -> createObjectMessage2(This,inputObject,pMessage)

#define IJMSTopicSession_createStreamMessage(This,pMessage)	\
    (This)->lpVtbl -> createStreamMessage(This,pMessage)

#define IJMSTopicSession_createTextMessage(This,pMessage)	\
    (This)->lpVtbl -> createTextMessage(This,pMessage)

#define IJMSTopicSession_createTextMessage2(This,inputText,pMessage)	\
    (This)->lpVtbl -> createTextMessage2(This,inputText,pMessage)

#define IJMSTopicSession_getTransacted(This,transacted)	\
    (This)->lpVtbl -> getTransacted(This,transacted)

#define IJMSTopicSession_setMessageListener(This,pListener)	\
    (This)->lpVtbl -> setMessageListener(This,pListener)

#define IJMSTopicSession_setMessageListener2(This)	\
    (This)->lpVtbl -> setMessageListener2(This)

#define IJMSTopicSession_resetMessageListener(This)	\
    (This)->lpVtbl -> resetMessageListener(This)

#define IJMSTopicSession_getTxnBatchSize(This,transactionBatchBufferSize)	\
    (This)->lpVtbl -> getTxnBatchSize(This,transactionBatchBufferSize)

#define IJMSTopicSession_setTxnBatchSize(This,transactionBatchBufferSize)	\
    (This)->lpVtbl -> setTxnBatchSize(This,transactionBatchBufferSize)


#define IJMSTopicSession_createTopic(This,topicName,ppTopic)	\
    (This)->lpVtbl -> createTopic(This,topicName,ppTopic)

#define IJMSTopicSession_createSubscriber(This,pTopic,ppSubscriber)	\
    (This)->lpVtbl -> createSubscriber(This,pTopic,ppSubscriber)

#define IJMSTopicSession_createSubscriber2(This,pTopic,Selector,noLocal,ppSubscriber)	\
    (This)->lpVtbl -> createSubscriber2(This,pTopic,Selector,noLocal,ppSubscriber)

#define IJMSTopicSession_createDurableSubscriber(This,pTopic,name,ppSubscriber)	\
    (This)->lpVtbl -> createDurableSubscriber(This,pTopic,name,ppSubscriber)

#define IJMSTopicSession_createDurableSubscriber2(This,pTopic,name,Selector,noLocal,ppSubscriber)	\
    (This)->lpVtbl -> createDurableSubscriber2(This,pTopic,name,Selector,noLocal,ppSubscriber)

#define IJMSTopicSession_createPublisher(This,pTopic,ppPublisher)	\
    (This)->lpVtbl -> createPublisher(This,pTopic,ppPublisher)

#define IJMSTopicSession_createPublisher2(This,ppPublisher)	\
    (This)->lpVtbl -> createPublisher2(This,ppPublisher)

#define IJMSTopicSession_createTemporaryTopic(This,ppTempTopic)	\
    (This)->lpVtbl -> createTemporaryTopic(This,ppTempTopic)

#define IJMSTopicSession_unsubscribe(This,subjName)	\
    (This)->lpVtbl -> unsubscribe(This,subjName)

#define IJMSTopicSession_setFlowControlDisabled(This,disabled)	\
    (This)->lpVtbl -> setFlowControlDisabled(This,disabled)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicSession_createTopic_Proxy( 
    IJMSTopicSession * This,
    /* [in] */ BSTR topicName,
    /* [retval][out] */ IJMSTopic **ppTopic);


void __RPC_STUB IJMSTopicSession_createTopic_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicSession_createSubscriber_Proxy( 
    IJMSTopicSession * This,
    /* [in] */ IJMSTopic *pTopic,
    /* [retval][out] */ IJMSTopicSubscriber **ppSubscriber);


void __RPC_STUB IJMSTopicSession_createSubscriber_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicSession_createSubscriber2_Proxy( 
    IJMSTopicSession * This,
    /* [in] */ IJMSTopic *pTopic,
    /* [in] */ BSTR Selector,
    /* [in] */ BOOL noLocal,
    /* [retval][out] */ IJMSTopicSubscriber **ppSubscriber);


void __RPC_STUB IJMSTopicSession_createSubscriber2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicSession_createDurableSubscriber_Proxy( 
    IJMSTopicSession * This,
    /* [in] */ IJMSTopic *pTopic,
    /* [in] */ BSTR name,
    /* [retval][out] */ IJMSDurableSubscriber **ppSubscriber);


void __RPC_STUB IJMSTopicSession_createDurableSubscriber_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicSession_createDurableSubscriber2_Proxy( 
    IJMSTopicSession * This,
    /* [in] */ IJMSTopic *pTopic,
    /* [in] */ BSTR name,
    /* [in] */ BSTR Selector,
    /* [in] */ BOOL noLocal,
    /* [retval][out] */ IJMSDurableSubscriber **ppSubscriber);


void __RPC_STUB IJMSTopicSession_createDurableSubscriber2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicSession_createPublisher_Proxy( 
    IJMSTopicSession * This,
    /* [in] */ IJMSTopic *pTopic,
    /* [retval][out] */ IJMSTopicPublisher **ppPublisher);


void __RPC_STUB IJMSTopicSession_createPublisher_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicSession_createPublisher2_Proxy( 
    IJMSTopicSession * This,
    /* [retval][out] */ IJMSTopicPublisher **ppPublisher);


void __RPC_STUB IJMSTopicSession_createPublisher2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicSession_createTemporaryTopic_Proxy( 
    IJMSTopicSession * This,
    /* [retval][out] */ IJMSTemporaryTopic **ppTempTopic);


void __RPC_STUB IJMSTopicSession_createTemporaryTopic_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicSession_unsubscribe_Proxy( 
    IJMSTopicSession * This,
    /* [in] */ BSTR subjName);


void __RPC_STUB IJMSTopicSession_unsubscribe_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSTopicSession_setFlowControlDisabled_Proxy( 
    IJMSTopicSession * This,
    /* [in] */ BOOL disabled);


void __RPC_STUB IJMSTopicSession_setFlowControlDisabled_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSTopicSession_INTERFACE_DEFINED__ */


#ifndef __IJMSQueueBrowser_INTERFACE_DEFINED__
#define __IJMSQueueBrowser_INTERFACE_DEFINED__

/* interface IJMSQueueBrowser */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSQueueBrowser;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("5CBDE198-BD24-48BC-9315-8CC5ADED9F5F")
    IJMSQueueBrowser : public IJMSObject
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE close( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getQueue( 
            /* [retval][out] */ IJMSQueue **ppQueue) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getMessageSelector( 
            /* [retval][out] */ BSTR *messageSelector) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getEnumeration( 
            /* [retval][out] */ IJMSEnumeration **pEnum) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSQueueBrowserVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSQueueBrowser * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSQueueBrowser * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSQueueBrowser * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSQueueBrowser * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSQueueBrowser * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSQueueBrowser * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSQueueBrowser * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSQueueBrowser * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSQueueBrowser * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSQueueBrowser * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *close )( 
            IJMSQueueBrowser * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getQueue )( 
            IJMSQueueBrowser * This,
            /* [retval][out] */ IJMSQueue **ppQueue);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getMessageSelector )( 
            IJMSQueueBrowser * This,
            /* [retval][out] */ BSTR *messageSelector);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getEnumeration )( 
            IJMSQueueBrowser * This,
            /* [retval][out] */ IJMSEnumeration **pEnum);
        
        END_INTERFACE
    } IJMSQueueBrowserVtbl;

    interface IJMSQueueBrowser
    {
        CONST_VTBL struct IJMSQueueBrowserVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSQueueBrowser_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSQueueBrowser_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSQueueBrowser_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSQueueBrowser_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSQueueBrowser_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSQueueBrowser_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSQueueBrowser_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSQueueBrowser_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSQueueBrowser_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSQueueBrowser_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSQueueBrowser_close(This)	\
    (This)->lpVtbl -> close(This)

#define IJMSQueueBrowser_getQueue(This,ppQueue)	\
    (This)->lpVtbl -> getQueue(This,ppQueue)

#define IJMSQueueBrowser_getMessageSelector(This,messageSelector)	\
    (This)->lpVtbl -> getMessageSelector(This,messageSelector)

#define IJMSQueueBrowser_getEnumeration(This,pEnum)	\
    (This)->lpVtbl -> getEnumeration(This,pEnum)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueBrowser_close_Proxy( 
    IJMSQueueBrowser * This);


void __RPC_STUB IJMSQueueBrowser_close_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueBrowser_getQueue_Proxy( 
    IJMSQueueBrowser * This,
    /* [retval][out] */ IJMSQueue **ppQueue);


void __RPC_STUB IJMSQueueBrowser_getQueue_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueBrowser_getMessageSelector_Proxy( 
    IJMSQueueBrowser * This,
    /* [retval][out] */ BSTR *messageSelector);


void __RPC_STUB IJMSQueueBrowser_getMessageSelector_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSQueueBrowser_getEnumeration_Proxy( 
    IJMSQueueBrowser * This,
    /* [retval][out] */ IJMSEnumeration **pEnum);


void __RPC_STUB IJMSQueueBrowser_getEnumeration_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSQueueBrowser_INTERFACE_DEFINED__ */


#ifndef __IJMSDurableSubscriber_INTERFACE_DEFINED__
#define __IJMSDurableSubscriber_INTERFACE_DEFINED__

/* interface IJMSDurableSubscriber */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSDurableSubscriber;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("A5E02826-98FE-4537-9D6B-44D526C400BB")
    IJMSDurableSubscriber : public IJMSMessageConsumer
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getTopic( 
            /* [retval][out] */ IJMSTopic **ppTopic) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getNoLocal( 
            /* [retval][out] */ BOOL *pNoLocal) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSDurableSubscriberVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSDurableSubscriber * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSDurableSubscriber * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSDurableSubscriber * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSDurableSubscriber * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSDurableSubscriber * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSDurableSubscriber * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSDurableSubscriber * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSDurableSubscriber * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSDurableSubscriber * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSDurableSubscriber * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *close )( 
            IJMSDurableSubscriber * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setMessageListener )( 
            IJMSDurableSubscriber * This,
            /* [in] */ IUnknown *pMessageListener);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setMessageListener2 )( 
            IJMSDurableSubscriber * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *resetMessageListener )( 
            IJMSDurableSubscriber * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getMessageSelector )( 
            IJMSDurableSubscriber * This,
            /* [retval][out] */ BSTR *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *receive )( 
            IJMSDurableSubscriber * This,
            /* [retval][out] */ IJMSMessage **ppMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *receive2 )( 
            IJMSDurableSubscriber * This,
            /* [in] */ long timeout,
            /* [retval][out] */ IJMSMessage **ppMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *receiveNoWait )( 
            IJMSDurableSubscriber * This,
            /* [retval][out] */ IJMSMessage **ppMessage);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getTopic )( 
            IJMSDurableSubscriber * This,
            /* [retval][out] */ IJMSTopic **ppTopic);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getNoLocal )( 
            IJMSDurableSubscriber * This,
            /* [retval][out] */ BOOL *pNoLocal);
        
        END_INTERFACE
    } IJMSDurableSubscriberVtbl;

    interface IJMSDurableSubscriber
    {
        CONST_VTBL struct IJMSDurableSubscriberVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSDurableSubscriber_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSDurableSubscriber_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSDurableSubscriber_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSDurableSubscriber_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSDurableSubscriber_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSDurableSubscriber_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSDurableSubscriber_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSDurableSubscriber_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSDurableSubscriber_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSDurableSubscriber_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSDurableSubscriber_close(This)	\
    (This)->lpVtbl -> close(This)

#define IJMSDurableSubscriber_setMessageListener(This,pMessageListener)	\
    (This)->lpVtbl -> setMessageListener(This,pMessageListener)

#define IJMSDurableSubscriber_setMessageListener2(This)	\
    (This)->lpVtbl -> setMessageListener2(This)

#define IJMSDurableSubscriber_resetMessageListener(This)	\
    (This)->lpVtbl -> resetMessageListener(This)

#define IJMSDurableSubscriber_getMessageSelector(This,param)	\
    (This)->lpVtbl -> getMessageSelector(This,param)

#define IJMSDurableSubscriber_receive(This,ppMessage)	\
    (This)->lpVtbl -> receive(This,ppMessage)

#define IJMSDurableSubscriber_receive2(This,timeout,ppMessage)	\
    (This)->lpVtbl -> receive2(This,timeout,ppMessage)

#define IJMSDurableSubscriber_receiveNoWait(This,ppMessage)	\
    (This)->lpVtbl -> receiveNoWait(This,ppMessage)


#define IJMSDurableSubscriber_getTopic(This,ppTopic)	\
    (This)->lpVtbl -> getTopic(This,ppTopic)

#define IJMSDurableSubscriber_getNoLocal(This,pNoLocal)	\
    (This)->lpVtbl -> getNoLocal(This,pNoLocal)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSDurableSubscriber_getTopic_Proxy( 
    IJMSDurableSubscriber * This,
    /* [retval][out] */ IJMSTopic **ppTopic);


void __RPC_STUB IJMSDurableSubscriber_getTopic_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSDurableSubscriber_getNoLocal_Proxy( 
    IJMSDurableSubscriber * This,
    /* [retval][out] */ BOOL *pNoLocal);


void __RPC_STUB IJMSDurableSubscriber_getNoLocal_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSDurableSubscriber_INTERFACE_DEFINED__ */


#ifndef __IJMSMapMessage_INTERFACE_DEFINED__
#define __IJMSMapMessage_INTERFACE_DEFINED__

/* interface IJMSMapMessage */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IJMSMapMessage;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("A54D5E79-4E57-45C2-8DE7-5E4907FF2418")
    IJMSMapMessage : public IJMSMessage
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getBoolean( 
            /* [in] */ BSTR itemName,
            /* [retval][out] */ BOOL *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setBoolean( 
            /* [in] */ BSTR itemName,
            /* [in] */ BOOL param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getByte( 
            /* [in] */ BSTR itemName,
            /* [retval][out] */ VARIANT *pParam) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setByte( 
            /* [in] */ BSTR itemName,
            /* [in] */ VARIANT param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getBytes( 
            /* [in] */ BSTR itemName,
            /* [in] */ int length,
            /* [retval][out] */ VARIANT *pParam) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setBytes( 
            /* [in] */ BSTR itemName,
            /* [in] */ VARIANT param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setBytes2( 
            /* [in] */ BSTR itemName,
            /* [in] */ VARIANT param,
            /* [in] */ int offset,
            /* [in] */ int length) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getChar( 
            /* [in] */ BSTR itemName,
            /* [retval][out] */ VARIANT *pParam) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setChar( 
            /* [in] */ BSTR itemName,
            /* [in] */ VARIANT param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getShort( 
            /* [in] */ BSTR itemName,
            /* [retval][out] */ short *pParam) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setShort( 
            /* [in] */ BSTR itemName,
            /* [in] */ short param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getInt( 
            /* [in] */ BSTR itemName,
            /* [retval][out] */ int *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setInt( 
            /* [in] */ BSTR itemName,
            /* [in] */ int param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setLong( 
            /* [in] */ BSTR itemName,
            /* [in] */ long param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getLong( 
            /* [in] */ BSTR itemName,
            /* [retval][out] */ long *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getFloat( 
            /* [in] */ BSTR itemName,
            /* [retval][out] */ float *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setFloat( 
            /* [in] */ BSTR itemName,
            /* [in] */ float param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getDouble( 
            /* [in] */ BSTR itemName,
            /* [retval][out] */ double *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setDouble( 
            /* [in] */ BSTR itemName,
            /* [in] */ double param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getString( 
            /* [in] */ BSTR itemName,
            /* [retval][out] */ BSTR *param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setString( 
            /* [in] */ BSTR itemName,
            /* [in] */ BSTR param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE setObject( 
            /* [in] */ BSTR itemName,
            /* [in] */ VARIANT param) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE getMapNames( 
            /* [retval][out] */ IJMSEnumeration **pMapNames) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE itemExists( 
            /* [in] */ BSTR itemName,
            /* [retval][out] */ BOOL *pExists) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSMapMessageVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSMapMessage * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSMapMessage * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSMapMessage * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSMapMessage * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSMapMessage * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSMapMessage * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSMapMessage * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *equals )( 
            IJMSMapMessage * This,
            /* [in] */ IJMSObject *obj,
            /* [retval][out] */ BOOL *isEqual);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clone )( 
            IJMSMapMessage * This,
            /* [retval][out] */ IJMSObject **clonedObj);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *toString )( 
            IJMSMapMessage * This,
            /* [retval][out] */ BSTR *objDesc);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *acknowledge )( 
            IJMSMapMessage * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clearBody )( 
            IJMSMapMessage * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSDestination )( 
            IJMSMapMessage * This,
            /* [retval][out] */ IJMSDestination **ppDest);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSDestination )( 
            IJMSMapMessage * This,
            /* [in] */ IJMSDestination *pDest);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSDeliveryMode )( 
            IJMSMapMessage * This,
            /* [retval][out] */ enum JMSDeliveryMode *pDelMode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSDeliveryMode )( 
            IJMSMapMessage * This,
            /* [in] */ enum JMSDeliveryMode pDelMode);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSMessageID )( 
            IJMSMapMessage * This,
            /* [retval][out] */ BSTR *pMessageID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSMessageID )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR messageID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSTimestamp )( 
            IJMSMapMessage * This,
            /* [retval][out] */ long *pTimestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSTimestamp )( 
            IJMSMapMessage * This,
            /* [in] */ long timestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSExpiration )( 
            IJMSMapMessage * This,
            /* [retval][out] */ long *pExpiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSExpiration )( 
            IJMSMapMessage * This,
            /* [in] */ long expiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSRedelivered )( 
            IJMSMapMessage * This,
            /* [retval][out] */ BOOL *pRedelivered);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSRedelivered )( 
            IJMSMapMessage * This,
            /* [in] */ BOOL redelivered);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSPriority )( 
            IJMSMapMessage * This,
            /* [retval][out] */ int *pPriority);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSPriority )( 
            IJMSMapMessage * This,
            /* [in] */ int priority);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSReplyTo )( 
            IJMSMapMessage * This,
            /* [retval][out] */ IJMSDestination **destination);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSReplyTo )( 
            IJMSMapMessage * This,
            /* [in] */ IJMSDestination *destination);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSCorrelationID )( 
            IJMSMapMessage * This,
            /* [retval][out] */ BSTR *pCorrelationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSCorrelationID )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR correlationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSCorrelationIDAsBytes )( 
            IJMSMapMessage * This,
            /* [retval][out] */ VARIANT *pCorrelationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSCorrelationIDAsBytes )( 
            IJMSMapMessage * This,
            /* [in] */ VARIANT correlationID);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSType )( 
            IJMSMapMessage * This,
            /* [retval][out] */ BSTR *pJMSType);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSType )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR JMSType);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getStringProperty )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ BSTR *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setStringProperty )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ BSTR property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getIntProperty )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ int *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setIntProperty )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ int property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBooleanProperty )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ BOOL *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setBooleanProperty )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ BOOL property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDoubleProperty )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ double *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setDoubleProperty )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ double property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getFloatProperty )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ float *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setFloatProperty )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ float property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getByteProperty )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ VARIANT *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setByteProperty )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ VARIANT property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getLongProperty )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ long *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setLongProperty )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ long property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getShortProperty )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ short *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setShortProperty )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ short property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getObjectProperty )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ VARIANT *pProperty);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setObjectProperty )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [in] */ VARIANT property);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *clearProperties )( 
            IJMSMapMessage * This);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getPropertyNames )( 
            IJMSMapMessage * This,
            /* [retval][out] */ IJMSEnumeration **pPropertyNames);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *propertyExists )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR propName,
            /* [retval][out] */ BOOL *pExists);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBodySize )( 
            IJMSMapMessage * This,
            /* [retval][out] */ long *pSize);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSTimestampLow )( 
            IJMSMapMessage * This,
            /* [retval][out] */ long *pTimestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSTimestampLow )( 
            IJMSMapMessage * This,
            /* [in] */ long timestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSExpirationLow )( 
            IJMSMapMessage * This,
            /* [retval][out] */ long *pExpiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSExpirationLow )( 
            IJMSMapMessage * This,
            /* [in] */ long expiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSTimestampHigh )( 
            IJMSMapMessage * This,
            /* [retval][out] */ long *pTimestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSTimestampHigh )( 
            IJMSMapMessage * This,
            /* [in] */ long timestamp);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getJMSExpirationHigh )( 
            IJMSMapMessage * This,
            /* [retval][out] */ long *pExpiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setJMSExpirationHigh )( 
            IJMSMapMessage * This,
            /* [in] */ long expiration);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBoolean )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [retval][out] */ BOOL *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setBoolean )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [in] */ BOOL param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getByte )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [retval][out] */ VARIANT *pParam);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setByte )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [in] */ VARIANT param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getBytes )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [in] */ int length,
            /* [retval][out] */ VARIANT *pParam);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setBytes )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [in] */ VARIANT param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setBytes2 )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [in] */ VARIANT param,
            /* [in] */ int offset,
            /* [in] */ int length);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getChar )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [retval][out] */ VARIANT *pParam);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setChar )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [in] */ VARIANT param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getShort )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [retval][out] */ short *pParam);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setShort )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [in] */ short param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getInt )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [retval][out] */ int *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setInt )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [in] */ int param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setLong )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [in] */ long param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getLong )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [retval][out] */ long *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getFloat )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [retval][out] */ float *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setFloat )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [in] */ float param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getDouble )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [retval][out] */ double *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setDouble )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [in] */ double param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getString )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [retval][out] */ BSTR *param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setString )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [in] */ BSTR param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *setObject )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [in] */ VARIANT param);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *getMapNames )( 
            IJMSMapMessage * This,
            /* [retval][out] */ IJMSEnumeration **pMapNames);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *itemExists )( 
            IJMSMapMessage * This,
            /* [in] */ BSTR itemName,
            /* [retval][out] */ BOOL *pExists);
        
        END_INTERFACE
    } IJMSMapMessageVtbl;

    interface IJMSMapMessage
    {
        CONST_VTBL struct IJMSMapMessageVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSMapMessage_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSMapMessage_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSMapMessage_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSMapMessage_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSMapMessage_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSMapMessage_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSMapMessage_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSMapMessage_equals(This,obj,isEqual)	\
    (This)->lpVtbl -> equals(This,obj,isEqual)

#define IJMSMapMessage_clone(This,clonedObj)	\
    (This)->lpVtbl -> clone(This,clonedObj)

#define IJMSMapMessage_toString(This,objDesc)	\
    (This)->lpVtbl -> toString(This,objDesc)


#define IJMSMapMessage_acknowledge(This)	\
    (This)->lpVtbl -> acknowledge(This)

#define IJMSMapMessage_clearBody(This)	\
    (This)->lpVtbl -> clearBody(This)

#define IJMSMapMessage_getJMSDestination(This,ppDest)	\
    (This)->lpVtbl -> getJMSDestination(This,ppDest)

#define IJMSMapMessage_setJMSDestination(This,pDest)	\
    (This)->lpVtbl -> setJMSDestination(This,pDest)

#define IJMSMapMessage_getJMSDeliveryMode(This,pDelMode)	\
    (This)->lpVtbl -> getJMSDeliveryMode(This,pDelMode)

#define IJMSMapMessage_setJMSDeliveryMode(This,pDelMode)	\
    (This)->lpVtbl -> setJMSDeliveryMode(This,pDelMode)

#define IJMSMapMessage_getJMSMessageID(This,pMessageID)	\
    (This)->lpVtbl -> getJMSMessageID(This,pMessageID)

#define IJMSMapMessage_setJMSMessageID(This,messageID)	\
    (This)->lpVtbl -> setJMSMessageID(This,messageID)

#define IJMSMapMessage_getJMSTimestamp(This,pTimestamp)	\
    (This)->lpVtbl -> getJMSTimestamp(This,pTimestamp)

#define IJMSMapMessage_setJMSTimestamp(This,timestamp)	\
    (This)->lpVtbl -> setJMSTimestamp(This,timestamp)

#define IJMSMapMessage_getJMSExpiration(This,pExpiration)	\
    (This)->lpVtbl -> getJMSExpiration(This,pExpiration)

#define IJMSMapMessage_setJMSExpiration(This,expiration)	\
    (This)->lpVtbl -> setJMSExpiration(This,expiration)

#define IJMSMapMessage_getJMSRedelivered(This,pRedelivered)	\
    (This)->lpVtbl -> getJMSRedelivered(This,pRedelivered)

#define IJMSMapMessage_setJMSRedelivered(This,redelivered)	\
    (This)->lpVtbl -> setJMSRedelivered(This,redelivered)

#define IJMSMapMessage_getJMSPriority(This,pPriority)	\
    (This)->lpVtbl -> getJMSPriority(This,pPriority)

#define IJMSMapMessage_setJMSPriority(This,priority)	\
    (This)->lpVtbl -> setJMSPriority(This,priority)

#define IJMSMapMessage_getJMSReplyTo(This,destination)	\
    (This)->lpVtbl -> getJMSReplyTo(This,destination)

#define IJMSMapMessage_setJMSReplyTo(This,destination)	\
    (This)->lpVtbl -> setJMSReplyTo(This,destination)

#define IJMSMapMessage_getJMSCorrelationID(This,pCorrelationID)	\
    (This)->lpVtbl -> getJMSCorrelationID(This,pCorrelationID)

#define IJMSMapMessage_setJMSCorrelationID(This,correlationID)	\
    (This)->lpVtbl -> setJMSCorrelationID(This,correlationID)

#define IJMSMapMessage_getJMSCorrelationIDAsBytes(This,pCorrelationID)	\
    (This)->lpVtbl -> getJMSCorrelationIDAsBytes(This,pCorrelationID)

#define IJMSMapMessage_setJMSCorrelationIDAsBytes(This,correlationID)	\
    (This)->lpVtbl -> setJMSCorrelationIDAsBytes(This,correlationID)

#define IJMSMapMessage_getJMSType(This,pJMSType)	\
    (This)->lpVtbl -> getJMSType(This,pJMSType)

#define IJMSMapMessage_setJMSType(This,JMSType)	\
    (This)->lpVtbl -> setJMSType(This,JMSType)

#define IJMSMapMessage_getStringProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getStringProperty(This,propName,pProperty)

#define IJMSMapMessage_setStringProperty(This,propName,property)	\
    (This)->lpVtbl -> setStringProperty(This,propName,property)

#define IJMSMapMessage_getIntProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getIntProperty(This,propName,pProperty)

#define IJMSMapMessage_setIntProperty(This,propName,property)	\
    (This)->lpVtbl -> setIntProperty(This,propName,property)

#define IJMSMapMessage_getBooleanProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getBooleanProperty(This,propName,pProperty)

#define IJMSMapMessage_setBooleanProperty(This,propName,property)	\
    (This)->lpVtbl -> setBooleanProperty(This,propName,property)

#define IJMSMapMessage_getDoubleProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getDoubleProperty(This,propName,pProperty)

#define IJMSMapMessage_setDoubleProperty(This,propName,property)	\
    (This)->lpVtbl -> setDoubleProperty(This,propName,property)

#define IJMSMapMessage_getFloatProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getFloatProperty(This,propName,pProperty)

#define IJMSMapMessage_setFloatProperty(This,propName,property)	\
    (This)->lpVtbl -> setFloatProperty(This,propName,property)

#define IJMSMapMessage_getByteProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getByteProperty(This,propName,pProperty)

#define IJMSMapMessage_setByteProperty(This,propName,property)	\
    (This)->lpVtbl -> setByteProperty(This,propName,property)

#define IJMSMapMessage_getLongProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getLongProperty(This,propName,pProperty)

#define IJMSMapMessage_setLongProperty(This,propName,property)	\
    (This)->lpVtbl -> setLongProperty(This,propName,property)

#define IJMSMapMessage_getShortProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getShortProperty(This,propName,pProperty)

#define IJMSMapMessage_setShortProperty(This,propName,property)	\
    (This)->lpVtbl -> setShortProperty(This,propName,property)

#define IJMSMapMessage_getObjectProperty(This,propName,pProperty)	\
    (This)->lpVtbl -> getObjectProperty(This,propName,pProperty)

#define IJMSMapMessage_setObjectProperty(This,propName,property)	\
    (This)->lpVtbl -> setObjectProperty(This,propName,property)

#define IJMSMapMessage_clearProperties(This)	\
    (This)->lpVtbl -> clearProperties(This)

#define IJMSMapMessage_getPropertyNames(This,pPropertyNames)	\
    (This)->lpVtbl -> getPropertyNames(This,pPropertyNames)

#define IJMSMapMessage_propertyExists(This,propName,pExists)	\
    (This)->lpVtbl -> propertyExists(This,propName,pExists)

#define IJMSMapMessage_getBodySize(This,pSize)	\
    (This)->lpVtbl -> getBodySize(This,pSize)

#define IJMSMapMessage_getJMSTimestampLow(This,pTimestamp)	\
    (This)->lpVtbl -> getJMSTimestampLow(This,pTimestamp)

#define IJMSMapMessage_setJMSTimestampLow(This,timestamp)	\
    (This)->lpVtbl -> setJMSTimestampLow(This,timestamp)

#define IJMSMapMessage_getJMSExpirationLow(This,pExpiration)	\
    (This)->lpVtbl -> getJMSExpirationLow(This,pExpiration)

#define IJMSMapMessage_setJMSExpirationLow(This,expiration)	\
    (This)->lpVtbl -> setJMSExpirationLow(This,expiration)

#define IJMSMapMessage_getJMSTimestampHigh(This,pTimestamp)	\
    (This)->lpVtbl -> getJMSTimestampHigh(This,pTimestamp)

#define IJMSMapMessage_setJMSTimestampHigh(This,timestamp)	\
    (This)->lpVtbl -> setJMSTimestampHigh(This,timestamp)

#define IJMSMapMessage_getJMSExpirationHigh(This,pExpiration)	\
    (This)->lpVtbl -> getJMSExpirationHigh(This,pExpiration)

#define IJMSMapMessage_setJMSExpirationHigh(This,expiration)	\
    (This)->lpVtbl -> setJMSExpirationHigh(This,expiration)


#define IJMSMapMessage_getBoolean(This,itemName,param)	\
    (This)->lpVtbl -> getBoolean(This,itemName,param)

#define IJMSMapMessage_setBoolean(This,itemName,param)	\
    (This)->lpVtbl -> setBoolean(This,itemName,param)

#define IJMSMapMessage_getByte(This,itemName,pParam)	\
    (This)->lpVtbl -> getByte(This,itemName,pParam)

#define IJMSMapMessage_setByte(This,itemName,param)	\
    (This)->lpVtbl -> setByte(This,itemName,param)

#define IJMSMapMessage_getBytes(This,itemName,length,pParam)	\
    (This)->lpVtbl -> getBytes(This,itemName,length,pParam)

#define IJMSMapMessage_setBytes(This,itemName,param)	\
    (This)->lpVtbl -> setBytes(This,itemName,param)

#define IJMSMapMessage_setBytes2(This,itemName,param,offset,length)	\
    (This)->lpVtbl -> setBytes2(This,itemName,param,offset,length)

#define IJMSMapMessage_getChar(This,itemName,pParam)	\
    (This)->lpVtbl -> getChar(This,itemName,pParam)

#define IJMSMapMessage_setChar(This,itemName,param)	\
    (This)->lpVtbl -> setChar(This,itemName,param)

#define IJMSMapMessage_getShort(This,itemName,pParam)	\
    (This)->lpVtbl -> getShort(This,itemName,pParam)

#define IJMSMapMessage_setShort(This,itemName,param)	\
    (This)->lpVtbl -> setShort(This,itemName,param)

#define IJMSMapMessage_getInt(This,itemName,param)	\
    (This)->lpVtbl -> getInt(This,itemName,param)

#define IJMSMapMessage_setInt(This,itemName,param)	\
    (This)->lpVtbl -> setInt(This,itemName,param)

#define IJMSMapMessage_setLong(This,itemName,param)	\
    (This)->lpVtbl -> setLong(This,itemName,param)

#define IJMSMapMessage_getLong(This,itemName,param)	\
    (This)->lpVtbl -> getLong(This,itemName,param)

#define IJMSMapMessage_getFloat(This,itemName,param)	\
    (This)->lpVtbl -> getFloat(This,itemName,param)

#define IJMSMapMessage_setFloat(This,itemName,param)	\
    (This)->lpVtbl -> setFloat(This,itemName,param)

#define IJMSMapMessage_getDouble(This,itemName,param)	\
    (This)->lpVtbl -> getDouble(This,itemName,param)

#define IJMSMapMessage_setDouble(This,itemName,param)	\
    (This)->lpVtbl -> setDouble(This,itemName,param)

#define IJMSMapMessage_getString(This,itemName,param)	\
    (This)->lpVtbl -> getString(This,itemName,param)

#define IJMSMapMessage_setString(This,itemName,param)	\
    (This)->lpVtbl -> setString(This,itemName,param)

#define IJMSMapMessage_setObject(This,itemName,param)	\
    (This)->lpVtbl -> setObject(This,itemName,param)

#define IJMSMapMessage_getMapNames(This,pMapNames)	\
    (This)->lpVtbl -> getMapNames(This,pMapNames)

#define IJMSMapMessage_itemExists(This,itemName,pExists)	\
    (This)->lpVtbl -> itemExists(This,itemName,pExists)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_getBoolean_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [retval][out] */ BOOL *param);


void __RPC_STUB IJMSMapMessage_getBoolean_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_setBoolean_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [in] */ BOOL param);


void __RPC_STUB IJMSMapMessage_setBoolean_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_getByte_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [retval][out] */ VARIANT *pParam);


void __RPC_STUB IJMSMapMessage_getByte_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_setByte_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [in] */ VARIANT param);


void __RPC_STUB IJMSMapMessage_setByte_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_getBytes_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [in] */ int length,
    /* [retval][out] */ VARIANT *pParam);


void __RPC_STUB IJMSMapMessage_getBytes_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_setBytes_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [in] */ VARIANT param);


void __RPC_STUB IJMSMapMessage_setBytes_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_setBytes2_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [in] */ VARIANT param,
    /* [in] */ int offset,
    /* [in] */ int length);


void __RPC_STUB IJMSMapMessage_setBytes2_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_getChar_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [retval][out] */ VARIANT *pParam);


void __RPC_STUB IJMSMapMessage_getChar_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_setChar_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [in] */ VARIANT param);


void __RPC_STUB IJMSMapMessage_setChar_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_getShort_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [retval][out] */ short *pParam);


void __RPC_STUB IJMSMapMessage_getShort_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_setShort_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [in] */ short param);


void __RPC_STUB IJMSMapMessage_setShort_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_getInt_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [retval][out] */ int *param);


void __RPC_STUB IJMSMapMessage_getInt_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_setInt_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [in] */ int param);


void __RPC_STUB IJMSMapMessage_setInt_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_setLong_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [in] */ long param);


void __RPC_STUB IJMSMapMessage_setLong_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_getLong_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [retval][out] */ long *param);


void __RPC_STUB IJMSMapMessage_getLong_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_getFloat_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [retval][out] */ float *param);


void __RPC_STUB IJMSMapMessage_getFloat_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_setFloat_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [in] */ float param);


void __RPC_STUB IJMSMapMessage_setFloat_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_getDouble_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [retval][out] */ double *param);


void __RPC_STUB IJMSMapMessage_getDouble_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_setDouble_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [in] */ double param);


void __RPC_STUB IJMSMapMessage_setDouble_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_getString_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [retval][out] */ BSTR *param);


void __RPC_STUB IJMSMapMessage_getString_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_setString_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [in] */ BSTR param);


void __RPC_STUB IJMSMapMessage_setString_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_setObject_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [in] */ VARIANT param);


void __RPC_STUB IJMSMapMessage_setObject_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_getMapNames_Proxy( 
    IJMSMapMessage * This,
    /* [retval][out] */ IJMSEnumeration **pMapNames);


void __RPC_STUB IJMSMapMessage_getMapNames_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMapMessage_itemExists_Proxy( 
    IJMSMapMessage * This,
    /* [in] */ BSTR itemName,
    /* [retval][out] */ BOOL *pExists);


void __RPC_STUB IJMSMapMessage_itemExists_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSMapMessage_INTERFACE_DEFINED__ */



#ifndef __JMSCOMCLIENTLib_LIBRARY_DEFINED__
#define __JMSCOMCLIENTLib_LIBRARY_DEFINED__

/* library JMSCOMCLIENTLib */
/* [helpstring][version][uuid] */ 


EXTERN_C const IID LIBID_JMSCOMCLIENTLib;

#ifndef __IJMSMessageListener_INTERFACE_DEFINED__
#define __IJMSMessageListener_INTERFACE_DEFINED__

/* interface IJMSMessageListener */
/* [object][helpstring][uuid] */ 


EXTERN_C const IID IID_IJMSMessageListener;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("D8D4E87A-D70B-4FA3-96E4-B7FC83D0F51A")
    IJMSMessageListener : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE onMessage( 
            /* [in] */ IJMSMessage *pMessage) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSMessageListenerVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSMessageListener * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSMessageListener * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSMessageListener * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSMessageListener * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSMessageListener * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSMessageListener * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSMessageListener * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *onMessage )( 
            IJMSMessageListener * This,
            /* [in] */ IJMSMessage *pMessage);
        
        END_INTERFACE
    } IJMSMessageListenerVtbl;

    interface IJMSMessageListener
    {
        CONST_VTBL struct IJMSMessageListenerVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSMessageListener_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSMessageListener_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSMessageListener_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSMessageListener_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSMessageListener_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSMessageListener_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSMessageListener_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSMessageListener_onMessage(This,pMessage)	\
    (This)->lpVtbl -> onMessage(This,pMessage)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSMessageListener_onMessage_Proxy( 
    IJMSMessageListener * This,
    /* [in] */ IJMSMessage *pMessage);


void __RPC_STUB IJMSMessageListener_onMessage_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSMessageListener_INTERFACE_DEFINED__ */


#ifndef __IJMSExceptionListener_INTERFACE_DEFINED__
#define __IJMSExceptionListener_INTERFACE_DEFINED__

/* interface IJMSExceptionListener */
/* [object][helpstring][uuid] */ 


EXTERN_C const IID IID_IJMSExceptionListener;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("A0C2D943-20AC-4718-922A-0A4A4C74AA5E")
    IJMSExceptionListener : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE onException( 
            /* [in] */ IJMSThrowable *pException) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSExceptionListenerVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSExceptionListener * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSExceptionListener * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSExceptionListener * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSExceptionListener * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSExceptionListener * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSExceptionListener * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSExceptionListener * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *onException )( 
            IJMSExceptionListener * This,
            /* [in] */ IJMSThrowable *pException);
        
        END_INTERFACE
    } IJMSExceptionListenerVtbl;

    interface IJMSExceptionListener
    {
        CONST_VTBL struct IJMSExceptionListenerVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSExceptionListener_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSExceptionListener_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSExceptionListener_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSExceptionListener_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSExceptionListener_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSExceptionListener_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSExceptionListener_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSExceptionListener_onException(This,pException)	\
    (This)->lpVtbl -> onException(This,pException)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSExceptionListener_onException_Proxy( 
    IJMSExceptionListener * This,
    /* [in] */ IJMSThrowable *pException);


void __RPC_STUB IJMSExceptionListener_onException_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSExceptionListener_INTERFACE_DEFINED__ */


#ifndef __IJMSConnectionStateChangeListener_INTERFACE_DEFINED__
#define __IJMSConnectionStateChangeListener_INTERFACE_DEFINED__

/* interface IJMSConnectionStateChangeListener */
/* [object][helpstring][uuid] */ 


EXTERN_C const IID IID_IJMSConnectionStateChangeListener;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("EC276BCF-870F-4cb8-9446-2995533845DD")
    IJMSConnectionStateChangeListener : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE connectionStateChanged( 
            /* [in] */ int state) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IJMSConnectionStateChangeListenerVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IJMSConnectionStateChangeListener * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IJMSConnectionStateChangeListener * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IJMSConnectionStateChangeListener * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IJMSConnectionStateChangeListener * This,
            /* [out] */ UINT *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IJMSConnectionStateChangeListener * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IJMSConnectionStateChangeListener * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IJMSConnectionStateChangeListener * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS *pDispParams,
            /* [out] */ VARIANT *pVarResult,
            /* [out] */ EXCEPINFO *pExcepInfo,
            /* [out] */ UINT *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *connectionStateChanged )( 
            IJMSConnectionStateChangeListener * This,
            /* [in] */ int state);
        
        END_INTERFACE
    } IJMSConnectionStateChangeListenerVtbl;

    interface IJMSConnectionStateChangeListener
    {
        CONST_VTBL struct IJMSConnectionStateChangeListenerVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IJMSConnectionStateChangeListener_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IJMSConnectionStateChangeListener_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IJMSConnectionStateChangeListener_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IJMSConnectionStateChangeListener_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IJMSConnectionStateChangeListener_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IJMSConnectionStateChangeListener_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IJMSConnectionStateChangeListener_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IJMSConnectionStateChangeListener_connectionStateChanged(This,state)	\
    (This)->lpVtbl -> connectionStateChanged(This,state)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IJMSConnectionStateChangeListener_connectionStateChanged_Proxy( 
    IJMSConnectionStateChangeListener * This,
    /* [in] */ int state);


void __RPC_STUB IJMSConnectionStateChangeListener_connectionStateChanged_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IJMSConnectionStateChangeListener_INTERFACE_DEFINED__ */


EXTERN_C const CLSID CLSID_CJMSObject;

#ifdef __cplusplus

class DECLSPEC_UUID("BDA6567C-63EF-4066-B69E-243C07A8E970")
CJMSObject;
#endif

EXTERN_C const CLSID CLSID_CJMSQueueConnectionFactory;

#ifdef __cplusplus

class DECLSPEC_UUID("D658E3EF-4176-4822-98AF-B894BC75333F")
CJMSQueueConnectionFactory;
#endif

EXTERN_C const CLSID CLSID_CJMSTopicConnectionFactory;

#ifdef __cplusplus

class DECLSPEC_UUID("47C22096-324A-49E7-BDE5-0587AF07564F")
CJMSTopicConnectionFactory;
#endif

EXTERN_C const CLSID CLSID_CJMSDestination;

#ifdef __cplusplus

class DECLSPEC_UUID("FFF6A23E-0E7E-4BA9-AF6D-40E35DAB9CD9")
CJMSDestination;
#endif

EXTERN_C const CLSID CLSID_CJMSQueue;

#ifdef __cplusplus

class DECLSPEC_UUID("9A1712B6-5E21-48C3-B29E-57C7C441BA6E")
CJMSQueue;
#endif

EXTERN_C const CLSID CLSID_CJMSTopic;

#ifdef __cplusplus

class DECLSPEC_UUID("11739894-0250-4532-8D73-1F4E39E27CA3")
CJMSTopic;
#endif

EXTERN_C const CLSID CLSID_CJMSTemporaryQueue;

#ifdef __cplusplus

class DECLSPEC_UUID("01875514-D24E-4DB0-A1EB-380A0A2146E8")
CJMSTemporaryQueue;
#endif

EXTERN_C const CLSID CLSID_CJMSTemporaryTopic;

#ifdef __cplusplus

class DECLSPEC_UUID("1661EA20-0EE9-4958-AC8A-2337B5395751")
CJMSTemporaryTopic;
#endif

EXTERN_C const CLSID CLSID_CJMSMessage;

#ifdef __cplusplus

class DECLSPEC_UUID("86639EF7-BFC8-4310-80DF-57A9DAEB549D")
CJMSMessage;
#endif

EXTERN_C const CLSID CLSID_CJMSBytesMessage;

#ifdef __cplusplus

class DECLSPEC_UUID("91D4875F-C152-4D5C-BF29-E93C6F122853")
CJMSBytesMessage;
#endif

EXTERN_C const CLSID CLSID_CJMSObjectMessage;

#ifdef __cplusplus

class DECLSPEC_UUID("574F03C3-F8C4-4FCC-83FE-2905080A3E32")
CJMSObjectMessage;
#endif

EXTERN_C const CLSID CLSID_CJMSStreamMessage;

#ifdef __cplusplus

class DECLSPEC_UUID("F2713407-06EA-43DB-99E7-3758003BC7FF")
CJMSStreamMessage;
#endif

EXTERN_C const CLSID CLSID_CJMSTextMessage;

#ifdef __cplusplus

class DECLSPEC_UUID("7F874938-0D96-4EFB-BDC0-74E2DBD235CF")
CJMSTextMessage;
#endif

EXTERN_C const CLSID CLSID_CJMSConnectionStateChangeListenerHelper;

#ifdef __cplusplus

class DECLSPEC_UUID("844258E6-8B7A-4bf4-A76D-C0665E604AC3")
CJMSConnectionStateChangeListenerHelper;
#endif

EXTERN_C const CLSID CLSID_CJMSConnection;

#ifdef __cplusplus

class DECLSPEC_UUID("65638450-4282-41B2-AAE6-7D591A646AA2")
CJMSConnection;
#endif

EXTERN_C const CLSID CLSID_CJMSQueueConnection;

#ifdef __cplusplus

class DECLSPEC_UUID("87FD42DD-CB00-4846-8F16-2361FC8FB9E0")
CJMSQueueConnection;
#endif

EXTERN_C const CLSID CLSID_CJMSTopicConnection;

#ifdef __cplusplus

class DECLSPEC_UUID("AD8BD693-EE0B-4515-8B9E-4F202808F99A")
CJMSTopicConnection;
#endif

EXTERN_C const CLSID CLSID_CJMSMessageConsumer;

#ifdef __cplusplus

class DECLSPEC_UUID("36B378EF-1084-4BC5-A790-C050981A619A")
CJMSMessageConsumer;
#endif

EXTERN_C const CLSID CLSID_CJMSQueueReceiver;

#ifdef __cplusplus

class DECLSPEC_UUID("AFBA0606-36D2-4E8A-8CDF-54825972B7C3")
CJMSQueueReceiver;
#endif

EXTERN_C const CLSID CLSID_CJMSTopicSubscriber;

#ifdef __cplusplus

class DECLSPEC_UUID("72750E1D-6D6F-40D9-994B-05587EBED0A2")
CJMSTopicSubscriber;
#endif

EXTERN_C const CLSID CLSID_CJMSMessageProducer;

#ifdef __cplusplus

class DECLSPEC_UUID("B72236F1-2AFC-4B23-95B5-52A8B7D8597D")
CJMSMessageProducer;
#endif

EXTERN_C const CLSID CLSID_CJMSQueueSender;

#ifdef __cplusplus

class DECLSPEC_UUID("2F73767D-A368-4046-9FCF-CCB6D2519E4E")
CJMSQueueSender;
#endif

EXTERN_C const CLSID CLSID_CJMSTopicPublisher;

#ifdef __cplusplus

class DECLSPEC_UUID("0D94C625-3A4A-4A2E-996D-A179F7C9F79B")
CJMSTopicPublisher;
#endif

EXTERN_C const CLSID CLSID_CJMSSession;

#ifdef __cplusplus

class DECLSPEC_UUID("731F2B68-3C50-4157-B2F4-CAAA33929A4D")
CJMSSession;
#endif

EXTERN_C const CLSID CLSID_CJMSQueueSession;

#ifdef __cplusplus

class DECLSPEC_UUID("610760D7-75A2-421C-BEE8-32220FB982C6")
CJMSQueueSession;
#endif

EXTERN_C const CLSID CLSID_CJMSTopicSession;

#ifdef __cplusplus

class DECLSPEC_UUID("75E1D581-DEF6-4897-A2CB-318D54B2816A")
CJMSTopicSession;
#endif

EXTERN_C const CLSID CLSID_CJMSQueueBrowser;

#ifdef __cplusplus

class DECLSPEC_UUID("F59B5AC6-99E5-4D9F-A47C-AA86F431764D")
CJMSQueueBrowser;
#endif

EXTERN_C const CLSID CLSID_CJMSEnumeration;

#ifdef __cplusplus

class DECLSPEC_UUID("E4671093-C7E5-4E57-9D6C-9C2D502B0F27")
CJMSEnumeration;
#endif

EXTERN_C const CLSID CLSID_CJMSConnectionMetaData;

#ifdef __cplusplus

class DECLSPEC_UUID("DE23B9F1-7263-40BD-8F8A-7E7F5B2C1B36")
CJMSConnectionMetaData;
#endif

EXTERN_C const CLSID CLSID_CJMSCOMException;

#ifdef __cplusplus

class DECLSPEC_UUID("EB8FDD63-5C55-4D11-A150-D324C5260D10")
CJMSCOMException;
#endif

EXTERN_C const CLSID CLSID_CJMSDurableSubscriber;

#ifdef __cplusplus

class DECLSPEC_UUID("E8FE5CD8-38CB-4DEB-BA64-FAFD95329258")
CJMSDurableSubscriber;
#endif

EXTERN_C const CLSID CLSID_CJMSMapMessage;

#ifdef __cplusplus

class DECLSPEC_UUID("8D5C029B-7F27-4F1E-AF0D-CCD80195364C")
CJMSMapMessage;
#endif

EXTERN_C const CLSID CLSID_CJMSThrowable;

#ifdef __cplusplus

class DECLSPEC_UUID("FFA8A5CB-DC60-4400-ACB9-0CC054EA9C9C")
CJMSThrowable;
#endif
#endif /* __JMSCOMCLIENTLib_LIBRARY_DEFINED__ */

/* Additional Prototypes for ALL interfaces */

unsigned long             __RPC_USER  BSTR_UserSize(     unsigned long *, unsigned long            , BSTR * ); 
unsigned char * __RPC_USER  BSTR_UserMarshal(  unsigned long *, unsigned char *, BSTR * ); 
unsigned char * __RPC_USER  BSTR_UserUnmarshal(unsigned long *, unsigned char *, BSTR * ); 
void                      __RPC_USER  BSTR_UserFree(     unsigned long *, BSTR * ); 

unsigned long             __RPC_USER  VARIANT_UserSize(     unsigned long *, unsigned long            , VARIANT * ); 
unsigned char * __RPC_USER  VARIANT_UserMarshal(  unsigned long *, unsigned char *, VARIANT * ); 
unsigned char * __RPC_USER  VARIANT_UserUnmarshal(unsigned long *, unsigned char *, VARIANT * ); 
void                      __RPC_USER  VARIANT_UserFree(     unsigned long *, VARIANT * ); 

/* end of Additional Prototypes */

#ifdef __cplusplus
}
#endif

#endif


