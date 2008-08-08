#ifndef _INCLUDE_CLASSTYPES_H_
#define _INCLUDE_CLASSTYPES_H_
/*
 * Copyright (c) 2001 - 2008 Progress Software Corporation. All Rights Reserved.
 * This software is the confidential and proprietary information of Progress
 * Software Corporation ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Progress Software Corporation.
 */


enum {
	/* Value 0 reserved for indicating success in C API. */
	package_java_lang = 100,
	package_java_util = 200,
	package_progress_message_jclient = 300,
	package_global = 400
};

/* Class enumerators used in type(), getType(), and instanceof(). */
enum {
	class_java_lang_ArrayIndexOutOfBoundsException = package_java_lang,
	class_java_lang_AString,
	class_java_lang_Boolean,
	class_java_lang_Byte,
	class_java_lang_ClassCastException,
	class_java_lang_CloneNotSupportedException,
	class_java_lang_Double,
	class_java_lang_Exception,
	class_java_lang_Float,
	class_java_lang_IllegalArgumentException,
	class_java_lang_IndexOutOfBoundsException,
	class_java_lang_Integer,
	class_java_lang_Long,
	class_java_lang_Number,
	class_java_lang_NumberFormatException,
	class_java_lang_Object,
	class_java_lang_NullPointerException,
	class_java_lang_RuntimeException,
	class_java_lang_Short,
	class_java_lang_String,
	class_java_lang_StringIndexOutOfBoundsException,
	class_java_lang_Throwable,
	class_java_lang_UTF8String,
	class_java_lang_WString,
	class_java_lang_end,

	class_java_util_Dictionary = package_java_util,
	class_java_util_Enumeration,
	class_java_util_Hashtable,
    class_java_util_JlongHashtable,
	class_java_util_NoSuchElementException,
	class_java_util_end,

	class_progress_message_jclient_BrokerName = package_progress_message_jclient,
	class_progress_message_jclient_BytesMessage,
	class_progress_message_jclient_Connection,
	class_progress_message_jclient_ConnectionMetaData,
	class_progress_message_jclient_Destination,
	class_progress_message_jclient_DurableSubscriber,
	class_progress_message_jclient_IllegalStateException,
	class_progress_message_jclient_InvalidClientIDException,
	class_progress_message_jclient_InvalidDestinationException,
	class_progress_message_jclient_InvalidSelectorException,
	class_progress_message_jclient_ILoginSPI,
	class_progress_message_jclient_ITopicSubscriber,
	class_progress_message_jclient_JMSException,
	class_progress_message_jclient_JMSSecurityException,
	class_progress_message_jclient_Message,
	class_progress_message_jclient_MessageConsumer,
	class_progress_message_jclient_MessageEOFException,
	class_progress_message_jclient_MessageFormatException,
	class_progress_message_jclient_MessageNotReadableException,
	class_progress_message_jclient_MessageNotWriteableException,
	class_progress_message_jclient_MessageProducer,
	class_progress_message_jclient_Queue,
	class_progress_message_jclient_QueueBrowser,
	class_progress_message_jclient_QueueConnection,
	class_progress_message_jclient_QueueConnectionFactory,
	class_progress_message_jclient_QueueReceiver,
	class_progress_message_jclient_QueueSender,
	class_progress_message_jclient_QueueSession,
	class_progress_message_jclient_ResourceAllocationException,
	class_progress_message_jclient_Session,
	class_progress_message_jclient_TemporaryQueue,
	class_progress_message_jclient_TemporaryTopic,
	class_progress_message_jclient_TextMessage,
	class_progress_message_jclient_Topic,
	class_progress_message_jclient_TopicConnection,
	class_progress_message_jclient_TopicConnectionFactory,
	class_progress_message_jclient_TopicPublisher,
	class_progress_message_jclient_TopicSession,
	class_progress_message_jclient_TopicSubscriber,
	class_progress_message_jclient_TransactionInProgressException,
	class_progress_message_jclient_TransactionRolledBackException,
	class_progress_message_jclient_MessageListener,
	class_progress_message_jclient_ExceptionListener,
	class_progress_message_jclient_ConnectionStateChangeListener,
    class_progress_message_jclient_StreamMessage,
    class_progress_message_jclient_ConnectionFactory,
	class_progress_message_jclient_end,

	class_jbyteArray = package_global,
	class_jcharArray,
	class_global_end
};

#endif /* _INCLUDE_CLASSTYPES_H_ */
