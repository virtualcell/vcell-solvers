#ifndef _SIMULATIONMESSAGING_H_
#define _SIMULATIONMESSAGING_H_

#ifdef USE_MESSAGING
#include <activemq/library/ActiveMQCPP.h>
#include <decaf/lang/Thread.h>
#include <decaf/lang/Runnable.h>
#include <decaf/util/concurrent/CountDownLatch.h>
#include <decaf/lang/Integer.h>
#include <decaf/lang/Long.h>
#include <decaf/lang/System.h>
#include <activemq/core/ActiveMQConnectionFactory.h>
#include <activemq/util/Config.h>
#include <cms/Connection.h>
#include <cms/Session.h>
#include <cms/TextMessage.h>
#include <cms/BytesMessage.h>
#include <cms/MapMessage.h>
#include <cms/ExceptionListener.h>
#include <cms/MessageListener.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include <memory>

using namespace activemq::core;
using namespace decaf::util::concurrent;
using namespace decaf::util;
using namespace decaf::lang;
using namespace cms;
#endif

using namespace std;

#if (defined(WIN32) || defined(WIN64) )
#include <windows.h>
#else
#include <pthread.h>
#include <unistd.h>
#include <stdlib.h>
#include <memory.h>
#endif

#ifdef USE_MESSAGING
#if (!defined(WIN32) && !defined(WIN64) )
#include <sys/time.h>
#else
#include <time.h>
#endif

static const int ONE_SECOND = 1000;
static const int ONE_MINUTE = 60 * ONE_SECOND;
static const int DEFAULT_TTL_HIGH = 10 * ONE_MINUTE;
static const int DEFAULT_TTL_LOW = ONE_MINUTE;
#endif

static const int WORKEREVENT_OUTPUT_MODE_STDOUT = 0;
static const int WORKEREVENT_OUTPUT_MODE_MESSAGING = 1;

static const int JOB_STARTING = 999;
static const int JOB_DATA = 1000;
static const int JOB_PROGRESS = 1001;
static const int JOB_FAILURE = 1002;
static const int JOB_COMPLETED = 1003;
static const int JOB_WORKER_ALIVE = 1004;

static const int DEFAULT_PRIORITY = 0; //range 0-127, the bigger number the higher priority
static const int Message_DEFAULT_PRIORITY = 0;

struct WorkerEvent {
	int status;
	double progress;
	double timepoint;
	char* eventMessage;

	WorkerEvent(const WorkerEvent* aWorkerEvent) {
		status = aWorkerEvent->status;
		progress = aWorkerEvent->progress;
		timepoint = aWorkerEvent->timepoint;
		eventMessage = NULL;
		if (aWorkerEvent->eventMessage != NULL) {
			int len = (int)strlen(aWorkerEvent->eventMessage) + 1;
			eventMessage = new char[len];
			memset(eventMessage, 0, len);
			strcpy(eventMessage, aWorkerEvent->eventMessage);
		}
	}

	WorkerEvent(int arg_status, double arg_progress, double arg_timepoint) {
		status = arg_status;
		progress = arg_progress;
		timepoint = arg_timepoint;
		eventMessage = NULL;
	}

	WorkerEvent(int arg_status, const char* arg_eventMessage) {
		status = arg_status;
		int len = (int)strlen(arg_eventMessage) + 1;
		eventMessage = new char[len];
		memset(eventMessage, 0, len);
		strcpy(eventMessage, arg_eventMessage);
		progress = 0.0;
		timepoint = 0.0;
	}

	bool equals(WorkerEvent* aWorkerEvent) {
		if (status != aWorkerEvent->status || progress != aWorkerEvent->progress || timepoint != aWorkerEvent->timepoint) {
			return false;
		}
		if (eventMessage != NULL && aWorkerEvent->eventMessage != NULL && strcmp(eventMessage, aWorkerEvent->eventMessage) != 0) {
			return false;
		}

		if (eventMessage != NULL && aWorkerEvent->eventMessage == NULL
			|| eventMessage == NULL && aWorkerEvent->eventMessage != NULL) {
			return false;
		}

		return true;
	}
};

#ifdef USE_MESSAGING
class SimulationMessaging : public ExceptionListener, MessageListener/*, Runnable*/
#else
class SimulationMessaging
#endif
{
public:
    virtual ~SimulationMessaging() throw();
	static SimulationMessaging* create();
	static SimulationMessaging* getInstVar();
	void start();
	void setWorkerEvent(WorkerEvent* workerEvent);

	bool isStopRequested() {
		return bStopRequested;
	}

	int getTaskID() {
#ifdef USE_MESSAGING
		return m_taskID;
#else
		return -1;
#endif
	}

#ifdef USE_MESSAGING
	static SimulationMessaging* create(const char* broker, const char* smqusername, const char* passwd, const char* qname, const char* tname,
			const char* vcusername, int simKey, int jobIndex, int taskID, int ttl_low=DEFAULT_TTL_LOW, int ttl_high=DEFAULT_TTL_HIGH);
    void onException(const CMSException& anException);
	void onMessage(const Message* aMessage) throw();
	void waitUntilFinished();
	friend void* startMessagingThread(void* param);
#endif
	void setLogStream(std::ostream &dest) {
		pStream = &dest;
	}

private:
	SimulationMessaging();
	static SimulationMessaging *m_inst;
	/**
	 * where logging goes
	 */
	std::ostream *pStream;
	WorkerEvent* workerEvent;
	int workerEventOutputMode;

	WorkerEvent* sendStatus();
	bool bStopRequested;

#ifdef USE_MESSAGING
	bool bStarted;

	SimulationMessaging(const char* broker, const char* smqusername, const char* passwd, const char* qname, const char*tname, const char* vcusername, int simKey, int jobIndex,  int taskID, int ttl_low=DEFAULT_TTL_LOW, int ttl_high=DEFAULT_TTL_HIGH);
	std::ostream & logStream ( ) {
		return *pStream;
	}

	WorkerEvent* getWorkerEvent();
	void keepAlive();
	static char* trim(char* str);
	void setupConnection ();    //synchronized
	Session& getQueueSession();
	//Queue& getQueueSender();
	TextMessage* initWorkerEventMessage();
	void cleanup();
	
	bool m_connActive;
	Connection* connection;
	Session* session;
	//always send message from queue
	Destination* m_queue;
	MessageProducer* qProducer;
//	QueueConnectionRef m_qConnect;
//	QueueSessionRef m_qSession;
//	QueueSenderRef m_qSender;

	//always receive message from topic
	Destination* m_topic;
	MessageConsumer* tConsumer;
//	TopicConnectionRef m_tConnect;
//	TopicSessionRef m_tSession;
//	TopicSubscriberRef m_tSubscriber;

	char *m_broker;
	char *m_smqusername;
	char *m_password;
	char *m_qname;
	char* m_tname;
	char* m_vcusername;
	int m_simKey;
	int m_taskID;
	int m_jobIndex;

	char* m_tListener;

	char m_hostname[256];
	int  m_ttl_lowPriority;
	int  m_ttl_highPriority;
	time_t lastSentEventTime;

	char* getStatusString(int status);

	bool lockMessaging();
	void unlockMessaging();
	void delay(int duration);

	bool lockWorkerEvent(bool bTry=false);
	void unlockWorkerEvent();

#ifdef WIN32
    CRITICAL_SECTION lockForMessaging;
	CRITICAL_SECTION lockForWorkerEvent;
	HANDLE hNewWorkerEvent;
	HANDLE hMessagingThreadEndEvent;
#else // UNIX
	pthread_t newWorkerEventThread;
	pthread_mutex_t mutex_messaging;
    pthread_mutex_t mutex_workerEvent;
    pthread_mutex_t mutex_cond_workerEvent;
	pthread_cond_t cond_workerEvent;
	bool bNewWorkerEvent;
#endif
#endif
};

#endif // _SIMULATIONMESSAGING_H_
