#ifndef _SIMULATIONMESSAGING_H_
#define _SIMULATIONMESSAGING_H_

#if ( defined(WIN32) || defined(WIN64) )
#include <windows.h>
#else
#include <pthread.h>
#include <unistd.h>
#include <stdlib.h>
#include <memory.h>
#endif

#ifdef USE_MESSAGING
#if ( !defined(WIN32) && !defined(WIN64) )
#include <sys/time.h>
#else
#include <time.h>
#endif
#include <progress/message/jclient/package.h>
using namespace progress::message::jclient;

static const int ONE_SECOND = 1000;
static const int ONE_MINUTE = 60 * ONE_SECOND;
static const int DEFAULT_TTL = 10 * ONE_MINUTE;
#endif

static const int WORKEREVENT_OUTPUT_MODE_STDOUT = 0;
static const int WORKEREVENT_OUTPUT_MODE_MESSAGING = 1;

static const int JOB_STARTING = 999;
static const int JOB_DATA = 1000;
static const int JOB_PROGRESS = 1001;
static const int JOB_FAILURE = 1002;
static const int JOB_COMPLETED = 1003;
static const int JOB_WORKER_ALIVE = 1004;

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
class SimulationMessaging : public progress::message::jclient::ExceptionListener, progress::message::jclient::MessageListener
#else
class SimulationMessaging
#endif
{
public:
    ~SimulationMessaging();
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
	static SimulationMessaging* create(char* broker, char* smqusername, char* passwd, char* qname, char* tname, char* vcusername, jint simKey, jint jobIndex, jint taskID, jint ttl=DEFAULT_TTL);
    void onException(JMSExceptionRef anException);
	void onMessage(MessageRef aMessage);
	void waitUntilFinished();
	friend void* startMessagingThread(void* param);
#endif

private:
	SimulationMessaging();
	static SimulationMessaging *m_inst;
	WorkerEvent* workerEvent;
	int workerEventOutputMode;

	WorkerEvent* sendStatus();
	bool bStopRequested;

#ifdef USE_MESSAGING
	bool bStarted;

	SimulationMessaging(char* broker, char* smqusername, char* passwd, char* qname, char*tname, char* vcusername, jint simKey, jint jobIndex,  jint taskID, jint ttl=DEFAULT_TTL);

	WorkerEvent* getWorkerEvent();
	void keepAlive();
	static char* trim(char* str);
	void setupConnection ();    //synchronized
	QueueSessionRef getQueueSession();
	QueueSenderRef getQueueSender();

	TextMessageRef initWorkerEventMessage();
	bool m_connActive;

	QueueRef m_queue;
	QueueConnectionRef m_qConnect;
	QueueSessionRef m_qSession;
	QueueSenderRef m_qSender;

	TopicRef m_topic;
	TopicConnectionRef m_tConnect;
	TopicSessionRef m_tSession;
	TopicSubscriberRef m_tSubscriber;

	char *m_broker;
	char *m_smqusername;
	char *m_password;
	char *m_qname;
	char* m_tname;
	char* m_vcusername;
	jint m_simKey;
	jint m_taskID;
	jint m_jobIndex;

	char* m_tListener;

	char m_hostname[256];
	jint  m_ttl;
	time_t lastSentEventTime;

	char* getStatusString(jint status);

	bool lockMessaging();
	void unlockMessaging();
	void delay(jint duration);

	bool lockWorkerEvent(bool bTry=false);
	void unlockWorkerEvent();

	bool bStarted;

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
