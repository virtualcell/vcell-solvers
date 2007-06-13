#ifndef _SIMULATIONMESSAGING_H_
#define _SIMULATIONMESSAGING_H_

#ifdef WIN32
    #include <windows.h>	
#else
    #include <pthread.h>
    #include <unistd.h>
#endif

#include <time.h>
#include <progress/message/jclient/package.h>
using namespace progress::message::jclient;

static const int ONE_SECOND = 1000;
static const int ONE_MINUTE = 60 * ONE_SECOND;
static const int DEFAULT_TTL = 10 * ONE_MINUTE;
static const int WORKEREVENT_OUTPUT_MODE_STDOUT = 0;
static const int WORKEREVENT_OUTPUT_MODE_MESSAGING = 1;

static const jint JOB_STARTING = 999;
static const jint JOB_DATA = 1000;
static const jint JOB_PROGRESS = 1001;
static const jint JOB_FAILURE = 1002;
static const jint JOB_COMPLETED = 1003;
static const jint JOB_WORKER_ALIVE = 1004;	

struct WorkerEvent {
	jint status;
	double progress;
	double timepoint;
	char* eventMessage;	
	WorkerEvent(const WorkerEvent* aWorkerEvent) {
		status = aWorkerEvent->status;
		progress = aWorkerEvent->progress;
		timepoint = aWorkerEvent->timepoint;
		eventMessage = NULL;
		if (aWorkerEvent->eventMessage != NULL) {
			int len = strlen(aWorkerEvent->eventMessage) + 1;
			eventMessage = new char[len];
			memset(eventMessage, 0, len);
			strcpy(eventMessage, aWorkerEvent->eventMessage);
		}
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

	WorkerEvent(jint arg_status, jdouble arg_progress, jdouble arg_timepoint) {
		status = arg_status;
		progress = arg_progress;
		timepoint = arg_timepoint;
		eventMessage = NULL;
	}
	WorkerEvent(jint arg_status, char* arg_eventMessage) {
		status = arg_status;
		eventMessage = arg_eventMessage;
		progress = 0.0;
		timepoint = 0.0;
	}
};

class SimulationMessaging : public progress::message::jclient::ExceptionListener, progress::message::jclient::MessageListener {
public:
	static SimulationMessaging *m_inst;

    ~SimulationMessaging();
	static SimulationMessaging* create();
	static SimulationMessaging* create(char* broker, char* smqusername, char* passwd, char* qname, char* tname, char* vcusername, jlong simKey, jint jobIndex, jint taskID, jint ttl=DEFAULT_TTL);
    void onException(JMSExceptionRef anException);
	void onMessage(MessageRef aMessage);
	static SimulationMessaging* getInstVar();	
	void waitUntilFinished();

	void setWorkerEvent(WorkerEvent* workerEvent);
	void start();
	friend void startMessagingThread(void* param);

private:
	SimulationMessaging();
	SimulationMessaging(char* broker, char* smqusername, char* passwd, char* qname, char*tname, char* vcusername, jlong simKey, jint jobIndex,  jint taskID, jint ttl=DEFAULT_TTL);

	WorkerEvent* getWorkerEvent();
	WorkerEvent* sendStatus();
	void keepAlive();
	static char* trim(char* str);
	void setupConnection ();    //synchronized
	QueueSessionRef getQueueSession();
	QueueSenderRef getQueueSender();	

	TextMessageRef initWorkerEventMessage();
	HANDLE hNewWorkerEvent;
	HANDLE hMessagingThreadEndEvent;
	WorkerEvent* workerEvent;
	bool  m_connActive;

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
	int workerEventOutputMode;
	time_t lastSentEventTime;

	char* getStatusString(jint status);

	//Platform specific methods
	void createCriticalSection();
	void destroyCriticalSection();
	bool enterCriticalSection(bool iftry=false);
	void leaveCriticalSection();
	void delay(jint duration);

	bool lockWorkerEvent(bool iftry=false);
	void unlockWorkerEvent();

#ifdef WIN32
    CRITICAL_SECTION lock;
	CRITICAL_SECTION lockForWorkerEvent;
#endif
#ifdef SOLARIS
    static pthread_mutexattr_t mattr;
#endif
#ifdef HPUX
	static pthread_mutexattr_t mattr;
#endif
#ifdef AIX
        static pthread_mutexattr_t mattr;
#endif
#ifdef UNIX
    static pthread_mutex_t mp;
#endif	
};

#endif // _SIMULATIONMESSAGING_H_
