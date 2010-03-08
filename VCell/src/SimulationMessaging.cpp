#include <VCELL/SimulationMessaging.h>
#include <utility>
#include <iostream>
using std::cerr;
using std::cout;
using std::endl;

#ifdef USE_MESSAGING
#if ( !defined(WIN32) && !defined(WIN64) ) // UNIX
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#endif

#include <jmsconstants.h>

static const char* MESSAGE_TYPE_PROPERTY	= "MessageType";
static const char* MESSAGE_TYPE_WORKEREVENT_VALUE	= "WorkerEvent";
static const char* MESSAGE_TYPE_STOPSIMULATION_VALUE = "StopSimulation";

static const char* USERNAME_PROPERTY = "UserName";
static const char* HOSTNAME_PROPERTY = "HostName";
static const char* SIMKEY_PROPERTY = "SimKey";
static const char* TASKID_PROPERTY	= "TaskID";
static const char* JOBINDEX_PROPERTY	= "JobIndex";
static const char* WORKEREVENT_STATUS = "WorkerEvent_Status";
static const char* WORKEREVENT_PROGRESS = "WorkerEvent_Progress";
static const char* WORKEREVENT_TIMEPOINT = "WorkerEvent_TimePoint";
static const char* WORKEREVENT_STATUSMSG = "WorkerEvent_StatusMsg";

static const int CONNECTION_RETRY_PERIOD = 10 * ONE_SECOND;
static const int CONNECTION_PING_PERIOD = 30 * ONE_SECOND;
#endif

SimulationMessaging *SimulationMessaging::m_inst = NULL;

SimulationMessaging::SimulationMessaging() {
	bStopRequested = false;
	workerEvent = NULL;
#ifdef USE_MESSAGING
	m_taskID = -1;
#endif
	workerEventOutputMode = WORKEREVENT_OUTPUT_MODE_STDOUT;
}

#ifdef USE_MESSAGING
SimulationMessaging::SimulationMessaging(char* broker, char* smqusername, char* passwd, char*qname,  char* tname, char* vcusername, jint simKey, jint jobIndex, jint taskID, jint ttl) {
	m_qConnect = NULL;
    m_qSession = NULL;
    m_qSender = NULL;
	m_tConnect = NULL;
    m_tSession = NULL;
    m_tSubscriber = NULL;
	workerEvent = NULL;
	bStopRequested = false;

	m_broker = broker;
	m_smqusername = smqusername;
	m_password = passwd;	
	m_qname = qname;
	m_tname = tname;

	m_vcusername = vcusername;
	m_simKey = simKey;
	m_jobIndex = jobIndex;
	m_taskID = taskID;
	
	m_ttl = ttl;
	m_connActive = false;

	{
		memset(m_hostname, 0, 256);

		// get hostname
#if ( defined(WIN32) || defined(WIN64) )
		WSADATA wsaData;
		WORD wVersionRequested = MAKEWORD( 2, 2 );		 
		WSAStartup( wVersionRequested, &wsaData );			
#endif
		gethostname(m_hostname, 256);
		//cout << "hostname is " << m_hostname << endl;		
	}

	time(&lastSentEventTime);

	workerEventOutputMode = WORKEREVENT_OUTPUT_MODE_MESSAGING;

#if ( defined(WIN32) || defined(WIN64) )
    InitializeCriticalSection(&lockForMessaging);
	InitializeCriticalSection(&lockForWorkerEvent);
#else // UNIX
	pthread_mutex_init(&mutex_messaging, NULL);
	pthread_mutex_init(&mutex_workerEvent, NULL);
	pthread_mutex_init(&mutex_cond_workerEvent, NULL);
	pthread_cond_init(&cond_workerEvent, NULL);
	bNewWorkerEvent = false;
#endif

}
#endif

SimulationMessaging::~SimulationMessaging()
{
	if (workerEventOutputMode == WORKEREVENT_OUTPUT_MODE_STDOUT) {
		return;
	}
	if (workerEvent != NULL) {
		delete workerEvent;
		workerEvent = NULL;
	}
#ifdef USE_MESSAGING
	try {
		if (m_qConnect != null) {
			m_qConnect->close();
		}
		if (m_tConnect != null) {
			m_tConnect->close();
		}
	} catch (JMSExceptionRef jmse) {
	} catch (...) {		
	}
#if ( defined(WIN32) || defined(WIN64) )
    DeleteCriticalSection(&lockForMessaging);
	DeleteCriticalSection(&lockForWorkerEvent);
#else // UNIX
    pthread_mutex_destroy(&mutex_messaging);
	pthread_mutex_destroy(&mutex_workerEvent);
	pthread_mutex_destroy(&mutex_cond_workerEvent);
	pthread_cond_destroy(&cond_workerEvent);
#endif

#endif
}

SimulationMessaging* SimulationMessaging::getInstVar() {
	return m_inst;
}

SimulationMessaging* SimulationMessaging::create()
{
	if (m_inst == 0){    
        m_inst = new SimulationMessaging();
	}

    return(m_inst);
}

WorkerEvent* SimulationMessaging::sendStatus() {
	if (workerEvent == NULL) {
		return NULL;
	}

	if (workerEventOutputMode == WORKEREVENT_OUTPUT_MODE_STDOUT) {
		switch (workerEvent->status) {
		case JOB_DATA:
			printf("[[[data:%lg]]]", workerEvent->timepoint);
			fflush(stdout);
			break;
		case JOB_PROGRESS:
			printf("[[[progress:%lg%%]]]", workerEvent->progress * 100.0);
			fflush(stdout);
			break;
		case JOB_STARTING:
			cout << workerEvent->eventMessage << endl;
			break;
		case JOB_COMPLETED:
			cerr << "Simulation Complete in Main() ... " << endl;
			break;
		case JOB_FAILURE:
			cerr << workerEvent->eventMessage << endl;			
			break;
		}

		return workerEvent;
	} else {
#ifdef USE_MESSAGING
		lockWorkerEvent();
		WorkerEvent* newWorkerEvent = new WorkerEvent(workerEvent);
		unlockWorkerEvent();

		TextMessageRef msg = initWorkerEventMessage();
	
		char* revisedMsg = newWorkerEvent->eventMessage;
		if (revisedMsg != NULL) {
			revisedMsg = trim(revisedMsg);
			if (strlen(revisedMsg) > 2048) {
				revisedMsg[2047] = 0; //status message is only 2048 chars long in database
			}

			for (int i = 0; i < (int)strlen(revisedMsg); i ++) {
				switch (revisedMsg[i]) { 
				case '\r': 
				case '\n':
				case '\'':
					revisedMsg[i] = ' ';
					break;
					// these characters are not valid both in database and in messages as a property
				}
			}	
		}

		//status
		msg->setIntProperty(createString(WORKEREVENT_STATUS), newWorkerEvent->status);
		//event message
		if (revisedMsg != NULL) {
			msg->setStringProperty(createString(WORKEREVENT_STATUSMSG), createString(revisedMsg));
		}
		//progress
		msg->setDoubleProperty(createString(WORKEREVENT_PROGRESS), newWorkerEvent->progress);
		//timePoint
		msg->setDoubleProperty(createString(WORKEREVENT_TIMEPOINT), newWorkerEvent->timepoint);		

		cout << "!!!SimulationMessaging::sendStatus [" << (long)m_simKey << ":" << getStatusString(newWorkerEvent->status);
		if (revisedMsg != NULL) {
			cout << ":" << revisedMsg;
		} else {
			cout << ":" << newWorkerEvent->progress << ":" << newWorkerEvent->timepoint;
		}

		cout << "]" << endl;
		//send
		try {
			getQueueSender()->send(msg, DeliveryMode_PERSISTENT, Message_DEFAULT_PRIORITY, m_ttl); 	
		} catch (JMSExceptionRef jmse) {
			cout << "!!!SimulationMessaging::sendStatus [" << (const char*)jmse->getMessage()->toAscii() << "]" << endl;
		}

		time(&lastSentEventTime);
		if (newWorkerEvent->status == JOB_COMPLETED || newWorkerEvent->status == JOB_FAILURE) {
#if ( defined(WIN32) || defined(WIN64) )
			SetEvent(hMessagingThreadEndEvent);
#else // UNIX
			cout <<  "!!!thread exiting" << endl;
			pthread_exit(NULL);
#endif
		}

		return newWorkerEvent;
#else
		throw "OUPUT_MODE_STANDOUT must be using if not using messaging!";
#endif
	}
}

void SimulationMessaging::setWorkerEvent(WorkerEvent* arg_workerEvent) {
	if (m_inst == 0) {
		throw "SimulationMessaging is not initialized";
	}
	if (workerEventOutputMode == WORKEREVENT_OUTPUT_MODE_STDOUT) {
		workerEvent = arg_workerEvent;
		sendStatus();
	} else {
#ifdef USE_MESSAGING
		bool ifset = true;
		bool iftry = false;
		lockWorkerEvent();
		jint lastStatus = -1;
		if (workerEvent != NULL) {
			lastStatus = workerEvent->status;
		}
		unlockWorkerEvent();
		if (arg_workerEvent->status == lastStatus && (arg_workerEvent->status == JOB_PROGRESS || arg_workerEvent->status == JOB_DATA)) {
			iftry = true;
			if (((int)arg_workerEvent->progress) % 25 != 0) {
				time_t currTime;
				time(&currTime);
				if (currTime - lastSentEventTime < 5) {
					ifset = false;
				}
			}
		}

		if (ifset) {
			bool succ = false;
			if (iftry) {
				succ = lockWorkerEvent(true);
			} else {
				succ = lockWorkerEvent();
			}
			if (succ) {
				delete workerEvent;
				workerEvent = arg_workerEvent;
				unlockWorkerEvent();
#if ( defined(WIN32) || defined(WIN64) )				
				SetEvent(hNewWorkerEvent);
#else  //UNIX
				pthread_mutex_lock(&mutex_cond_workerEvent);
				bNewWorkerEvent = true;
				pthread_cond_signal(&cond_workerEvent);
				pthread_mutex_unlock(&mutex_cond_workerEvent);
#endif
			}
		}
#else
		throw "OUPUT_MODE_STANDOUT must be using if not using messaging!";
#endif
	}
}



#ifdef USE_MESSAGING
/**
 * Keep trying to start the connection.  Once it is established,
 * setup the message handlers and publishers. (This is synchronized
 * because it can be called from the main thread, and from the
 * onException handler.)
 */
void SimulationMessaging::setupConnection () { //synchronized
	lockMessaging();

    // Eliminate a damaged connection
    if (!m_connActive) {
        if (m_qConnect != NULL){
            m_qConnect = NULL;
        }
    }

    while (m_qConnect == NULL) {
    // Create a connection.
        try
        {
            // Create a connection and sessions.
            m_qConnect = createQueueConnection(createString(m_broker), createString(""),
                createString(m_smqusername), createString(m_password));
            m_tConnect = createTopicConnection(createString(m_broker), createString(""),
                createString(m_smqusername), createString(m_password));

            m_connActive = true;
            // Ping the broker periodically to ensure that the connection is still active.
            m_qConnect->setPingInterval(CONNECTION_PING_PERIOD);
            m_qConnect->setExceptionListenerObj(this);
        } catch (JMSExceptionRef e){
            cerr << "error: Cannot connect to Broker - " << m_broker ;
            cerr << ".  Try again in  - " << CONNECTION_RETRY_PERIOD/1000 << " seconds." << "\n" ;
            delay(CONNECTION_RETRY_PERIOD);
            continue;
        }

        // Create Sender and Receiver to 'talk' queue
        try {
            m_qSession = m_qConnect->createQueueSession(jfalse,Session_AUTO_ACKNOWLEDGE);            
			m_tSession = m_tConnect->createTopicSession(jfalse,Session_AUTO_ACKNOWLEDGE);  
            if (m_qname != NULL){
                m_queue = m_qSession->createQueue(createString(m_qname));
                m_qSender = m_qSession->createSender(m_queue);
                m_qSender->setDeliveryMode(DeliveryMode_PERSISTENT);
				m_qConnect->start();
            }
			if (m_tname != NULL) {
                m_topic = m_tSession->createTopic(createString(m_tname));
                m_tSubscriber = m_tSession->createSubscriber(m_topic);
				m_tSubscriber->setMessageListenerObj(this);
				m_tConnect->start();
			}			
        } catch (JMSExceptionRef e) {
            onException(e);
        }
    }   //end-while

    unlockMessaging();
}

/**
 * Suspend the current thread for "duration" milliseconds.
 */
void SimulationMessaging::delay(jint duration)
{
#if ( defined(WIN32) || defined(WIN64) )
    Sleep(duration);
#else // UNIX
    sleep(duration/1000);
#endif
}

QueueSessionRef SimulationMessaging::getQueueSession() {
	return(m_qSession);
}

QueueSenderRef SimulationMessaging::getQueueSender() {
	return(m_qSender);
}

/**
 * Handle the message
 * (as specified in the javax::jms::MessageListener interface).
 */
void SimulationMessaging::onMessage(MessageRef aMessage)
{
    // Cast the message as a TextMessage if possible.
    try
    {
		StringRef msgType = aMessage->getStringProperty(createString(MESSAGE_TYPE_PROPERTY));	
		if (msgType == NULL) {
			return;
		}
		
		jlong key = aMessage->getLongProperty(createString(SIMKEY_PROPERTY));

		if (strcmp((const char*)msgType->toAscii(), MESSAGE_TYPE_STOPSIMULATION_VALUE) == 0 && key==m_simKey) {	
			cout << "Stopped by user" << endl;
			bStopRequested = true;
		}

    } catch(JMSExceptionRef jmse) {
        onException(jmse);
    }
}

void SimulationMessaging::onException(JMSExceptionRef e)
{
	cout << "!!!JMSException: " << (const char *)e->getMessage()->toAscii() << endl;
	int dropCode = ErrorCodes_ERR_CONNECTION_DROPPED;
	if (ErrorCodes::testException(e, dropCode)) {
		m_connActive = false;
		setupConnection();
		m_connActive = true;
		cout << "Connection restored.  Messages will now be accepted again" << endl;
	}
}

bool SimulationMessaging::lockMessaging()
{
#if ( defined(WIN32) || defined(WIN64) )
    EnterCriticalSection(&lockForMessaging);
	return true;
#else // UNIX
    if (pthread_mutex_lock(&mutex_messaging)) {
        cout << "Cannot acquire mutex, fatal error." << endl;
        exit(1);
    }
#endif
}

void SimulationMessaging::unlockMessaging()
{
#if ( defined(WIN32) || defined(WIN64) )
    LeaveCriticalSection(&lockForMessaging);
#else // UNIX
    pthread_mutex_unlock(&mutex_workerEvent);
#endif
}


bool SimulationMessaging::lockWorkerEvent(bool bTry)
{	
#if ( defined(WIN32) || defined(WIN64) )
	if (bTry) {
		return TryEnterCriticalSection(&lockForWorkerEvent)!=0;
	}
	try {
		EnterCriticalSection(&lockForWorkerEvent);
	} catch (...) {
		return false;
	}
	return true;
#else // UNIX
	if (bTry) {
		return (pthread_mutex_trylock(&mutex_workerEvent) == 0);
	}
    if (pthread_mutex_lock(&mutex_workerEvent) != 0) {
        return false;
    }
	return true;
#endif
}

void SimulationMessaging::unlockWorkerEvent()
{
#if ( defined(WIN32) || defined(WIN64) )
    LeaveCriticalSection(&lockForWorkerEvent);
#else // UNIX
    pthread_mutex_unlock(&mutex_workerEvent);
#endif
}

char* SimulationMessaging::trim(char* str) {	
	int leftIndex, rightIndex;
	int len = (int)strlen(str);
	for (leftIndex = 0; leftIndex < len; leftIndex ++) { // remove leading spaces
		char c = str[leftIndex];
		if (c != ' ' && c != '\n' && c != '\r') {
			break;
		}
	}
	for (rightIndex = len - 1; rightIndex >= 0; rightIndex --) { // remove trailing spaces and new line and carriage return		
		char c = str[rightIndex];
		if (c != ' ' && c != '\n' && c != '\r') {
			break;
		}
	}

	len = rightIndex - leftIndex + 2;
	if (len <= 0) {
		return 0;
	}

	char* newstr = new char[len];
	memset(newstr, 0, len * sizeof(char));
	strncpy(newstr, str + leftIndex, len - 1);

	return newstr;
}

TextMessageRef SimulationMessaging::initWorkerEventMessage() {
	TextMessageRef msg = getQueueSession()->createTextMessage();
	// message type
	msg->setStringProperty(createString(MESSAGE_TYPE_PROPERTY), createString(MESSAGE_TYPE_WORKEREVENT_VALUE));
	//Hostname
	msg->setStringProperty(createString(HOSTNAME_PROPERTY), createString(m_hostname));
	//Username
	msg->setStringProperty(createString(USERNAME_PROPERTY), createString(m_vcusername));
	//simKey
	msg->setLongProperty(createString(SIMKEY_PROPERTY), m_simKey);
	//jobIndex
	msg->setIntProperty(createString(JOBINDEX_PROPERTY), m_jobIndex);
	//taskID
	msg->setIntProperty(createString(TASKID_PROPERTY), m_taskID);
	return msg;
}

void SimulationMessaging::keepAlive() {
	if (workerEventOutputMode == WORKEREVENT_OUTPUT_MODE_MESSAGING) {
		TextMessageRef msg = initWorkerEventMessage();
		//status
		msg->setIntProperty(createString(WORKEREVENT_STATUS), JOB_WORKER_ALIVE);

		//send
		getQueueSender()->send(msg, DeliveryMode_PERSISTENT, Message_DEFAULT_PRIORITY, m_ttl); 
		time(&lastSentEventTime);
	}
}

char* SimulationMessaging::getStatusString(jint status) {
	switch (status) {
	case JOB_STARTING:
		return "JOB_STARTING";
		
	case JOB_PROGRESS:
		return "JOB_PROGRESS";
	
	case JOB_DATA:
		return "JOB_DATA";

	case JOB_COMPLETED:
		return "JOB_COMPLETED";

	case JOB_WORKER_ALIVE:
		return "JOB_WORKER_ALIVE";

	case JOB_FAILURE:
		return "JOB_FAILURE";

	default:
		return "Unknown status";
	}
}

SimulationMessaging* SimulationMessaging::create(char* broker, char* smqusername, char* passwd, char* qname, char* tname, char* vcusername, jint simKey, jint jobIndex, jint taskID, jint ttl)
{
	if (m_inst == NULL){    
        m_inst = new SimulationMessaging(broker, smqusername, passwd, qname, tname, vcusername, simKey, jobIndex, taskID, ttl);
	}

    return(m_inst);
}

WorkerEvent* SimulationMessaging::getWorkerEvent() {
	return workerEvent;
}

void SimulationMessaging::waitUntilFinished() {
	if (workerEventOutputMode == WORKEREVENT_OUTPUT_MODE_STDOUT) {
		return;
	}
#if ( defined(WIN32) || defined(WIN64) )
	DWORD dwWaitResult = WaitForSingleObject(hMessagingThreadEndEvent, INFINITE);
	switch (dwWaitResult) {
	case WAIT_OBJECT_0: 
	case WAIT_TIMEOUT: 	
		break; 

	default:
		cout << "Wait error: " << GetLastError() << endl;
		break;
	}
#else
	cout << "!!!waiting for thread to exit" << endl;
	pthread_join(newWorkerEventThread, NULL);
#endif
}

#ifdef LINUX64
void* startMessagingThread(void* param);
#endif

void SimulationMessaging::start() {
	if (workerEventOutputMode == WORKEREVENT_OUTPUT_MODE_STDOUT) {
		return;
	}
#if ( defined(WIN32) || defined(WIN64) )
	HANDLE hThread; 
	DWORD IDThread; 

	char messagingThreadEndEventName[256];
	char newWorkerEventName[256];
	sprintf(messagingThreadEndEventName, "MessagingThreadEndEvent_%ld_%d_%d", m_simKey, m_jobIndex, m_taskID);
	sprintf(newWorkerEventName, "NewWorkerEvent_%ld_%d_%d", m_simKey, m_jobIndex, m_taskID);

	// Create a manual-reset event object. The master thread sets 
	// this to nonsignaled when it writes to the shared buffer.
	hMessagingThreadEndEvent = CreateEvent( 
		NULL,         // no security attributes
		true,         // manual-reset event
		false,         // initial state is nonsignaled
		messagingThreadEndEventName  // object name
	); 

	if (hMessagingThreadEndEvent == NULL) { 
		throw "CreateEvent failed for new worker event";
	}

	if (GetLastError() == ERROR_ALREADY_EXISTS) {
		throw "Event for messagingThreadEndEvent already exists";
	}

	hNewWorkerEvent = CreateEvent( 
		NULL,         // no security attributes
		true,         // manual-reset event
		false,         // initial state is nonsignaled
		newWorkerEventName  // object name
	); 

	if (hNewWorkerEvent == NULL) { 
		throw "CreateEvent failed for new worker event";
	}

	if (GetLastError() == ERROR_ALREADY_EXISTS) {
		throw "Event for newWorkerEvent already exists";
	}

	// Create multiple threads and an auto-reset event object 
	// for each thread. Each thread sets its event object to 
	// signaled when it is not reading from the shared buffer. 

	hThread = CreateThread(NULL, 0, 
		(LPTHREAD_START_ROUTINE) startMessagingThread, 
		&hNewWorkerEvent,  // pass event handle
		0, &IDThread); 
	if (hThread == NULL) 
	{
		throw "CreateThread failed for messaging thread";
	}
#else // UNIX
	int retv = pthread_create(&newWorkerEventThread, NULL, startMessagingThread, this);
#endif
}


void* startMessagingThread(void* lpParam){	
	SimulationMessaging::getInstVar()->setupConnection();

#if ( defined(WIN32) || defined(WIN64) )
    DWORD dwWaitResult;
    HANDLE hEvent; 
	
    hEvent = *(HANDLE*)lpParam;  // thread's read event

	while (true) {
		dwWaitResult = WaitForSingleObject(hEvent, 5*60*1000);    // 5 min wait

		switch (dwWaitResult) {
		case WAIT_OBJECT_0: 
			{
				WorkerEvent* sentWorkerEvent = SimulationMessaging::getInstVar()->sendStatus();
				SimulationMessaging::getInstVar()->lockWorkerEvent();
				if (sentWorkerEvent != NULL 
					&& SimulationMessaging::getInstVar()->getWorkerEvent() != NULL
					&& sentWorkerEvent->equals(SimulationMessaging::getInstVar()->getWorkerEvent())) {
					ResetEvent(hEvent);
				}
				SimulationMessaging::getInstVar()->unlockWorkerEvent();		
				delete sentWorkerEvent;
			}
			break; 		

			// An error occurred.
		case WAIT_TIMEOUT: 
			SimulationMessaging::getInstVar()->keepAlive();
			break; 

		default:
			cout << "Wait error: " << GetLastError() << endl; 
			break;
		}
	}
#else // UNIX
	int waitReturn = 0;
	struct timespec timeout;
	struct timeval now;
	struct timeval start;
	SimulationMessaging* simMessaging = (SimulationMessaging*)lpParam;

	while (true) {
		waitReturn = 1;
		gettimeofday(&start, NULL);		
		// condition might be signalled before this thread gets blocked
		// so this thread might miss signal and doesn't get wakened
		// if this happens don't want to wait too long to check if there 
		// is new worker event. This is the reason for the loop and 2 sec timeout
		pthread_mutex_lock(&simMessaging->mutex_cond_workerEvent);
		while (timeout.tv_sec - start.tv_sec < 5 * 60) {
			// at this point, there are two possibilities
			// 1. this thread has been awakened, waited < 2 secs, waitReturn is 0
			// 2. this thread has waited full 2 sec timout, waitReturn is non zero
			// in either case, check if there is new worker event.
			if (simMessaging->bNewWorkerEvent) {
				simMessaging->bNewWorkerEvent = false;
				waitReturn = 0;
				break;
			}
			gettimeofday(&now, NULL);
			timeout.tv_sec = now.tv_sec + 2;
			timeout.tv_nsec = now.tv_usec * 1000;
			waitReturn = pthread_cond_timedwait(&simMessaging->cond_workerEvent, &simMessaging->mutex_cond_workerEvent, &timeout);			
		}
		pthread_mutex_unlock(&simMessaging->mutex_cond_workerEvent);
		
		switch (waitReturn) {
			case 0:  { // new event
				WorkerEvent* sentWorkerEvent = simMessaging->sendStatus();
				delete sentWorkerEvent;
				break; 
			}

			case ETIMEDOUT: // time out
				simMessaging->keepAlive();
				break; 

			default:
				cout << "Wait error: " << waitReturn << endl; 
				break;
		}
	}
#endif		
}
#endif
