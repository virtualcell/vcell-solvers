#include <utility>
#include <iostream>

#ifdef WIN32
    #include <strstream>   // for ostrstream -- what is Unix version?
#endif

#ifdef LINUX
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#endif
#ifdef UNIX
#include <signal.h>
#endif

#include <VCELL\SimulationMessaging.h>
#include <VCELL\SimTool.h>
#include <jmsconstants.h>

#if defined (SOLARIS) || defined(SOLARIS_GCC)
pthread_mutexattr_t SimulationMessaging::mattr;
pthread_mutex_t SimulationMessaging::mp = PTHREAD_MUTEX_INITIALIZER;
#endif

#ifdef HPUX
pthread_mutexattr_t SimulationMessaging::mattr;
pthread_mutex_t SimulationMessaging::mp = PTHREAD_MUTEX_INITIALIZER;
#endif

#ifdef AIX
pthread_mutexattr_t SimulationMessaging::mattr;
pthread_mutex_t SimulationMessaging::mp = PTHREAD_MUTEX_INITIALIZER;
#endif

#ifdef LINUX
pthread_mutex_t SimulationMessaging::mp = /* PTHREAD_MUTEX_RECURSIVE_NP;*/
      {0, 0, 0, PTHREAD_MUTEX_RECURSIVE_NP, __LOCK_INITIALIZER};
#endif

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

SimulationMessaging *SimulationMessaging::m_inst = NULL;

SimulationMessaging::SimulationMessaging() {
	time(&lastSentEventTime);
	workerEventOutputMode = WORKEREVENT_OUTPUT_MODE_STDOUT;
}

SimulationMessaging::SimulationMessaging(char* broker, char* smqusername, char* passwd, char*qname,  char* tname, char* vcusername, jlong simKey, jint jobIndex, jint taskID, jint ttl) {
	m_qConnect = NULL;
    m_qSession = NULL;
    m_qSender = NULL;
	m_tConnect = NULL;
    m_tSession = NULL;
    m_tSubscriber = NULL;
	workerEvent = NULL;

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
		WSADATA wsaData;
		WORD wVersionRequested = MAKEWORD( 2, 2 );		 
		WSAStartup( wVersionRequested, &wsaData );			
		gethostname(m_hostname, 256);
		//std::cout << "hostname is " << m_hostname << std::endl;		
	}

	time(&lastSentEventTime);

    createCriticalSection();
	workerEventOutputMode = WORKEREVENT_OUTPUT_MODE_MESSAGING;
}

SimulationMessaging::~SimulationMessaging()
{
	if (workerEventOutputMode == WORKEREVENT_OUTPUT_MODE_STDOUT) {
		return;
	}
	try {
		if (m_qConnect != null) {
			m_qConnect->close();
		}
		if (m_tConnect != null) {
			m_tConnect->close();
		}
		destroyCriticalSection();
	} catch (JMSExceptionRef jmse) {
		destroyCriticalSection();
		onException(jmse);
	}
	if (workerEvent != NULL) {
		delete workerEvent;
		workerEvent = NULL;
	}
}

SimulationMessaging* SimulationMessaging::getInstVar() {
	return m_inst;
}

/**
 * Keep trying to start the connection.  Once it is established,
 * setup the message handlers and publishers. (This is synchronized
 * because it can be called from the main thread, and from the
 * onException handler.)
 */
void SimulationMessaging::setupConnection () { //synchronized
    enterCriticalSection();

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
            std::cerr << "error: Cannot connect to Broker - " << m_broker ;
            std::cerr << ".  Try again in  - " << CONNECTION_RETRY_PERIOD/1000 << " seconds." << "\n" ;
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

    leaveCriticalSection();
}

/**
 * Suspend the current thread for "duration" milliseconds.
 */
void SimulationMessaging::delay(jint duration)
{
#ifdef WIN32
    Sleep(duration);
#endif

#ifdef UNIX
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
			std::cout << "Stopped by user" << std::endl;
			SimTool::getInstance()->requestStop();
		}

    } catch(JMSExceptionRef jmse) {
        onException(jmse);
    }
}

void SimulationMessaging::onException(JMSExceptionRef e)
{
	std::cout << "!!!JMSException: " << (const char *)e->getMessage()->toAscii() << std::endl;
	int dropCode = ErrorCodes_ERR_CONNECTION_DROPPED;
	if (ErrorCodes::testException(e, dropCode)) {
		m_connActive = false;
		setupConnection();
		m_connActive = true;
		std::cout << "Connection restored.  Messages will now be accepted again" << std::endl;
	}
}

/**
 * The following functions contain platform-dependent code for managing
 * thread synchronization, and for suspending the current thread for a 
 * specified amount of time.
 */
void SimulationMessaging::createCriticalSection()
{
#ifdef WIN32
    InitializeCriticalSection(&lock);
	InitializeCriticalSection(&lockForWorkerEvent);
#endif

#if defined (SOLARIS) || defined(SOLARIS_GCC)
    /* Initialize an attribute to default value */
    if (pthread_mutexattr_init(&mattr) != 0) {
        fprintf(stderr, "Could not initialize mutex, fatal error.\n");
        exit(1);
    }

#ifdef SOLARIS_2_6
#else
    if (pthread_mutexattr_settype(&mattr , PTHREAD_MUTEX_RECURSIVE) != 0) {
        fprintf(stderr, "Could not set mutex type, fatal error.\n");
        pthread_mutexattr_destroy(&mattr);
        exit(1);
    }
#endif

    if (pthread_mutex_init(&mp, &mattr) != 0) {
        fprintf(stderr, "Could not initialize mutex, fatal error.\n");
        pthread_mutexattr_destroy(&mattr);
        exit(1);
    }
#endif

#ifdef HPUX
	/* Initialize an attribute to default value */
	if (pthread_mutexattr_init(&mattr) != 0) {
		fprintf(stderr, "Could not initialize mutex, fatal error.\n");
		exit(1);
	}

	if (pthread_mutexattr_settype(&mattr , PTHREAD_MUTEX_RECURSIVE) != 0) {
		fprintf(stderr, "Could not set mutex type, fatal error.\n");
		pthread_mutexattr_destroy(&mattr);
		exit(1);
	}

	if (pthread_mutex_init(&mp, &mattr) != 0) {
		fprintf(stderr, "Could not initialize mutex, fatal error.\n");
		pthread_mutexattr_destroy(&mattr);
		exit(1);
	}
#endif

#ifdef AIX
        /* Initialize an attribute to default value */
        if (pthread_mutexattr_init(&mattr) != 0) {
                fprintf(stderr, "Could not initialize mutex, fatal error.\n");
                exit(1);
        }

        if (pthread_mutexattr_settype(&mattr , PTHREAD_MUTEX_RECURSIVE) != 0) {
                fprintf(stderr, "Could not set mutex type, fatal error.\n");
                pthread_mutexattr_destroy(&mattr);
                exit(1);
        }

        if (pthread_mutex_init(&mp, &mattr) != 0) {
                fprintf(stderr, "Could not initialize mutex, fatal error.\n");
                pthread_mutexattr_destroy(&mattr);
                exit(1);
        }
#endif

#ifdef LINUX
// Nothing to do. Static initialization took care of it
#endif
}

void SimulationMessaging::destroyCriticalSection()
{
#ifdef WIN32
    DeleteCriticalSection(&lock);
	DeleteCriticalSection(&lockForWorkerEvent);
#endif

#ifdef UNIX
    pthread_mutex_destroy(&mp);
#if defined (SOLARIS) || defined(SOLARIS_GCC)
    pthread_mutexattr_destroy(&mattr);
#endif
#ifdef HPUX
	pthread_mutexattr_destroy(&mattr);
#endif
#ifdef AIX
        pthread_mutexattr_destroy(&mattr);
#endif
#endif
}

bool SimulationMessaging::enterCriticalSection(bool iftry)
{
#ifdef WIN32
	if (iftry) {
		return (bool)TryEnterCriticalSection(&lock);
	}
    EnterCriticalSection(&lock);
	return true;
#endif

#ifdef UNIX
    int ret;
    if ((ret = pthread_mutex_lock(&mp)) != 0) {
        /* This indicates a programming error. The mutex is recursive. */
        fprintf(stderr, "Cannot acquire mutex, fatal error.\n");
        exit(1);
    }
#endif
}

void SimulationMessaging::leaveCriticalSection()
{
#ifdef WIN32
    LeaveCriticalSection(&lock);
#endif

#ifdef UNIX
    pthread_mutex_unlock(&mp);
#endif
}

bool SimulationMessaging::lockWorkerEvent(bool iftry)
{	
#ifdef WIN32
	if (iftry) {
		return (bool)TryEnterCriticalSection(&lockForWorkerEvent);
	}
    EnterCriticalSection(&lockForWorkerEvent);
	return true;
#endif

#ifdef UNIX
    int ret;
    if ((ret = pthread_mutex_lock(&mp)) != 0) {
        /* This indicates a programming error. The mutex is recursive. */
        fprintf(stderr, "Cannot acquire mutex, fatal error.\n");
        exit(1);
    }
#endif
}

void SimulationMessaging::unlockWorkerEvent()
{
#ifdef WIN32
    LeaveCriticalSection(&lockForWorkerEvent);
#endif

#ifdef UNIX
    pthread_mutex_unlock(&mp);
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
			printf(workerEvent->eventMessage);
			fflush(stdout);
			break;
		case JOB_COMPLETED:
			std::cerr << "Simulation Complete in Main() ... " << std::endl;
			break;
		case JOB_FAILURE:
			std::cerr << workerEvent->eventMessage << std::endl;			
			break;
		}

		return workerEvent;
	} else {
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

		std::cout << "!!!SimulationMessaging::sendStatus [" << (long)m_simKey << ":" << getStatusString(newWorkerEvent->status);
		if (revisedMsg != NULL) {
			std::cout << ":" << revisedMsg;
		} else {
			std::cout << ":" << newWorkerEvent->progress << ":" << newWorkerEvent->timepoint;
		}

		std::cout << "]" << std::endl;
		//send
		try {
			getQueueSender()->send(msg, DeliveryMode_PERSISTENT, Message_DEFAULT_PRIORITY, m_ttl); 	
		} catch (JMSExceptionRef jmse) {
			std::cout << "!!!SimulationMessaging::sendStatus [" << (const char*)jmse->getMessage()->toAscii() << "]" << std::endl;
		}

		time(&lastSentEventTime);
		if (newWorkerEvent->status == JOB_COMPLETED || newWorkerEvent->status == JOB_FAILURE) {
			SetEvent(hMessagingThreadEndEvent);
		}

		return newWorkerEvent;
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

void SimulationMessaging::setWorkerEvent(WorkerEvent* arg_workerEvent) {	
	if (workerEventOutputMode == WORKEREVENT_OUTPUT_MODE_STDOUT) {
		workerEvent = arg_workerEvent;
		sendStatus();
	} else {		
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
				SetEvent(hNewWorkerEvent);
				unlockWorkerEvent();
			}
		}
	}
}

SimulationMessaging* SimulationMessaging::create()
{
	if (m_inst){    
        std::cerr << "Only one instance of SimulationMessaging permitted.  ";
        std::cerr << "Existing instance must be deleted before creating a new one.\n";
    } else {
        m_inst = new SimulationMessaging();
	}

    return(m_inst);
}

SimulationMessaging* SimulationMessaging::create(char* broker, char* smqusername, char* passwd, char* qname, char* tname, char* vcusername, jlong simKey, jint jobIndex, jint taskID, jint ttl)
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

	DWORD dwWaitResult = WaitForSingleObject(hMessagingThreadEndEvent, INFINITE);
	switch (dwWaitResult) {
	case WAIT_OBJECT_0: 
	case WAIT_TIMEOUT: 	
		break; 

	default:
		std::cout << "Wait error: " << GetLastError() << std::endl;
		break;
	}
}

void SimulationMessaging::start() {
	if (workerEventOutputMode == WORKEREVENT_OUTPUT_MODE_STDOUT) {
		return;
	}
#ifdef WIN32
	HANDLE hThread; 
	DWORD IDThread; 

	char messagingThreadEndEventName[256];
	char newWorkerEventName[256];
	sprintf(messagingThreadEndEventName, "MessagingThreadEndEvent_%ld_%d", m_simKey, m_jobIndex);
	sprintf(newWorkerEventName, "NewWorkerEvent_%ld_%d", m_simKey, m_jobIndex);

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
#endif
}


void startMessagingThread(void* lpParam){	
	SimulationMessaging::getInstVar()->setupConnection();

#ifdef WIN32
    DWORD dwWaitResult;
    HANDLE hEvent; 
	
    hEvent = *(HANDLE*)lpParam;  // thread's read event

	while (true) {
		dwWaitResult = WaitForSingleObject(hEvent, 5*60*1000);    // 5 min wait

		switch (dwWaitResult) {
		case WAIT_OBJECT_0: 
			{
				/*
				bool succ = true;
				if (SimulationMessaging::m_inst->workerEvent->status == JOB_PROGRESS) {
					succ = SimulationMessaging::m_inst->enterCriticalSection(true);
				} else {
					succ = SimulationMessaging::m_inst->enterCriticalSection();
				}*/					
			
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
			printf("Wait error: %d\n", GetLastError()); 
			break;
		}
	}
#else
#endif
		
}
