/**
 *
 * wrapper functions to be called from fortran 90
 *
 */
#include <iostream>
using namespace std;

#include <VCELL/SimulationMessaging.h>

extern "C"
#ifdef WIN32
void __cdecl LOAD_JMS_INFO(char* broker, int slen1, char* smqusername, int slen2, char* password, int slen3,
		char* qname, int slen4, char* tname, int slen5, char* vcname, int slen6, int* simKey, int* jobIndex, int* taskID) {
#else
void load_jms_info_(char* broker, char* smqusername, char* password, char* qname, char* tname, char* vcname, int* simKey, int* jobIndex, int* taskID) {
#endif
////	char *broker = "tcp://code:2507";
//	char* smqusername = "serverUser";
//	char* password = "cbittech";
//	char* qname = "workerEventDev";
//	char* tname = "serviceControlDev";
//	char* vcname = "fgao";
////	int simKey = 29476281;
////	int jobIndex = 0;
////	int taskID = 0;

//	cout << "c [" << broker << "]" << endl;
//	cout << "c [" << smqusername << "]" << endl;
//	cout << "c [" << password << "]" << endl;
//	cout << "c [" << qname << "]" << endl;
//	cout << "c [" << tname << "]" << endl;
//	cout << "c [" << vcname << "]" << endl;
//	cout << "c [" << *simKey << "]" << endl;
//	cout << "c [" << *jobIndex << "]" << endl;
//	cout << "c [" << *taskID << "]" << endl;

	if (*taskID >= 0) {
		//cout << "in wrapper create messaging" << endl;
		SimulationMessaging::create(broker, smqusername, password, qname, tname, vcname, *simKey, *jobIndex, *taskID);
	} else {
		//cout << "in wrapper create" << endl;
		SimulationMessaging::create();
	}
	SimulationMessaging::getInstVar()->start();
}

extern "C"
#ifdef WIN32
void __cdecl SEND_PROGRESS(double* progress, double* time) {
#else
void send_progress_(double* progress, double* time) {
#endif
	//cout << "in wrapper progress " << *progress << " " << *time << endl;
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_PROGRESS, *progress, *time));
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_DATA, *progress, *time));
}

extern "C"
#ifdef WIN32
void __cdecl SEND_COMPLETE(double* progress, double* time) {
#else
void send_complete_(double* time) {
#endif
	//cout << "in wrapper complete" << endl;
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_COMPLETED, 1, *time));
	SimulationMessaging::getInstVar()->waitUntilFinished();
}

extern "C"
#ifdef WIN32
void __cdecl SEND_FAILED(char* errmsg) {
#else
void send_failed_(char* errmsg) {
#endif
	//cout << "in wrapper failed" << endl;
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_FAILURE, errmsg));
	SimulationMessaging::getInstVar()->waitUntilFinished();
}
