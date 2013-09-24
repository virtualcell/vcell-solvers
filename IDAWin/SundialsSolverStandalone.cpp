#ifdef _DEBUG
//#define _CRTDBG_MAP_ALLOC
#ifdef _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>
#else
//#include <vld.h>
#endif
#endif

#ifdef USE_MESSAGING
#include <VCELL/SimulationMessaging.h>
#endif
#include "VCellCVodeSolver.h"
#include "VCellIDASolver.h"
#include "OdeResultSet.h"
#include "StoppedByUserException.h"

#include <stdio.h>
#include <iomanip>
#include <fstream>
#include <sstream>
using std::ifstream;
using std::stringstream;

#include <memory.h>
#include <stdlib.h>

#define CVODE_SOLVER "CVODE"
#define IDA_SOLVER "IDA"

void printUsage() {
	cout << "Usage: SundialsSolverStandalone input output";
#ifdef USE_MESSAGING
	cout << " [-tid 0]" << endl;
#endif
	cout << endl;
}

void loadJMSInfo(istream& ifsInput, int taskID) {
	char *broker = new char[256];
	char *smqusername = new char[256];
	char *password = new char[256];
	char *qname = new char[256];
	char *tname = new char[256];
	char *vcusername = new char[256];
	string nextToken;
	int simKey, jobIndex;

	while (!ifsInput.eof()) {			
		nextToken = "";
		ifsInput >> nextToken;			
		if (nextToken.size() == 0) {
			continue;
		} else if (nextToken[0] == '#') {
			getline(ifsInput, nextToken);
			continue;
		}  else if (nextToken == "JMS_PARAM_END") {
			break;
		} else if (nextToken == "JMS_BROKER") {
			memset(broker, 0, 256 * sizeof(char));
			ifsInput >> broker;
		} else if (nextToken == "JMS_USER") {
			memset(smqusername, 0, 256 * sizeof(char));
			memset(password, 0, 256 * sizeof(char));
			ifsInput >> smqusername >> password;
		} else if (nextToken == "JMS_QUEUE") {
			memset(qname, 0, 256 * sizeof(char));
			ifsInput >> qname;
		} else if (nextToken == "JMS_TOPIC") {
			memset(tname, 0, 256 * sizeof(char));
			ifsInput >> tname;
		} else if (nextToken == "VCELL_USER") {
			memset(vcusername, 0, 256 * sizeof(char));
			ifsInput >> vcusername;
		} else if (nextToken == "SIMULATION_KEY") {
			ifsInput >> simKey;
			continue;
		} else if (nextToken == "JOB_INDEX") {
			ifsInput >> jobIndex;
			continue;
		} 
	}

#ifdef USE_MESSAGING	
	if (taskID >= 0) {
		SimulationMessaging::create(broker, smqusername, password, qname, tname, vcusername, simKey, jobIndex, taskID);
	} else {
		SimulationMessaging::create();
	}
#endif
}

void errExit(int returnCode, string& errorMsg) {	
#ifdef USE_MESSAGING
	if (returnCode != 0) {
		if (SimulationMessaging::getInstVar() != 0 && !SimulationMessaging::getInstVar()->isStopRequested()) {
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_FAILURE, errorMsg.c_str()));
		}	
	}
	if (SimulationMessaging::getInstVar() != 0) {
		SimulationMessaging::getInstVar()->waitUntilFinished();
		delete SimulationMessaging::getInstVar();
	} else {
		if (returnCode != 0) {	
			cerr << errorMsg << endl;
		}
	}
#else
	if (returnCode != 0) {	
		cerr << errorMsg << endl;
	}
#endif
}
#if !defined(SVNVERSION)
#error SVNVERSION version not defined
#endif
#define VCELLSVNQ(x) #x
#define VCELLSVNQUOTE(x) VCELLSVNQ(x)


int main(int argc, char *argv[]) {
    	std::cout 
	    << "Sundials Standalone version $URL$"VCELLSVNQUOTE(SVNVERSION) 
	    << std::endl; 
	cout << setprecision(20);

	int taskID = -1;
	string inputfname;
	string outputfname;
	string solver;
	string errMsg;
	int returnCode = 0;

	if (argc < 3) {
		cout << "Missing arguments!" << endl;
		printUsage();
		exit(1);
	}
	for (int i = 1; i < argc; i ++) {
		if (!strcmp(argv[i], "-tid")) {
#ifdef USE_MESSAGING
			i ++;
			if (i >= argc) {
				cout << "Missing taskID!" << endl;
				printUsage();
				exit(1);
			}
			for (int j = 0; j < (int)strlen(argv[i]); j ++) {
				if (argv[i][j] < '0' || argv[i][j] > '9') {
					cout << "Wrong argument : " << argv[i] << ", taskID must be an integer!" << endl;
					printUsage();
					exit(1);
				}
			}
			taskID = atoi(argv[i]);
#else
			cout << "Wrong argument : " << argv[i] << endl;
			printUsage();
			exit(1);
#endif
		} else {
			inputfname = argv[i];
			i ++;
			outputfname = argv[i];
		}	
	}

	FILE* outputFile = NULL;
	ifstream inputstream(inputfname.c_str());
	try {		
		if (!inputstream.is_open()) {
			throw string("input file [") + inputfname + "] doesn't exit!";
		}

		// Open the output file...		
		if ((outputFile = fopen(argv[2], "w")) == NULL) {
			throw string("Could not open output file[") +  outputfname + "] for writing.";
		}

		string nextToken;		

		while (!inputstream.eof()) {			
			nextToken = "";
			inputstream >> nextToken;	
			if (nextToken.empty()) {
				continue;
			} else if (nextToken[0] == '#') {
				getline(inputstream, nextToken);
				continue;
			} else if (nextToken == "JMS_PARAM_BEGIN") {
				loadJMSInfo(inputstream, taskID);
#ifdef USE_MESSAGING
				SimulationMessaging::getInstVar()->start(); // start the thread
#endif				
			} else if (nextToken == "SOLVER") {
				inputstream >> solver;
				break;
			}
		}

		if (solver.empty()) {
			throw "Solver not defined ";
		}

#ifdef _CRTDBG_MAP_ALLOC
		_CrtMemState s1, s2, s3;
#endif
		errMsg += solver + " solver failed : ";
#ifdef _CRTDBG_MAP_ALLOC
		_CrtMemCheckpoint( &s1 );
#endif
		VCellSundialsSolver* vss = 0;
		if (solver == IDA_SOLVER) {
			vss = new VCellIDASolver();
		} else if (solver == CVODE_SOLVER) {
			vss = new VCellCVodeSolver();
		} else {
			stringstream ss;
			ss << "Solver " << solver << " not defined!";
			throw ss.str();
		}
		vss->readInput(inputstream);
		vss->solve(0, true, outputFile, VCellSundialsSolver::checkStopRequested);

		delete vss;
#ifdef _CRTDBG_MAP_ALLOC
		_CrtMemCheckpoint( &s2 );
		if ( _CrtMemDifference( &s3, &s1, &s2) )
		_CrtMemDumpStatistics( &s3 );
		_CrtDumpMemoryLeaks();
#endif		
	} catch (const char* ex) {
		errMsg += ex;
		returnCode = -1;
	} catch (string& ex) {
		errMsg += ex;
		returnCode = -1;
	} catch (StoppedByUserException) {
		returnCode = 0;  // stopped by user;
	} catch (VCell::Exception& ex) {
		errMsg += ex.getMessage();
		returnCode = -1;
	} catch (...) {
		errMsg += "unknown error";
		returnCode = -1;
	}
	
	if (outputFile != NULL) {
		fclose(outputFile);
	}
	if (inputstream.is_open()) {
		inputstream.close();
	}

	errExit(returnCode, errMsg);
#ifdef _CRTDBG_MAP_ALLOC
	_CrtDumpMemoryLeaks();
#endif
	return returnCode;
}
