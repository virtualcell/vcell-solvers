#include <iostream>
#include <string>
#include <fstream>
#include <string.h>
#include <stdlib.h>
#include <Exception.h>
#ifdef USE_MESSAGING
#include <VCELL/SimulationMessaging.h>
#endif
#include "Gibson.h"
using namespace std;

static void printUsage() {
	cout << "Usage: VCellStoch {gibson|gillespie} input_filename output_filename";
#ifdef USE_MESSAGING
	cout << " [-tid 0]" << endl;
#endif
	cout << endl;
}

static void loadJMSInfo(istream& ifsInput, int taskID) {
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

static void errExit(int returnCode, string& errorMsg) {	
#ifdef USE_MESSAGING
	if (returnCode != 0) {	
		if (!SimulationMessaging::getInstVar()->isStopRequested()) {
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_FAILURE, errorMsg.c_str()));
		}	
	}
	SimulationMessaging::getInstVar()->waitUntilFinished();
	delete SimulationMessaging::getInstVar();
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

/* This file is the entrance of the Virtual Cell stochastic simulation package.
 * It parses the commandline arguments to load different simulators. Four parameters
 * are required for the command. The Usage is: 
 * VCellStoch gibson[gillespie] input_filename output_filename. 
 *
 * @Author: Tracy LI
 * @version:1.0 Beta
 * @CCAM,UCHC. May 26,2006
 */
int main(int argc, char *argv[])
{
    std::cout 
	    << "Stochastic simulation version $URL$" VCELLSVNQUOTE(SVNVERSION)
	    << std::endl; 
	if (argc != 4 && argc != 6) {
		cout << "Wrong arguments!" << endl;
		printUsage();
		exit(-1);
	}

	ifstream inputstream(argv[2]);
	if (!inputstream.is_open()) {
		cerr <<  "input file [" << argv[2] << "] doesn't exit!" << endl;
		exit(-1);
	}

	char* solver = argv[1];
	char* inputfile = argv[2];
	char* outputfile = argv[3];
	int taskID = -1;
	if (argc == 6) {
		taskID = atoi(argv[5]);
	}

	string errMsg = "Gibson solver failed : ";
	int returnCode = 0;
	
	try {

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
				break;
			}
		}
		inputstream.close();

		string s2(solver);
				
		if (s2.compare("gibson")==0)
		{
			Gibson *gb=new Gibson(inputfile, outputfile); // e.g 
//			Gibson *gb = new Gibson("c:/sim.txt","c:/sim_out.txt");
   			gb->march();
			delete gb;
		}
		else if (s2.compare("gillespie")==0)
		{
			cout << "Gillespie method is under development.";
		}

	} catch (string& ex) {
		errMsg += ex;
		returnCode = -1;
	} catch (std::exception& ex) {
		errMsg += ex.what();
		returnCode = -1;
	} catch (const char* ex) {
		errMsg += ex;
		returnCode = -1;
	} catch (...) {
		errMsg += "unknown error";
		returnCode = -1;
	}

	errExit(returnCode, errMsg);
	return returnCode;
}//end of main()
