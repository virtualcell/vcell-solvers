#include <iostream>
#include <fstream>
#include <string>
using namespace std;

#include <VCELL/FVSolver.h>
#include <sys/stat.h>
#include <VCELL/SimTool.h>
#include <VCELL/SimulationMessaging.h>
#include <Exception.h>

#ifdef CH_MPI
#include <mpi.h>
#endif

void vcellExit(int returnCode, string& errorMsg) {
	if (SimulationMessaging::getInstVar() == 0) {
		if (returnCode != 0) {
			cerr << errorMsg << endl;
		}
	} else if (!SimulationMessaging::getInstVar()->isStopRequested()) {
		if (returnCode != 0) {
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_FAILURE, errorMsg.c_str()));
		}
#ifdef USE_MESSAGING
		SimulationMessaging::getInstVar()->waitUntilFinished();
#endif
	}
	delete SimulationMessaging::getInstVar();
	delete SimTool::getInstance();
}

void printUsage() {
#ifdef USE_MESSAGING
	cout << "Arguments : [-d output] [-nz] [-tid taskID] fvInputFile" <<  endl;
#else
	cout << "Arguments : [-d output] [-nz] fvInputFile" <<  endl;
#endif
}

#include <math.h>

int main(int argc, char *argv[])
{
#ifdef CH_MPI
  MPI_Init(&argc, &argv);
#endif
	int returnCode = 0;
	string errorMsg = "Exception : ";

	char* fvInputFile = 0;
	ifstream ifsInput;
	try {
		int taskID = -1;
		if (argc < 2) {
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
				fvInputFile = argv[i];
			}
		}

		// strip " in case that file name has " around
		int fl = strlen(fvInputFile);
		if (fvInputFile[0] == '"' && fvInputFile[fl-1] == '"') {
						fvInputFile[fl-1] = 0;
						fvInputFile ++;
		}
		ifsInput.open(fvInputFile);
		if (!ifsInput.is_open()) {
			cout << "File doesn't exist: " << fvInputFile << endl;
			exit(102);
		}

		FVSolver* fvSolver = new FVSolver(ifsInput, taskID);
		ifsInput.close();

		fvSolver->solve();

	} catch (const char *exStr){
		errorMsg += exStr;
		returnCode = 1;
	} catch (string& exStr){
		errorMsg += exStr;
		returnCode = 1;
	} catch (VCell::Exception& ex){
		errorMsg += ex.getMessage();
		returnCode = 1;
	} catch (...){
		errorMsg += "unknown error";
		returnCode = 1;
	}

	if (ifsInput.is_open()) {
		ifsInput.close();
	}
	vcellExit(returnCode, errorMsg);
#ifdef CH_MPI
  MPI_Finalize();
#endif
	return returnCode;
}
