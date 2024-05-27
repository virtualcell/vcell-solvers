#include <iostream>
#include <fstream>
#include <string>
using namespace std;

#include <VCELL/FVSolver.h>
#include <sys/stat.h>
#include <VCELL/SimTool.h>
#include <VCELL/SimulationMessaging.h>
#include <VCELL/GitDescribe.h>
#include <Exception.h>
#include <vcellhybrid.h>

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
}

void printUsage() {
#ifdef USE_MESSAGING
	cout << "Arguments : [-d output] [-nz] [-tid taskID] fvInputFile" <<  endl;
#else
	cout << "Arguments : [-d output] [-nz] fvInputFile" <<  endl;
#endif
}

int main(int argc, char *argv[])
{
	std::cout
	    << "Finite Volume version " << g_GIT_DESCRIBE << " with smoldyn version " << VERSION
		<< std::endl;

	vcellhybrid::setHybrid(); //get smoldyn library in correct state
  	int returnCode = 0;
	string errorMsg = "Exception : ";

	char* outputPath = 0;
	char* fvInputFile = 0;
	ifstream ifsInput;
	FVSolver* fvSolver = NULL;

	bool bSimZip = true;
	try {
		int taskID = -1;
		if (argc < 2) {
			cout << "Missing arguments!" << endl;
			printUsage();
			exit(1);
		}
		for (int i = 1; i < argc; i ++) {
			if (!strcmp(argv[i], "-nz")) {
				bSimZip = false;
			} else if (!strcmp(argv[i], "-d")) {
				i ++;
				if (i >= argc) {
					cout << "Missing output directory!" << endl;
					printUsage();
					exit(1);
				}
				outputPath = argv[i];
			} else if (!strcmp(argv[i], "-tid")) {
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
		struct stat buf;
		if (outputPath != 0 && stat(outputPath, &buf)) {
			cerr << "Output directory [" << outputPath <<"] doesn't exist" << endl;
			exit(1);
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

		fvSolver = new FVSolver(ifsInput, taskID, outputPath, bSimZip);
		ifsInput.close();

		if(fvSolver->getNumVariables() == 0){
			//sims with no reactions and no diffusing species cause exit logic to 'wait' forever
			//never sending a job failed or job finished message and never cleaning up threads
			throw invalid_argument("FiniteVolume error: Must have at least 1 variable or reaction to solve");
		}
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
	} catch (std::exception & e) {
		errorMsg += e.what(); 
		returnCode = 1;
	} catch (...){
		errorMsg += "unknown error";
		returnCode = 1;
	}

	if (ifsInput.is_open()) {
		ifsInput.close();
	}
	vcellExit(returnCode, errorMsg);
	delete fvSolver;
	return returnCode;
}
