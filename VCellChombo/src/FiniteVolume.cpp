#ifdef CH_CYGWIN
#include <CH_Timer.H>
#endif
#include <parstream.H>
#include <iostream>
#include <fstream>
#include <sstream>
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

static bool bNormalReturn = false;

void printUsage() {
	pout() << "Arguments : [-d output] [-nz]";
#ifdef USE_MESSAGING
	pout() << " [-tid taskID]";
#endif
	pout() << " fvInputFile" <<  endl;
}

void vcellExit(int returnCode, string& errorMsg)
{
	if (SimulationMessaging::getInstVar() == 0) {
		if (returnCode != 0) {
			cerr << errorMsg << endl;
		}
	} else if (!SimulationMessaging::getInstVar()->isStopRequested()) {
		if (returnCode != 0) {
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_FAILURE, errorMsg.c_str()));
		}
#ifdef USE_MESSAGING
		if (SimTool::getInstance()->isRootRank())
		{
			SimulationMessaging::getInstVar()->waitUntilFinished();
		}
#endif
	}
	delete SimulationMessaging::getInstVar();
	delete SimTool::getInstance();

#ifdef CH_MPI
	if (returnCode != 0)
	{
		pout() << "MPI::Abort starting" << endl;
		//MPI::COMM_WORLD.Abort(returnCode);
		MPI_Abort(MPI_COMM_WORLD,returnCode);
		
		pout() << "MPI::Abort complete" << endl;
	}
#endif
}

bool bConsoleOutput = true;
char stdErrBuffer[1024];
void onExit()
{
	if (bNormalReturn)
	{
		return;
	}
	// chombo errors
	if (strlen(stdErrBuffer) > 0)
	{
		string errMsg = "";
		if (!bConsoleOutput)
		{
			errMsg = stdErrBuffer;
		}
		vcellExit(1, errMsg);
	}
}

#if !defined(SVNVERSION)
#error SVNVERSION version not defined
#endif
#define VCELLSVNQ(x) #x
#define VCELLSVNQUOTE(x) VCELLSVNQ(x)

int main(int argc, char *argv[])
{
#ifdef CH_MPI
	const char * const mpiStatus = "MPI ";
#else
	const char * const mpiStatus = "singleThread ";
#endif

	pout()
	<< "Chombo solver " << mpiStatus << VCELLSVNQUOTE(CH_SPACEDIM)"D version $URL$"VCELLSVNQUOTE(SVNVERSION)
	<< std::endl;
#ifdef CH_MPI
	pout() << "MPI::Init starting" << endl;
  //MPI::Init(argc, argv);
  	MPI_Init(&argc, &argv);
	pout() << "MPI::Init complete" << endl;
#endif

	char* timerEnv = getenv("CH_TIMER");
	pout() << "********** CH_TIMER is " << (timerEnv == NULL ? "off" : "on") << " **********" << endl;

	int returnCode = 0;
	string errorMsg = "Exception : ";

	char* fvInputFile = 0;
	ifstream ifsInput;
	bool bContinue = true;
	try {
		int taskID = -1;
		if (argc < 2) {
			errorMsg = "Missing arguments";
			pout() << errorMsg << endl;
			printUsage();
			returnCode = 1;
			bContinue = false;
		}
		if (bContinue)
		{
			for (int i = 1; i < argc; i ++) {
				if (!strcmp(argv[i], "-tid")) {
#ifdef USE_MESSAGING
					i ++;
					if (i >= argc) {
						errorMsg =  "Missing taskID!";
						pout() << errorMsg << endl;
						printUsage();
						returnCode = 1;
						bContinue = false;
					}
					for (int j = 0; j < (int)strlen(argv[i]); j ++) {
						if (argv[i][j] < '0' || argv[i][j] > '9') {
							stringstream ss;
							ss << "Wrong argument : " << argv[i] << ", taskID must be an integer!" << endl;
							errorMsg =  ss.str();
							pout() << errorMsg;
							printUsage();
							returnCode = 1;
							bContinue = false;
						}
					}
					taskID = atoi(argv[i]);
					if (taskID >= 0)
					{
						bConsoleOutput = false;
					}
#else
					stringstream ss;
					ss << "Wrong argument : " << argv[i] << endl;
					errorMsg =  ss.str();
					pout() << errorMsg;
					printUsage();
					returnCode = 1;
					bContinue = false;
#endif
				} else {
					fvInputFile = argv[i];
				}
			}
			if (bContinue)
			{
				atexit(onExit);
				// redirect std::cerr because Chombo would write out to std::cerr and exit.
				//std::cerr.rdbuf(stdErrBuffer.rdbuf());
				stdErrBuffer[0] = '\0';
				setvbuf (stderr , stdErrBuffer , _IOFBF , 1024 );
				// strip " in case that file name has " around
				int fl = strlen(fvInputFile);
				if (fvInputFile[0] == '"' && fvInputFile[fl-1] == '"') {
								fvInputFile[fl-1] = 0;
								fvInputFile ++;
				}
				ifsInput.open(fvInputFile);
				if (!ifsInput.is_open()) {
					stringstream ss;
					ss << "Solver input file fvinput doesn't exist: " << fvInputFile << endl;
					errorMsg =  ss.str();
					pout() << errorMsg;
					returnCode = 102;
					bContinue = false;
				}

				if (bContinue)
				{
					FVSolver* fvSolver = new FVSolver(ifsInput, taskID);
					ifsInput.close();

					pout( ) << "start solve" << endl;
					fvSolver->solve();
					pout( ) << "end solve" << endl;
				}
			}
		}
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
	pout() << endl << "MPI::Finalize starting" << endl;
	//MPI::Finalize();
	MPI_Finalize();
	pout() << "MPI::Finalize complete" << endl;
#endif
	
	bNormalReturn = true;
	return returnCode;
}
