/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */

#include <iostream>
#include <sstream>
using std::stringstream;
using std::cout;
using std::endl;

#include <VCELL/SimTypes.h>
#include <VCELL/SimTool.h>
#include <VCELL/DataSet.h>
#include <VCELL/SimulationMessaging.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/Variable.h>
#include <VCELL/ChomboIF.h>
#include <VCELL/ChomboSpec.h>

#include <float.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>

#if ( !defined(WIN32) && !defined(WIN64) ) // UNIX
#include <unistd.h>
#endif

#define ZIP_FILE_LIMIT 1E9

#define MESH_FILE_EXT ".mesh"
#define MESHMETRICS_FILE_EXT ".meshmetrics"
#define SIM_FILE_EXT ".sim"
#define PARTICLE_FILE_EXT ".particle"
#define LOG_FILE_EXT ".log"
#define ZIP_FILE_EXT ".zip"
#define TID_FILE_EXT ".tid"

#define CHOMBO_SEMIIMPLICIT_SOLVER "CHOMBO_SEMIIMPLICIT_SOLVER"

int zip32(int filecnt, char* zipfile, ...);
int unzip32(char* zipfile, char* file, char* exdir);

SimTool* SimTool::instance = 0;

static int NUM_TOKENS_PER_LINE = 4;
static const int numRetries = 2;
static const int retryWaitSeconds = 5;

SimTool::SimTool()
{
	bSimZip = true;

	simEndTime = 0.0;
	keepEvery = 100;
	bStoreEnable = true;
	baseFileName=0;
	simFileCount=0;
	zipFileCount = 0;
	baseDirName = NULL;
	baseSimName = NULL;
	bLoadFinal = true;

	_timer = 0;
	vcellModel = 0;
	simulation = 0;

	numDiscontinuityTimes = 0;
	discontinuityTimes = 0;

	sundialsRelTol = 1e-7;
	sundialsAbsTol = 1e-9;
	sundialsMaxStep = 0.1;

	solver = CHOMBO_SEMIIMPLICIT_SOLVER;
	pcgRelTol = 1e-8;

	simStartTime = 0;
	bSundialsOneStepOutput = false;
	keepAtMost = 5000;
}

SimTool::~SimTool()
{
	delete baseSimName;
	delete baseDirName;
	delete baseFileName;

	delete[] discontinuityTimes;	
}

void SimTool::setModel(VCellModel* model) {
	if (model == 0) {
		throw "SimTool::setModel(), model can't be null";
	}
	vcellModel = model;
}

void SimTool::setSimulation(SimulationExpression* sim) {
	if (sim == 0) {
		throw "SimTool::setSimulation(), simulation can't be null";
	}
	simulation = sim;
	simulation->setDT_sec(simDeltaTime);
}

void SimTool::setTimeStep(double period) {
	simDeltaTime = period;
}

void SimTool::create() {
	if (instance == 0) {
		instance = new SimTool();
	} else {
		throw "SimTool (singleton) has been created";
	}
}

SimTool* SimTool::getInstance() {
	if (instance == 0) {
		throw "SimTool (singleton) has not been created";
	}
	return instance;
}

void SimTool::requestNoZip() {
	bSimZip = false;
}

bool SimTool::checkStopRequested() {
	 return SimulationMessaging::getInstVar()->isStopRequested();
}

TimerHandle SimTool::getTimerHandle(string& identifier)
{
	if (_timer==NULL){
		_timer = new Timer();
	}
	return _timer->registerID(identifier);
}

void SimTool::startTimer(TimerHandle hnd)
{
	_timer->start(hnd);
}

void SimTool::stopTimer(TimerHandle hnd)
{
	_timer->stop(hnd);
}

double SimTool::getElapsedTimeSec(TimerHandle hnd)
{
	double time=0.0;
	_timer->getElapsedTimeSec(hnd, time);
	return time;
}

void SimTool::setBaseFilename(char *fname) {
	if (fname == 0 || strlen(fname) == 0) {
		throw "invalid base file name for data set";
	}
	baseFileName = new char[strlen(fname) + 1];
	memset(baseFileName, 0, strlen(fname) + 1);
	memcpy(baseFileName, fname, strlen(fname) * sizeof(char));

	// extract directory
	baseDirName = new char[strlen(baseFileName) + 1];
	baseSimName = NULL;

	strcpy(baseDirName, baseFileName);
	char* p = strrchr(baseDirName, DIRECTORY_SEPARATOR);
	if (p == NULL) {
		baseSimName = baseDirName;
		baseDirName = 0;
	} else {
		baseSimName = new char[strlen(p+1) + 1];
		strcpy(baseSimName, p + 1);
		*(p + 1)= 0;
	}
}

static void retryWait(int seconds) {
#if ( defined(WIN32) || defined(WIN64) )
	Sleep(seconds * 1000);
#else
	sleep(seconds);
#endif
}

static FILE* openFileWithRetry(const char* fileName, const char* mode) {
	FILE *fp = NULL;
	for (int retry = 0; retry < numRetries; retry ++) {
		fp = fopen(fileName, mode);

		if (fp != NULL) {
			break;
		}
		if (retry < numRetries - 1) {
			cout << "SimTool, error opening log file <" << fileName << ">, trying again" << endl;
			retryWait(retryWaitSeconds);
		}
	}
	return fp;
}

static bool zipUnzipWithRetry(bool bZip, char* zipFileName, char* simFileName, char* errmsg) {
	bool bSuccess = true;
	int retcode = 0;
	for (int retry = 0; retry < numRetries; retry ++) {
		try {
			if (bZip) {
				retcode = zip32(1, zipFileName, simFileName);						
			} else {
				retcode = unzip32(zipFileName, simFileName, NULL);
			}
			break;
		} catch (const char* ziperr) {
			sprintf(errmsg, "%s", ziperr);
		} catch (...) {
			sprintf(errmsg, "SimTool::updateLog(), adding .sim to .zip failed.");						
		}
		bSuccess = false;
		if (retry < numRetries - 1) {
			retryWait(retryWaitSeconds);
			cout << "SimTool::updateLog(), adding .sim to .zip failed, trying again" << endl;
		}
	}
	if (bSuccess && retcode != 0) {
		sprintf(errmsg, "Writing zip file <%s> failed, return code is %d", zipFileName, retcode);
		bSuccess = false;
	}
	return bSuccess;
}

void SimTool::loadFinal()
{
	if (simulation == NULL){
		printf("SimTool.loadFinal(), sim=NULL just returning\n");
		return;
	}

	//
	// read '.log' file to determine simulation time and iteration
	//

	bool bStartOver = true;

	char logFileName[128];
	char zipFileName[128];
	char dataFileName[128];

	sprintf(logFileName,"%s%s", baseFileName, LOG_FILE_EXT);

	FILE* tidFP = lockForReadWrite();
	FILE* logFP = fopen(logFileName, "r");

	if (logFP != NULL) {
		bStartOver = false; // log file exists, there is old data

		struct stat buf;
		sprintf(zipFileName,"%s00%s", baseFileName, ZIP_FILE_EXT);
		if (stat(zipFileName, &buf)) {
			bSimZip = false;
			NUM_TOKENS_PER_LINE = 3;
		} else {
			bSimZip = true;
			NUM_TOKENS_PER_LINE = 4;
		}

		simStartTime = -1;
		int tempIteration = -1, tempFileCount = 0, tempZipCount = 0;

		while (!feof(logFP)){
			char logBuffer[201];
			if (!fgets(logBuffer, 200, logFP)){
				break;
			}
			//
			// look for line with a valid filename (includes basename)
			//
			if (strstr(logBuffer, baseSimName)){
				//
				// parse iteration number and time
				//
				int numTokens = 0;
				if (bSimZip) {
					numTokens = sscanf(logBuffer, "%d %s %s %lg", &tempIteration, dataFileName, zipFileName, &simStartTime);
				} else {
					numTokens = sscanf(logBuffer, "%d %s %lg", &tempIteration, dataFileName, &simStartTime);
				}
				if (numTokens != NUM_TOKENS_PER_LINE){
					printf("SimTool::load(), error reading log file %s, reading iteration\n", logFileName);
					printf("error in line %d = '%s'\n",tempFileCount,logBuffer);
					bStartOver = true;
					break;
				}
			}
			tempFileCount++;
		} // while (!feof(logFP))

		// close log file
		fclose(logFP);

		if (tempIteration <= 0  || simStartTime <= 0) {
			bStartOver = true;
		}

		if (!bStartOver) {
			if (bSimZip) {
				// check if zip file exists
				char zipFileAbsoluteName[512];
				if (strchr(zipFileName, DIRECTORY_SEPARATOR) != 0) {
					strcpy(zipFileAbsoluteName,zipFileName);
				} else {
					if (baseDirName == 0) { // current directory
						strcpy(zipFileAbsoluteName, zipFileName);
					} else {
						sprintf(zipFileAbsoluteName,"%s%s",baseDirName, zipFileName);  // Jim Schaff made this change ... look at it.
					}
				}
				if (stat(zipFileAbsoluteName, &buf)) {
					cout << "SimTool::loadFinal(), unable to open zip file <" << zipFileAbsoluteName << ">" << endl;
					bStartOver = true;
				} else {
					// unzip the file (without directory) into exdir, currently we
					// unzip the file to the current working directory
					char errmsg[128];
					zipUnzipWithRetry(false, zipFileAbsoluteName, dataFileName, errmsg);
				}
			}			
			
			// otherwise check if sim file exists
			if (stat(dataFileName, &buf)) {
				cout << "SimTool::loadFinal(), unable to open sim file <" << dataFileName << ">" << endl;	
				bStartOver = true;
			} else {
				try {
					DataSet::read(dataFileName, simulation);
					simulation->setCurrIteration(tempIteration);
					simFileCount = tempFileCount;

					if (bSimZip) {
						remove(dataFileName);
						zipFileCount = getZipCount(zipFileName);
						// wrong zip file Name
						if (zipFileCount < 0) { // should never happen
							bStartOver = true;
						}  else {
							// check if this zip file is already big enough
							if (stat(zipFileName, &buf) == 0) {
								if (buf.st_size > ZIP_FILE_LIMIT) {
									zipFileCount ++;
								}
							}
						}
					} 
					
				} catch (const char* msg) {
					cout << "SimTool::loadFinal() : dataSet.read(" << dataFileName << " failed : " << msg << endl;
					bStartOver = true;
				} catch (...) {
					cout << "SimTool::loadFinal() : dataSet.read(" << dataFileName << " failed : unexpected error" << endl;
					bStartOver = true;
				}
			}			
		}  // if (!bStartOver) 
	} // if (logFP != NULL)

	// close tid file
	if (tidFP != 0) {
		fclose(tidFP);
	}
	if (bStartOver) {
		clearLog();
	}
}

FILE* SimTool::lockForReadWrite() {
	int myTaskID = SimulationMessaging::getInstVar()->getTaskID();
	if (myTaskID < 0) {
		return 0;
	}

	char tidFileName[128];
	sprintf(tidFileName,"%s%s", baseFileName, TID_FILE_EXT);

	bool bExist = false;

	struct stat buf;
	if (stat(tidFileName, &buf) == 0) { // if exists
		bExist = true;
	}
	
	FILE* fp = openFileWithRetry(tidFileName, bExist ? "r+" : "w+");

	if (fp == 0){
		char errmsg[512];
		sprintf(errmsg, "SimTool::lockForReadWrite() - error opening .tid file <%s>", tidFileName);
		throw errmsg;
	}
	if (bExist) {
		int taskIDInFile = 0;
		int numRead = fscanf(fp, "%d", &taskIDInFile);
		if (numRead == 1) {
			if (myTaskID < taskIDInFile) {
				cout << "there is a new process running the simulation, exit..." << endl;
				exit(0);
			}
			if (myTaskID == taskIDInFile) { // it's me
				return fp;
			}
		}
		rewind(fp);
	}
	fprintf(fp, "%5d", myTaskID);
	fflush(fp);

	return fp;
}

void SimTool::updateLog(double progress, double time, int iteration)
{
	FILE *logFP;
	char simFileName[128];
	char logFileName[128];
	char zipFileName[128];
	char particleFileName[128];

	bool bSuccess = true;
	char errmsg[512];
	FILE* tidFP = lockForReadWrite();

	// write sim files to local
	if (bSimZip) {
		sprintf(simFileName,"%s%.4d%s",baseSimName, simFileCount, SIM_FILE_EXT);
	} else {
		sprintf(simFileName,"%s%.4d%s",baseFileName, simFileCount, SIM_FILE_EXT);
	}
	sprintf(particleFileName,"%s%s",simFileName, PARTICLE_FILE_EXT);

	simulation->writeData(simFileName);

	struct stat buf;
	sprintf(logFileName,"%s%s",baseFileName, LOG_FILE_EXT);

	logFP = openFileWithRetry(logFileName, "a");

	if (logFP == 0) {
		sprintf(errmsg, "SimTool::updateLog() - error opening log file <%s>", logFileName);
		bSuccess = false;
	} else {
		// write zip file first, then write log file, in case that
		// zipping fails
		if (bSimZip) {
			sprintf(zipFileName,"%s%.2d%s",baseFileName, zipFileCount, ZIP_FILE_EXT);
			int retcode = 0;
			struct stat buf;
			if (stat(particleFileName, &buf) == 0) {	// has particle
				retcode = zip32(2, zipFileName, simFileName, particleFileName);
				remove(particleFileName);
			} else {
				bSuccess = zipUnzipWithRetry(true, zipFileName, simFileName, errmsg);
			}
			remove(simFileName);

			// write the log file
			if (bSuccess) {
				char zipFileNameWithoutPath[512];
				char simFileNameWithoutPath[512];
				sprintf(simFileNameWithoutPath,"%s%.4d%s",baseSimName, simFileCount, SIM_FILE_EXT);
				sprintf(zipFileNameWithoutPath,"%s%.2d%s",baseSimName, zipFileCount, ZIP_FILE_EXT);
				fprintf(logFP,"%4d %s %s %.15lg\n", iteration, simFileNameWithoutPath, zipFileNameWithoutPath, time);

				if (stat(zipFileName, &buf) == 0) { // if exists
					if (buf.st_size > ZIP_FILE_LIMIT) {
						zipFileCount ++;
					}
				}
			}
		} else { // old format, no zip
			char simFileNameWithoutPath[512];
			sprintf(simFileNameWithoutPath,"%s%.4d%s",baseSimName, simFileCount, SIM_FILE_EXT);
			fprintf(logFP,"%4d %s %.15lg\n", iteration, simFileNameWithoutPath, time);
		}
	}
	// close log file
	fclose(logFP);
	// close tid file
	if (tidFP != 0) {
		fclose(tidFP);
	}

	if (bSuccess) {
		SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_DATA, progress, time));
		simFileCount++;
	} else {
		throw errmsg;
	}
}

int SimTool::getZipCount(char* zipFileName) {
	char* p = strstr(zipFileName, ZIP_FILE_EXT);
	if (p == NULL) {
		return -1;
	}

	char str[3];
	strncpy(str, p - 2, 2 * sizeof(char));
	str[2] = 0;
	return atoi(str);
}

void SimTool::clearLog()
{
	simStartTime = 0;
	simFileCount = 0;
	zipFileCount = 0;

	FILE *fp;
	char logFileName[256];
	char buffer[256];

	// remove mesh file
	sprintf(buffer,"%s%s",baseFileName, MESH_FILE_EXT);
	remove(buffer);

	sprintf(buffer,"%s%s",baseFileName, MESHMETRICS_FILE_EXT);
	remove(buffer);

	sprintf(buffer,"%s%s",baseFileName, ZIP_FILE_EXT);
	remove(buffer);

	sprintf(buffer,"%s00%s",baseFileName, ZIP_FILE_EXT);
	remove(buffer);

	sprintf(logFileName,"%s%s",baseFileName, LOG_FILE_EXT);

	if ((fp=fopen(logFileName, "r"))==NULL){
		printf("error opening log file <%s>\n", logFileName);
		return;
	}

	char simFileName[128];
	char zipFileName[128];
	int iteration, oldCount=-1, count;
	double time;

	while (true) {
		int numTokens  = 0;
		if (bSimZip) {
			numTokens =  fscanf(fp,"%4d %s %s %lg\n", &iteration, simFileName, zipFileName, &time);
		} else {
			numTokens =  fscanf(fp,"%4d %s %lg\n", &iteration, simFileName, &time);
		}
		if (numTokens != NUM_TOKENS_PER_LINE){
			break;
		}
		char *dotSim = strstr(simFileName,SIM_FILE_EXT);
		if (!dotSim) {
			continue;
		}
		*dotSim = '\0';
		sprintf(buffer,"%s%s", simFileName, SIM_FILE_EXT);
		remove(buffer);
		sprintf(buffer,"%s%s%s",simFileName, SIM_FILE_EXT, PARTICLE_FILE_EXT);
		remove(buffer);
		if (bSimZip) {
			count = getZipCount(zipFileName);
			if (oldCount != count && count >= 0) {
				remove(zipFileName);
				oldCount = count;
			}
		}
	}

	fclose(fp);
	printf("SimTool::clearLog(), removing log file %s\n",logFileName);
	remove(logFileName);
}

void SimTool::setSolver(string& s) {
	if (s.length() == 0 || s != CHOMBO_SEMIIMPLICIT_SOLVER) {
		stringstream ss;
		ss << "SimTool::setSolver(), unknown solver : " << s;
		throw ss.str();
	}
	solver = s;
}

void SimTool::start() {
	simulation->resolveReferences();
	simulation->initSimulation();
	// always clear log, don't know how to load final
	clearLog();

	if (checkStopRequested()) {
		return;
	}

	if (simulation == NULL) {
		throw "NULL simulation";
	}

	if (bStoreEnable && (baseFileName == NULL || strlen(baseFileName) == 0)) {
		throw "Invalid base file name for dataset";
	}

	char message[256];
	sprintf(message, "simulation [%s] started", baseSimName);
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_STARTING, message));

	//
    // destroy any partial results from unfinished iterations
    //
    double percentile = simStartTime/simEndTime;
    double increment = 0.01;
	double lastSentPercentile = percentile;
	//
    // store initial log if enabled
    //
	if (simulation->getCurrIteration()==0) {
		// simulation starts from scratch
		if (bStoreEnable){
			updateLog(0.0, 0.0, 0);
		} else {
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_DATA, 0, 0));
		}
	} else {
		// simulation continues from existing results, send data message
		SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_DATA, percentile, simStartTime));
	}
	//
	// iterate up to but not including end time
	//
	double epsilon = 1e-12;

	while (true) {
		if (simulation->getTime_sec() + simulation->getDT_sec() > simEndTime + epsilon) {
			break;
		}

		if (checkStopRequested()) {
			return;
		}

		simulation->iterate();
		simulation->update();

		if (checkStopRequested()) {
			return;
		}

		if (simulation->getCurrIteration() % keepEvery == 0 || simulation->getTime_sec() > simEndTime - epsilon){
			if (bStoreEnable){
				updateLog(percentile,simulation->getTime_sec(), simulation->getCurrIteration());
            }
        }
		percentile = (simulation->getTime_sec() - simStartTime)/(simEndTime - simStartTime);
		if (percentile - lastSentPercentile >= increment) {
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_PROGRESS, percentile, simulation->getTime_sec()));
			lastSentPercentile = percentile;
		}
	}

	if (checkStopRequested()) {
		return;
	} 
	
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_PROGRESS, 1.0, simulation->getTime_sec()));
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_COMPLETED, percentile, simulation->getTime_sec()));	
}