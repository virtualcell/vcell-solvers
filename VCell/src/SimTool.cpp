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
#include <VCELL/Simulation.h>
#include <VCELL/FVDataSet.h>
#include <VCELL/SimulationMessaging.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/Solver.h>
#include <VCELL/Variable.h>
#include <VCELL/CartesianMesh.h>
#include <VCELL/VolumeRegion.h>
#include <VCELL/MembraneRegion.h>
#include <VCELL/FVUtils.h>
#include <VCELL/PostProcessingBlock.h>
#include <VCELL/PostProcessingHdf5Writer.h>
#include <VCELL/VolumeParticleVariable.h>
#include <VCELL/MembraneParticleVariable.h>
#include <VCELL/Element.h>
#include <vcellhybrid.h>

#include <float.h>
#include <math.h>
#include <time.h>
#include <sys/timeb.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <VCELL/ZipUtils.h>

#include <algorithm>
using std::min;
using std::max;

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
#define HDF5_FILE_EXT ".hdf5"

/*
#ifdef VCELL_HYBRID
	void smoldynOneStep(simptr sim);
	void vcellhybrid::smoldynOneStep(simptr sim) {
		::smoldynOneStep(sim);
	}
	simptr smoldynInit(SimTool* simTool, string& root);
	simptr vcellhybrid::smoldynInit(SimTool* simTool, string& root) {
		return ::smoldynInit(simTool, root);
	}
	void smoldynEnd(simptr sim);
	void vcellhybrid::smoldynEnd(simptr sim) {
		::smoldynEnd(sim);
	}
}
#endif
*/

SimTool* SimTool::instance = 0;

static int NUM_TOKENS_PER_LINE = 4;
static const int numRetries = 2;
static const int retryWaitSeconds = 5;

SimTool::SimTool()
	:bSimZip(true ),
	 vcellModel(0),
	simulation(0),
	_timer(0),
	bSimFileCompress(false),
	simEndTime(0),
	simStartTime(0),
	bCheckSpatiallyUniform(false),
	simDeltaTime(0),
	smoldynStepMultiplier(1),
	keepEvery(100),
	bStoreEnable(true),
	baseFileName(0),
	simFileCount(0),
	baseSimName(0),
	baseDirName(0 ),
	zipFileCount(0),
	solver(FV_SOLVER ),
    discontinuityTimes(0),
	numDiscontinuityTimes(0),
	bLoadFinal(true ),
	sundialsSolverOptions( ),
	spatiallyUniformAbsTol(1e-6 ),
	spatiallyUniformRelTol(1e-3),
	pcgRelTol(1e-8),

	bSundialsOneStepOutput(false),
	keepAtMost(5000),

	 serialScanParameterValues(0),
	numSerialParameterScans(0),

	postProcessingHdf5Writer(0),
	smoldynSim(0 ),
	smoldynInputFile("" )
{ }

SimTool::~SimTool()
{
	if (baseSimName != baseDirName) {
		delete[] baseSimName;
	}
	delete[] baseDirName;
	delete[] baseFileName;

	delete[] discontinuityTimes;

	for (int i = 0; i < numSerialParameterScans; i ++) {
		delete[] serialScanParameterValues[i];
	}
	delete[] serialScanParameterValues;

	delete postProcessingHdf5Writer;
}

void SimTool::setModel(VCellModel* model) {
	if (model == 0) {
		throw "SimTool::setModel(), model can't be null";
	}
	vcellModel = model;
}

void SimTool::setSimulation(Simulation* sim) {
	if (sim == 0) {
		throw "SimTool::setSimulation(), simulation can't be null";
	}
	simulation = sim;
	simulation->setDT_sec(simDeltaTime);
}

void SimTool::setTimeStep(double period) {
	simDeltaTime = period;
}

void SimTool::setSmoldynStepMultiplier(int steps) {
    if(steps <= 0) {
        throw "SimTool::setSmoldynStepMultiplier(), smoldynStepMultiplier must be a positive integer";
    }
	smoldynStepMultiplier = steps;
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

void SimTool::showSummary(FILE *fp)
{
	fprintf(fp,"--------------- sim summary ----------------\n");
	simulation->getMesh()->showSummary(fp);
	fprintf(fp,"\ttime = %lg sec\n",simulation->getTime_sec());
	fprintf(fp,"\tdT   = %lg sec\n",simulation->getDT_sec());
	fprintf(fp,"--------------------------------------------\n");
	if (_timer){
		fprintf(fp,"--------------- benchmark times ------------\n");
		_timer->show();
		fprintf(fp,"--------------------------------------------\n\n\n");
	}
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
			//	retcode = zip32(1, zipFileName, simFileName);
				addFilesToZip(zipFileName, simFileName);
			} else {
			//	retcode = unzip32(zipFileName, simFileName, NULL);
				extractFileFromZip(zipFileName, simFileName);
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
	if (!bLoadFinal) {
		clearLog();
		return;
	}

	if (smoldynSim != NULL) {
		clearLog();
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
		int numTimes = 0;

		while (!feof(logFP)){
			char logBuffer[1024];
			if (!fgets(logBuffer, 1024, logFP)){
				break;
			}
			numTimes ++;
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

			if (!bStartOver) {
				// otherwise check if sim file exists
				if (stat(dataFileName, &buf)) {
					cout << "SimTool::loadFinal(), unable to open sim file <" << dataFileName << ">" << endl;
					bStartOver = true;
				} else {
					try {
						if (postProcessingHdf5Writer != NULL) {
							bStartOver = !postProcessingHdf5Writer->loadFinal(numTimes);
						}

						if (!bStartOver) {
							FVDataSet::read(dataFileName, simulation);
							simulation->setCurrIteration(tempIteration);
							// set start time on sundials
							if (isSundialsPdeSolver() || isVCellPetscSolver()) {
								simulation->setSimStartTime(simStartTime);
							}
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
						}
					} catch (const char* msg) {
						cout << "SimTool::loadFinal() : dataSet.read(" << dataFileName << " failed : " << msg << endl;
						bStartOver = true;
					} catch (...) {
						cout << "SimTool::loadFinal() : dataSet.read(" << dataFileName << " failed : unexpected error" << endl;
						bStartOver = true;
					}
				}
			} // if (!bStartOver)
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
void SimTool::checkTaskIdLockFile(){
	FILE* fp = lockForReadWrite();
	if (fp != 0 ) {
		fclose(fp);
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
				ostringstream msg;
				msg <<  "Starting simulation with taskid=" << myTaskID <<" but lockfile has later taskid="
						<< taskIDInFile << std::ends;
				throw std::runtime_error(msg.str( ));
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

std::string getFileName(const std::string& path)
{
	size_t i = path.length() - 1;
	while (i>0)
	{
      if (path[i] == '/' || path[i] == '\\')
      {
         return path.substr(i+1);
      }
      i--;
      if (i == 0){
      	break;
      }
   }

   return path;
}

void SimTool::updateLog(double progress, double time, int iteration)
{
	if (bStoreEnable) {
		FILE *logFP;
		char simFileName[128];
		char logFileName[128];
		char zipFileName[128];
		char particleFileName[128];

		bool bSuccess = true;
		char errmsg[512];
		FILE* tidFP = lockForReadWrite();

		struct stat buf;
#if ( defined(WIN32) || defined(WIN64) ) // Windows
		wstring TempPath;
		wchar_t wcharPath[128];
		if (GetTempPathW(128, wcharPath)){
			TempPath = wcharPath;
		}else{
			throw "failed to obtain system temp directory";
		}
		std::string tempDir( TempPath.begin(), TempPath.end() );
#else
		std::string tempDir = "/tmp/";
		char const *folder = getenv("TMPDIR");
		if (folder != 0)
    		tempDir = folder;		
    	if (tempDir.at(tempDir.length()-1) != '/') 
    	    tempDir += '/';
#endif
		std::cout << "temporary directory used is " << tempDir << std::endl;
		static bool bUseTempDir = false;
		static bool bFirstTimeUpdateLog = true;

		if (bFirstTimeUpdateLog) {
			if (stat(tempDir.c_str(), &buf) == 0) {
				// use local temp directory for .sim files
				// to avoid network traffic
				if (buf.st_mode & S_IFDIR) {
					bUseTempDir = true;
				}
			}
			bFirstTimeUpdateLog = false;
		}

		// write sim files to local
		if (bSimZip && bUseTempDir) {
			sprintf(simFileName,"%s%s%.4d%s", tempDir.c_str(), getFileName(baseSimName).c_str(), simFileCount, SIM_FILE_EXT);
		} else {
			sprintf(simFileName,"%s%.4d%s",baseFileName, simFileCount, SIM_FILE_EXT);
		}
		std::cout << "sim file name is " << simFileName << std::endl;
		sprintf(particleFileName,"%s%s",simFileName, PARTICLE_FILE_EXT);

		simulation->writeData(simFileName,bSimFileCompress);

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
				if (stat(particleFileName, &buf) == 0) {	// has particle
				//	retcode = zip32(2, zipFileName, simFileName, particleFileName);
					addFilesToZip(zipFileName, simFileName, particleFileName);
					remove(particleFileName);
				} else {
					bSuccess = zipUnzipWithRetry(true, zipFileName, simFileName, errmsg);
				}
				remove(simFileName);

				// write the log file
				if (bSuccess) {
					// write hdf5 post processing before writing log entry
					if (postProcessingHdf5Writer != NULL) {
						postProcessingHdf5Writer->writeOutput();
					}

					char zipFileNameWithoutPath[512];
					sprintf(zipFileNameWithoutPath,"%s%.2d%s",baseSimName, zipFileCount, ZIP_FILE_EXT);
					char simFileNameWithoutPath[512];
					sprintf(simFileNameWithoutPath,"%s%.4d%s", baseSimName, simFileCount, SIM_FILE_EXT);
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
			simFileCount++;
		} else {
			throw errmsg;
		}
	} else{
		// write hdf5 post processing before writing log entry
		if (postProcessingHdf5Writer != NULL) {
			postProcessingHdf5Writer->writeOutput();
		}

	}

	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_DATA, progress, time));
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

	if (SimTool::getInstance()->isSundialsPdeSolver()) {
		simulation->setSimStartTime(0);
	}

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
			numTokens =  fscanf(fp,"%d %s %s %lg\n", &iteration, simFileName, zipFileName, &time);
		} else {
			numTokens =  fscanf(fp,"%d %s %lg\n", &iteration, simFileName, &time);
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

bool SimTool::isSundialsPdeSolver() {
	return solver == SUNDIALS_PDE_SOLVER;
}

bool SimTool::isVCellPetscSolver() {
	return solver == VCELL_PETSC_SOLVER;
}

void SimTool::setSolver(string& s) {
	if (s.length() > 0 && s != FV_SOLVER && s != SUNDIALS_PDE_SOLVER && s != VCELL_PETSC_SOLVER) {
		stringstream ss;
		ss << "unknown solver : " << s;
		throw ss.str();
	}
	solver = s;
}

void SimTool::start() {
	if (simulation->getNumVariables() == 0) {
		return;
	}
	simulation->resolveReferences();
    if (numSerialParameterScans == 0 ) {
        // Is Not paramScan, the .fvinput had no SERIAL_SCAN_PARAMETER_... section
        start1();
    } else if (SimulationMessaging::getInstVar()->getTaskID() >= 0) {
        // Is paramScan, the .fvinput had 'SERIAL_SCAN_PARAMETER_...' section
        // Is also a remote server (green-button) run , the .fvinput has 'JMS_PARAM_BEGIN' section
        // Each paramScan is run as a separate job with serial jobIndex in the 'JMS_PARAM_BEGIN' section
        SimulationExpression* sim = (SimulationExpression*)simulation;
        sim->setParameterValues(serialScanParameterValues[SimulationMessaging::getInstVar()->getJobIndex()]);
		start1();
	} else {
        // Is paramScan, the .fvinput had SERIAL_SCAN_PARAMETER_... section
        // Is also a local (blue-button) run where each paramScan is run in the following loop
		SimulationExpression* sim = (SimulationExpression*)simulation;
		for (int scan = 0; scan < numSerialParameterScans; scan ++) {
			if (scan > 0) {
				string bfn(baseFileName);
				char oldIndex[10], newIndex[10];
				sprintf(oldIndex, "_%d_\0", scan - 1);
				sprintf(newIndex, "_%d_\0", scan);
				int p = (int)bfn.rfind(oldIndex);
				bfn.replace(p, strlen(oldIndex), newIndex);
				setBaseFilename((char*)bfn.c_str());
			}
			sim->setParameterValues(serialScanParameterValues[scan]);
			start1();
		}
	}
}

void SimTool::setSmoldynInputFile(string& inputfile) {
	smoldynInputFile = inputfile;
}

void SimTool::start1() {

	// create post processing hdf5 writer
	if (simulation->getPostProcessingBlock() != NULL) {
		if (postProcessingHdf5Writer == NULL) {
			char h5PPFileName[128];
			sprintf(h5PPFileName, "%s%s", baseFileName, HDF5_FILE_EXT);
			postProcessingHdf5Writer = new PostProcessingHdf5Writer(h5PPFileName, simulation->getPostProcessingBlock());
		}
	}

	simulation->initSimulation();

	if (smoldynInputFile != "") {
		smoldynSim = vcellhybrid::smoldynInit(this, smoldynInputFile);//smoldynInit will write output therefore computeHistogram is called by VCellSmoldynOutput.write().
		copyParticleCountsToConcentration();
		// since smoldyn only initializes variable current value,
		// we need to copy current to old.
		for (int i = 0; i < (int)simulation->getNumVariables(); i ++) {
			Variable* var = simulation->getVariable(i);
			var->update();
		}
	}

	loadFinal();   // initializes to the latest file if it exists

	if (checkStopRequested()) {
		return;
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

	if (simulation->getCurrIteration()==0) {
		// simulation starts from scratch, needs to
		// write .mesh and .meshmetrics file
		if (bStoreEnable){
			FILE *fp = NULL;
			char filename[128];
			sprintf(filename, "%s%s", baseFileName, MESH_FILE_EXT);
			if ((fp=fopen(filename,"w"))==NULL){
				char errMsg[256];
				sprintf(errMsg, "cannot open mesh file %s for writing", filename);
				throw errMsg;
			}
			simulation->getMesh()->write(fp);
			fclose(fp);

			sprintf(filename, "%s%s", baseFileName, MESHMETRICS_FILE_EXT);
			if ((fp=fopen(filename,"w"))==NULL){
				char errMsg[256];
				sprintf(errMsg, "cannot open mesh metrics file %s for writing", filename);
				throw errMsg;
			}
			simulation->getMesh()->writeMeshMetrics(fp);
			fclose(fp);
		}
		updateLog(0.0, 0.0, 0);
	} else {
		// simulation continues from existing results, send data message
		SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_DATA, percentile, simStartTime));
	}

    //
    // iterate up to but not including end time
    //
	if (bCheckSpatiallyUniform) {
		increment = 0.001;
	}
	double epsilon = 1e-12;
	clock_t oldTime = clock(); // to control the output of progress, send progress every 2 seconds.
	int counter = 0;
	while (true) {
		if (simulation->getTime_sec() - simEndTime > epsilon // currentTime past endTime
				|| fabs(simEndTime - simulation->getTime_sec()) < epsilon) { // reached the end time
			break;
		}
		if (isSundialsPdeSolver() && bSundialsOneStepOutput) {
			if (simulation->getCurrIteration() / keepEvery > keepAtMost){ // reached keep at most
				break;
			}
		}

		if (checkStopRequested()) {
			return;
		}

		counter++;
		simulation->iterate();
		if (smoldynSim != NULL && counter == smoldynStepMultiplier) {
			vcellhybrid::smoldynOneStep(smoldynSim);//smoldynOneStep includes computeHistogram after each time step.
			copyParticleCountsToConcentration();
			counter = 0;
		}

		if (checkStopRequested()) {
			return;
		}

		simulation->update();

		if (simulation->getCurrIteration() % keepEvery == 0 || fabs(simEndTime - simulation->getTime_sec()) < epsilon) {
			updateLog(percentile,simulation->getTime_sec(), simulation->getCurrIteration());
        }

		clock_t currentTime = clock();
		double duration = (double)(currentTime - oldTime) / CLOCKS_PER_SEC;
		if (duration >= 2){
			percentile = (simulation->getTime_sec() - simStartTime)/(simEndTime - simStartTime);
			if (percentile - lastSentPercentile >= increment) {
				std::cout << "SimTool.start1() sending JOB_PROGRESS to SimulationMessaging: percentile=" << percentile << ", time=" << simulation->getTime_sec() << std::endl;
				SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_PROGRESS, percentile, simulation->getTime_sec()));
				lastSentPercentile = percentile;
				oldTime = currentTime;
			}
		}

		if (bCheckSpatiallyUniform) {
			bool uniform = true;
			for (int i = 0; i < simulation->getNumVariables(); i ++) {
				if (!checkSpatiallyUniform(simulation->getVariable(i))) {
					uniform = false;
					break;
				}
			}
			if (uniform) {
				cout << endl << "!!!Spatially Uniform reached at " << simulation->getTime_sec() << endl;
				if (simulation->getCurrIteration() % keepEvery != 0) {
					updateLog(percentile,simulation->getTime_sec(), simulation->getCurrIteration());
				}
				break;
			}
		}
	}

	if (smoldynSim != NULL) {
		vcellhybrid::smoldynEnd(smoldynSim);
	}
	if (checkStopRequested()) {
		return;
	}

	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_PROGRESS, 1.0, simulation->getTime_sec()));
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_COMPLETED, percentile, simulation->getTime_sec()));

	showSummary(stdout);
}

void SimTool::copyParticleCountsToConcentration(){ //concentration in terms of particleCounts/mesh element size
	//translating the particle counts into concentration (molecules/mesh size) for each mesh element
	int numVars = simulation->getNumVariables();
	for (int i=0; i<numVars; i++){
		if(simulation->getVariable(i)->getVarType() == VAR_VOLUME_PARTICLE){
			VolumeParticleVariable* var = (VolumeParticleVariable*)simulation->getVariable(i);
			for(int j=0; j<simulation->getMesh()->getNumVolumeElements(); j++){
				double count = var->getMoleculeCounts()[j];
				if(count>0){
					var->getCurr()[j]=count/(simulation->getMesh()->getVolumeOfElement_cu(j));
				}else {
					var->getCurr()[j]=0;
				}
			}
		}
		else if(simulation->getVariable(i)->getVarType() == VAR_MEMBRANE_PARTICLE){
			MembraneParticleVariable* var = (MembraneParticleVariable*)simulation->getVariable(i);
			for(int j=0; j<simulation->getMesh()->getNumMembraneElements(); j++){
				double count = var->getMoleculeCounts()[j];
				if(count>0){
					var->getCurr()[j]=count/(simulation->getMesh()->getMembraneElements()[j].area);
				}else {
					var->getCurr()[j]=0;
				}
			}
		}
	}
}


bool SimTool::checkSpatiallyUniform(Variable* var) {
	double* currSol = var->getCurr();
	CartesianMesh* mesh = (CartesianMesh*)simulation->getMesh();
	switch (var->getVarType()) {
		case VAR_VOLUME:
			for (int r = 0; r < mesh->getNumVolumeRegions(); r ++) {
				VolumeRegion* volRegion = mesh->getVolumeRegion(r);
				int numElements = volRegion->getNumElements();
				double minSol = DBL_MAX;
				double maxSol = -DBL_MAX;
				for(int j = 0; j < numElements; j ++){
					int volIndex = volRegion->getElementIndex(j);
					maxSol = max<double>(maxSol, currSol[volIndex]);
					minSol = min<double>(minSol, currSol[volIndex]);
				}
				double den = max<double>(fabs(maxSol), fabs(minSol));
				if (den >= spatiallyUniformAbsTol && (maxSol - minSol)/den > spatiallyUniformRelTol) {
					return false;
				}
			}
			return true;
		case VAR_MEMBRANE:
			for (int r = 0; r < mesh->getNumMembraneRegions(); r ++) {
				MembraneRegion* memRegion = mesh->getMembraneRegion(r);
				int numElements = memRegion->getNumElements();
				double minSol = DBL_MAX;
				double maxSol = -DBL_MAX;
				for(int j = 0; j < numElements; j ++){
					int memIndex = memRegion->getElementIndex(j);
					maxSol = max<double>(maxSol, currSol[memIndex]);
					minSol = min<double>(minSol, currSol[memIndex]);
				}
				double den = max<double>(fabs(maxSol), fabs(minSol));
				if (den >= spatiallyUniformAbsTol && (maxSol - minSol)/den > spatiallyUniformRelTol) {
					return false;
				}
			}
			return true;
		case VAR_VOLUME_REGION:
			return true;
		case VAR_MEMBRANE_REGION:
			return true;
		default:
			return true;
	}
}

void SimTool::setCheckSpatiallyUniform() {
	bCheckSpatiallyUniform = true;
	cout << endl << "----Stop At Spatially Uniform----" << endl;
}

void SimTool::setSerialParameterScans(int numScans, double** values) {
	numSerialParameterScans = numScans;
	serialScanParameterValues = values;
	cout << endl << "----Serial Parameter Scans : " << numSerialParameterScans << endl;
}
