/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifdef VCELL_MPI
#include "mpi.h"
#endif

#include <VCELL/SimTypes.h>
#include <VCELL/SimTool.h>
#include <VCELL/Mesh.h>
#include <VCELL/Simulation.h>
#include <VCELL/DataSet.h>
#include <VCELL/SimulationMessaging.h>
#include <VCELL/Solver.h>
#include <VCELL/Variable.h>
#include <VCELL/CartesianMesh.h>
#include <VCELL/VolumeRegion.h>
#include <VCELL/MembraneRegion.h>

#include <float.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <sstream>
using namespace std;

#define ZIP_FILE_LIMIT 1E9

#define MESH_FILE_EXT ".mesh"
#define MESHMETRICS_FILE_EXT ".meshmetrics"
#define SIM_FILE_EXT ".sim"
#define PARTICLE_FILE_EXT ".particle"
#define LOG_FILE_EXT ".log"
#define ZIP_FILE_EXT ".zip"

#define SUNDIALS_PDE_SOLVER "SUNDIALS_PDE_SOLVER"
#define FV_SOLVER "FV_SOLVER"

int zip32(int filecnt, char* zipfile, ...);
int unzip32(char* zipfile, char* file, char* exdir);

SimTool* SimTool::instance = 0;

static int NUM_TOKENS_PER_LINE = 4;

SimTool::SimTool()
{
	bSimZip = true;

	simEndTime = 0.0;
	bCheckSpatiallyUniform = false;
	keepEvery = 100;
	bStoreEnable = true;
	baseFileName=0;
	simFileCount=0;
	zipFileCount = 0;
	bSimFileCompress = false;
	baseDirName = NULL;
	baseSimName = NULL;

	_timer = 0;
	vcellModel = 0;
	simulation = 0;

	spatiallyUniformAbsTol = 1e-6;
	spatiallyUniformRelTol = 1e-3;

	numDiscontinuityTimes = 0;
	discontinuityTimes = 0;

	sundialsRelTol = 1e-7;
	sundialsAbsTol = 1e-9;

	solver = FV_SOLVER;

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

void SimTool::loadFinal()
{

	if (simulation == NULL){
		printf("SimTool.loadFinal(), sim=NULL just returning\n");
		return;
	}

	//
	// read '.log' file to determine simulation time and iteration
	//

	FILE *fp=NULL;

	char logFileName[128];
	char zipFileName[128];
	char dataFileName[128];

	sprintf(logFileName,"%s%s", baseFileName, LOG_FILE_EXT);

	if ((fp=fopen(logFileName, "r"))==NULL){
		printf("SimTool::loadFinal(), unable to open log file %s\n",logFileName);
		clearLog();
		return;
	}

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

	while (!feof(fp)){
		char logBuffer[201];
		if (!fgets(logBuffer, 200, fp)){
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
				fclose(fp);
				clearLog();
				return;
			}
		}
		tempFileCount++;
	}
	fclose(fp);

	if (tempIteration <= 0  || simStartTime <= 0) {
		clearLog();
		return;
	}

	if (isSundialsPdeSolver()) {
		simulation->setSimStartTime(simStartTime);
	}

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
			printf("SimTool::loadFinal(), unable to open zip file %s\n", zipFileAbsoluteName);
			clearLog();
			return;
		}
		try {
			// unzip the file (without directory) into exdir, currently we
			// unzip the file to the current working directory
			unzip32(zipFileAbsoluteName, dataFileName, NULL);
		} catch (...) {
			clearLog();
			return; // when exception occurs, clear all the data files and start over without exiting.
		}
	} else {
		// otherwise check if sim file exists
		if (stat(dataFileName, &buf)) {
			printf("SimTool::loadFinal(), unable to open zip file %s\n", zipFileName);
			clearLog();
			return;
		}
	}
	try {
		DataSet dataSet;
		dataSet.read(dataFileName, simulation);
		simulation->setCurrIteration(tempIteration);
		simFileCount = tempFileCount;

		if (bSimZip) {
			remove(dataFileName);
			zipFileCount = getZipCount(zipFileName);
			// wrong zip file Name
			if (zipFileCount < 0) {
				clearLog();
				return;
			}
			// check if this zip file is already big enough
			if (stat(zipFileName, &buf) == 0) {
				if (buf.st_size > ZIP_FILE_LIMIT) {
					zipFileCount ++;
				}
			}
		}
	} catch (const char* msg) {
		cout << "SimTool::loadFinal() : dataSet.read(" << dataFileName << " failed : " << msg << endl;
		clearLog();
	} catch (...) {
		cout << "SimTool::loadFinal() : dataSet.read(" << dataFileName << " failed : unexpected error" << endl;
		clearLog();
	}
}

void SimTool::updateLog(double progress, double time, int iteration)
{
	FILE *fp;
	char simFileName[128];
	char logFileName[128];
	char zipFileName[128];
	char particleFileName[128];

	// write sim files to local
	if (bSimZip) {
		sprintf(simFileName,"%s%.4d%s",baseSimName, simFileCount, SIM_FILE_EXT);
	} else {
		sprintf(simFileName,"%s%.4d%s",baseFileName, simFileCount, SIM_FILE_EXT);
	}
	sprintf(particleFileName,"%s%s",simFileName, PARTICLE_FILE_EXT);

	simulation->writeData(simFileName,bSimFileCompress);

	sprintf(logFileName,"%s%s",baseFileName, LOG_FILE_EXT);
	if ((fp=fopen(logFileName, "a"))==NULL){
		char errmsg[512];
		sprintf(errmsg, "SimTool::updateLog() - error opening log file <%s>", logFileName);
		throw errmsg;
	}

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
			retcode = zip32(1, zipFileName, simFileName);
		}
		remove(simFileName);

		if (retcode != 0) {
			char msg[256];
			sprintf(msg, "%s: %d", "Writing zip file failed, return code is ", retcode);
			throw  msg;
		}

		char zipFileNameWithoutPath[512];
		sprintf(zipFileNameWithoutPath,"%s%.2d%s",baseSimName, zipFileCount, ZIP_FILE_EXT);
		fprintf(fp,"%4d %s %s %.15lg\n", iteration, simFileName, zipFileNameWithoutPath, time);

		if (stat(zipFileName, &buf) == 0) { // if exists
			if (buf.st_size > ZIP_FILE_LIMIT) {
				zipFileCount ++;
			}
		}
	} else {
		char simFileNameWithoutPath[512];
		sprintf(simFileNameWithoutPath,"%s%.4d%s",baseSimName, simFileCount, SIM_FILE_EXT);
		fprintf(fp,"%4d %s %.15lg\n", iteration, simFileNameWithoutPath, time);
	}
	fclose(fp);
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_DATA, progress, time));

	simFileCount++;
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

bool SimTool::isSundialsPdeSolver() {
	return solver == SUNDIALS_PDE_SOLVER;
}

void SimTool::setSolver(string& s) {
	if (s.length() > 0 && s != FV_SOLVER && s != SUNDIALS_PDE_SOLVER) {
		stringstream ss;
		ss << "unknown solver : " << s;
		throw ss.str();
	}
	solver = s;
}

void SimTool::start() {
	if (checkStopRequested()) {
		return;
	}

	if (simulation == NULL) {
		throw "NULL simulation";
	}

	if (bStoreEnable && (baseFileName == NULL || strlen(baseFileName) == 0)) {
		throw "Invalid base file name for dataset";
	}

	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_STARTING, "simulation started"));

	//
    // destroy any partial results from unfinished iterations
    //
    double percentile = simStartTime/simEndTime;
    double increment = 0.01;
	double lastSentPercentile = percentile;
	//
    // store initial log if enabled
    //
	if (bStoreEnable) {
		if (simulation->getCurrIteration()==0) {
			// simulation starts from scratch
			ASSERTION(startTime == 0.0);
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

			updateLog(0.0, 0.0, 0);
		} else {
			// simulation continues from existing results, send data message
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_DATA, percentile, simStartTime));
		}
	}

    //
    // iterate up to but not including end time
    //
	if (bCheckSpatiallyUniform) {
		increment = 0.001;
	}
	double epsilon = 1e-12;

	while (true) {
		if (!isSundialsPdeSolver() || !bSundialsOneStepOutput) {
			if (simulation->getTime_sec() + simulation->getDT_sec() > simEndTime + epsilon) {
				break;
			}
		} else if (simulation->getTime_sec() > simEndTime - epsilon) {
			break;
		}

		if (isSundialsPdeSolver() && bSundialsOneStepOutput) {
			if (simulation->getCurrIteration() / keepEvery > keepAtMost){
				break;
			}
		}

		if (checkStopRequested()) {
			return;
		}

		simulation->iterate();
		simulation->update();

		if (checkStopRequested()) {
			return;
		}

        if (bStoreEnable){
			if (simulation->getCurrIteration() % keepEvery == 0 || simulation->getTime_sec() > simEndTime - epsilon){
				updateLog(percentile,simulation->getTime_sec(), simulation->getCurrIteration());
            }
        }
		percentile = (simulation->getTime_sec() - simStartTime)/(simEndTime - simStartTime);
		if (percentile - lastSentPercentile >= increment) {
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_PROGRESS, percentile, simulation->getTime_sec()));
			lastSentPercentile = percentile;
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
				 if (simulation->getCurrIteration() % keepEvery != 0){
					updateLog(percentile,simulation->getTime_sec(), simulation->getCurrIteration());
				}
				break;
			}
		}
	}

	if (checkStopRequested()) {
		return;
	} else {
		SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_COMPLETED, percentile, simulation->getTime_sec()));
	}

#ifndef VCELL_MPI
	showSummary(stdout);
#else
   int rank;
   MPI_Comm_rank(MPI_COMM_WORLD, &rank);

   printf("SimTool::start(), synchronizing MPI worker %d ...\n",rank);
   simulation->synchronize();

   printf("SimTool::start(), status for MPI worker %d starting ...\n",rank);
   if (rank == 0){
      showSummary(stdout);
   }
   printf("SimTool::start(), status for MPI worker %d ending\n",rank);
#endif
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
					int volIndex = volRegion->getIndex(j);
					maxSol = max(maxSol, currSol[volIndex]);
					minSol = min(minSol, currSol[volIndex]);
				}
				double den = max(abs(maxSol), abs(minSol));
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
					int memIndex = memRegion->getIndex(j);
					maxSol = max(maxSol, currSol[memIndex]);
					minSol = min(minSol, currSol[memIndex]);
				}
				double den = max(abs(maxSol), abs(minSol));
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
