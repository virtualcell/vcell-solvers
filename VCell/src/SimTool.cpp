/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifdef WIN32
#include <Windows.h>
#else
#include <UnixDefs.h>
#endif

#ifdef VCELL_MPI
#include "mpi.h"
#endif
///////////////////////////////////////////////
// SimTool.C
//////////////////////////////////////////////
#include <string.h>
//#include <libgen.h>
#include <stdio.h>
#include <math.h>
#include <VCELL/SimTypes.h>
#include <VCELL/App.h>
#include <VCELL/SimTool.h>
#include <VCELL/Mesh.h>
#include <VCELL/VCellModel.h>
#include <VCELL/Element.h>
#include <VCELL/Simulation.h>
#include <VCELL/DataSet.h>

#include <sys/types.h>
#include <sys/stat.h>
#define ZIP_FILE_LIMIT 1E9
#define NUM_TOKENS_PER_LINE 4

int zip32(int filecnt, char* zipfile, ...);
int unzip32(char* zipfile, char* file, char* exdir);

bool SimTool::bStopSimulation = false;

//--------------------------------------------------------------
// SimTool Class
//--------------------------------------------------------------
SimTool::SimTool(char *name)
{
	_sim = NULL;
	_endTimeSec = 1.0;
	_iteration = 0;
	_storeMultiple = 100;
	_storeEnable = FALSE;
	_displayVarName = "calcium";
	_baseFName="experiment_";
	_FNameCount=0;
	_ZNameCount = 0;
	_bFileCompress = TRUE;
	prefixDName = NULL;
	prefixFName = NULL;
}

SimTool::~SimTool()
{
}

void SimTool::setBaseFilename(char *fname) { 
	_baseFName = fname; 

	if (_baseFName == NULL || strlen(_baseFName) == 0) {
		return;
	}
	// get extract directory
	prefixDName = new char[strlen(_baseFName) + 1];
	prefixFName = NULL;

	strcpy(prefixDName, _baseFName);
	char* p = strrchr(prefixDName, '\\');
	if (p == NULL) {
		p = strrchr(prefixDName, '/');
	}
	if (p == NULL) {
		prefixFName = prefixDName; 
		prefixDName = NULL;
	} else {
		prefixFName = new char[strlen(p+1) + 1];
		strcpy(prefixFName, p + 1);
		*(p + 1)= 0;
	}
}

void SimTool::setup()
{
	_sim = theApplication->getSimulation();
	ASSERTION(_sim);
}

void SimTool::initSimulation()
{
	setup();
	_sim->initSimulation();
}

boolean SimTool::okToQuit()
{
	stop();
	return TRUE;
}

void SimTool::loadFinal()
{

	if (_sim==NULL){
		printf("SimTool.loadFinal(), sim=NULL just returning\n");
		return;
	}
      
	//
	// read '.log' file to determine simulation time and iteration
	//
	double parsedTime = -1.0;
	int tempIteration = -1;
	int tempFileCount = 0;
	int tempZipCount = 0; 

	FILE *fp=NULL;

	char logFileName[128];
	char zipFileName[128];
	char dataFileName[128];

	sprintf(logFileName,"%s.log", (char*)_baseFName);

	if ((fp=fopen(logFileName, "r"))==NULL){
		printf("SimTool::loadFinal(), unable to open log file %s\n",logFileName);
		clearLog(_baseFName);
		return;
	}
	while (!feof(fp)){
		char logBuffer[201];
		if (!fgets(logBuffer, 200, fp)){
			break;
		}
		//
		// look for line with a valid filename (includes basename)
		//
		if (strstr(logBuffer, prefixFName)){
			//
			// parse iteration number and time
			//		  
			if (sscanf(logBuffer, "%d %s %s %lg", &tempIteration, dataFileName, zipFileName, &parsedTime) != NUM_TOKENS_PER_LINE){
				printf("SimTool::load(), error reading log file %s, reading iteration\n", (char *)logFileName);
				printf("error in line %d = '%s'\n",tempFileCount,logBuffer);
				fclose(fp);
				break;  
			}
		}
		tempFileCount++;
	}
	fclose(fp);

	if (tempIteration == 0) {
		clearLog(_baseFName);
		return;
	}

	if (parsedTime>-1 && tempIteration>-1){
	   // if the zip file exists		
		struct stat buf;
		if (stat(zipFileName, &buf) != 0) {
			printf("SimTool::loadFinal(), unable to open zip file %s\n",(char *)(_baseFName+".zip"));
			clearLog(_baseFName);
			return;
		}

		DataSet dataSet;
		int retcode = 0;
		// unzip the file (without directory) into exdir, currently we 
		// unzip the file to the current working directory
		try {
			retcode = unzip32(zipFileName, dataFileName, NULL);
		} catch (...) {
			retcode = -1; // when exception occurs, clear all the data files and start over without exiting.
		}
		if (retcode == 0) {
			if (!dataSet.read(dataFileName,_sim)){
				printf("dataSet.read(%s) returned false in SimTool.loadFile()\n",dataFileName);
				clearLog(_baseFName);
 				return;
			} else{
				_ZNameCount = getZipCount(zipFileName);
				// wrong zip file Name
				if (_ZNameCount == -1) {
					clearLog(_baseFName);
					return;
				}
				_sim->setTime_sec(parsedTime);
				_iteration = tempIteration;
				_FNameCount = tempFileCount;
				remove(dataFileName);

				struct stat buf;
				if (stat(zipFileName, &buf) == 0) {
					if (buf.st_size > ZIP_FILE_LIMIT) {
						_ZNameCount ++;
					}
				}				
			}
		} else {			
			clearLog(_baseFName);
			return;			
		}
   }else{
	   printf("no files found with base filename %s\n",_baseFName);
   }
}

void SimTool::updateLog(char *baseFName, double progress, double time, int iteration)
{
	FILE *fp;
	char simFileName[128];
	char logFileName[128];
	char zipFileName[128];
	char particleFileName[128];

	// write sim files to local
	sprintf(simFileName,"%s%.4d.sim",prefixFName,_FNameCount);
	sprintf(particleFileName,"%s.particle",simFileName);
	assert(_sim);
	if (!_sim->writeData(simFileName,_bFileCompress)){
		printf("error writing dump to file %s\n", simFileName);
		return;
	}

	sprintf(logFileName,"%s.log",baseFName);
	if ((fp=fopen(logFileName, "a"))==NULL){
		printf("error opening log file <%s>\n",logFileName);
		return;
	}

   // write zip file first, then write log file, in case that 
   // zipping fails
	sprintf(zipFileName,"%s%.2d.zip",baseFName, _ZNameCount);
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

	//   if (_FNameCount==0) _sim->showStats(fp);
	//   fprintf(fp,"%4d>\tsim file=<%s>\ttime=%lg\n", iteration, simFileName, time);
	fprintf(fp,"%4d %s %s %lg\n", iteration, simFileName, zipFileName, time);
	fclose(fp);
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_DATA, progress, time));
   //printf("[[[data:%4d:%lg]]]", iteration, time);
   //fflush(stdout);	

	// if the zip file size is too big, then write another zip file
	if (stat(zipFileName, &buf) == 0) { // if exists
		if (buf.st_size > ZIP_FILE_LIMIT) {
			_ZNameCount ++;
		}
	}

	_FNameCount++;	
}

int SimTool::getZipCount(char* zipFileName) {
	char* p = strstr(zipFileName, ".zip");
	if (p == NULL) {
		return -1;
	}

	char str[3];
	strncpy(str, p - 2, 2 * sizeof(char));
	str[2] = 0;
	return atoi(str);
}

void SimTool::clearLog(char *baseFName)
{
	FILE *fp;
	char logFileName[256];

	char buffer[256];

	// remove mesh file
	sprintf(buffer,"%s.mesh",baseFName);
	remove(buffer);

	sprintf(buffer,"%s.meshmetrics",baseFName);
	remove(buffer);

	sprintf(buffer,"%s.zip",baseFName);
	remove(buffer);

	sprintf(buffer,"%s00.zip",baseFName);
	remove(buffer);

	sprintf(logFileName,"%s.log",baseFName);

	if ((fp=fopen(logFileName, "r"))==NULL){
		printf("error opening log file <%s>\n",logFileName);
		return;
	}

	char simFileName[128];
	char zipFileName[128];
	int iteration, oldCount=-1, count;
	double time;

	while (fscanf(fp,"%4d %s %s %lg\n", &iteration, simFileName, zipFileName, &time) == NUM_TOKENS_PER_LINE){
		char *dotSim = strstr(simFileName,".sim");
		assert(dotSim);
		*dotSim = '\0';
		sprintf(buffer,"%s.sim",simFileName);
		//printf("SimTool::clearLog(), removing file %s\n",buffer);
		remove(buffer);
		sprintf(buffer,"%s.sim.Z",simFileName);
		//printf("SimTool::clearLog(), removing file %s\n",buffer);
		remove(buffer);
		sprintf(buffer,"%s.sim.particle",simFileName);
		//printf("SimTool::clearLog(), removing file %s\n",buffer);
		remove(buffer);
		sprintf(buffer,"%s.sim.particle.Z",simFileName);
		//printf("SimTool::clearLog(), removing file %s\n",buffer);
		remove(buffer);
		count = getZipCount(zipFileName);
		if (oldCount != count && count != -1) {
			remove(zipFileName);
			oldCount = count;
		}
	}

	fclose(fp);
	printf("SimTool::clearLog(), removing file %s\n",logFileName);
	remove(logFileName);
}

void SimTool::start()
{
	if (bStopSimulation) {
		return;
	}

    _sim = theApplication->getSimulation();
    if (_sim==NULL) return;
    //
    // destroy any partial results from unfinished iterations
    //
    _sim->reset();
    double startTime=_sim->getTime_sec();
    //
    // store initial log if enabled
    //
	if (_storeEnable && _baseFName.length()>0){
		if (_iteration==0){
			ASSERTION(_sim->getTime_sec()==0.0);			
			FILE *fp = NULL;
			if ((fp=fopen(CString(_baseFName)+".mesh","w"))==NULL){
				printf("cannot open mesh file %s.mesh\n",_baseFName);
				exit(1);
			}
			if (!theApplication->getSimulation()->getMesh()->write(fp)){
				printf("error writing mesh file %s.mesh\n",_baseFName);
				exit(1);
			}
			fclose(fp);
			
			if ((fp=fopen(CString(_baseFName)+".meshmetrics","w"))==NULL){
				printf("cannot open mesh file %s.meshmetrics\n",_baseFName);
				exit(1);
			}
			if (!theApplication->getSimulation()->getMesh()->writeMeshMetrics(fp)){
				printf("error writing mesh file %s.meshmetrics\n",_baseFName);
				exit(1);
			}
			fclose(fp);
			updateLog(_baseFName, 0.0, 0.0, 0);
		}
	}
    double percentile=startTime/_endTimeSec;
    double increment =0.01;

    //
    // iterate up to but not including end time
    //

	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_STARTING, "simulation started ..."));

	// new simulation continues from existing results, send data message
	if (_iteration != 0) {
		SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_DATA, percentile, startTime));
	}
	//printf("simulation started ... ");
	//fflush(stdout);

//    TimerHandle tHndTotal = _sim->getTimerHandle("TOTAL TIMER");
//    _sim->startTimer(tHndTotal);
    double epsilon = 1E-8;
    while ((_sim->getTime_sec()+_sim->getDT_sec())<=(_endTimeSec+epsilon)){
		if (bStopSimulation) {
			return;
		}

       //if (_sim->iterate()){
		_sim->iterate();
		_iteration++;
		_sim->update();
        if (_storeEnable && _baseFName.length()>0){
            if (_iteration%_storeMultiple==0){				 
            updateLog(_baseFName,percentile,_sim->getTime_sec(), _iteration);
            }
        }
		while ((percentile+increment)*_endTimeSec < _sim->getTime_sec()){			  
        //while ((startTime + ((percentile+increment)*(_endTimeSec-startTime))) < _sim->getTime_sec()){			  
			if (bStopSimulation) {
				return;
			}
			  
            percentile+=increment;
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_PROGRESS, percentile, _sim->getTime_sec()));	
            //printf("[[[progress:%lg%%]]]", percentile*100.0);
            //fflush(stdout);			 
		}
		if (theApplication->checkForInterrupt()){
			printf("SimTool::start(),  application interrupted\n");
			return;
        }
		//}else{
		//   printf("SimTool::start(),  error in Simulation::iterate()\n");
		//   throw "SimTool::start(), error in Simulation::iterate()"; 
		// }
	}

	if (bStopSimulation) {
		return;
	}

//   _sim->stopTimer(tHndTotal);
#ifndef VCELL_MPI
	_sim->showSummary(stdout);
#else
   int rank;
   MPI_Comm_rank(MPI_COMM_WORLD, &rank);

   printf("SimTool::start(), synchronizing MPI worker %d ...\n",rank);
   _sim->synchronize();

   printf("SimTool::start(), status for MPI worker %d starting ...\n",rank);
   if (rank == 0){
      _sim->showSummary(stdout);
   }
   printf("SimTool::start(), status for MPI worker %d ending\n",rank);
#endif

	if (!bStopSimulation) {
   		SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_COMPLETED, percentile, _sim->getTime_sec()));
	}
}

void SimTool::startSteady(double tolerance, double maxTime)
{
	if (bStopSimulation) {
		return;
	}

    _sim = theApplication->getSimulation();
    if (_sim==NULL) return;
    //
    // destroy any partial results from unfinished iterations
    //
    _sim->reset();
    //
    // store initial log if enabled
    //
	if (_storeEnable && _baseFName.length()>0){
		if (_iteration==0){
			ASSERTION(_sim->getTime_sec()==0.0);
			clearLog(_baseFName);
			updateLog(_baseFName, 0.0, 0.0, 0);
		}
	}
    double percentile=0.00;
    double increment =0.10;
    double startTime=_sim->getTime_sec();
    //
    // iterate up to but not including end time
    //

	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_STARTING, "steady state solution started ..."));
	//printf("steady state solution started ... ");
	//fflush(stdout);

    double diff;
	while (_sim->getTime_sec()<maxTime){
		if (bStopSimulation) {
			return;
		}

       //if (_sim->iterate()){
		_sim->iterate();
		_iteration++;
		if (_storeEnable && _baseFName.length()>0){
			if (_iteration%_storeMultiple==0){
			updateLog(_baseFName,percentile,_sim->getTime_sec(), _iteration);
			}
		}
        while ((startTime + ((percentile+increment)*(_endTimeSec-startTime))) < _sim->getTime_sec()){
			if (bStopSimulation) {
				return;
			}
			percentile+=increment;
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_PROGRESS, percentile, _sim->getTime_sec()));	
			//printf("%lg%% ", percentile*100.0);
			//fflush(stdout);
        }

        if (theApplication->checkForInterrupt()){
            return;
        }
	    //}else{
		//   return;
		//}
		diff = _sim->getMaxDifference();
		if (diff < tolerance){
			printf("steady state reached, max absolute difference = %lg\n", diff);
			_sim->update();
			_sim->showSummary(stdout);
			if (_storeEnable && _baseFName.length()>0){
				updateLog(_baseFName,percentile,_sim->getTime_sec(), _iteration);
			}
			return;
		}
		printf("iteration(%7ld), diff=%lg\n",(long)_iteration,diff);
		_sim->update();
	}

	if (bStopSimulation) {
		return;
	}

	printf("MAX ITERATIONS EXCEEDED WITHOUT ACHIEVING STEADY STATE\n");
	_sim->showSummary(stdout);
	if (_storeEnable && _baseFName.length()>0){
		updateLog(_baseFName,percentile,_sim->getTime_sec(), _iteration);
	}

	if (!bStopSimulation) {
		SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_COMPLETED, percentile, _sim->getTime_sec()));
	}
}

void SimTool::stop()
{
//    if (_sim)
//       _sim->interrupt();
}

