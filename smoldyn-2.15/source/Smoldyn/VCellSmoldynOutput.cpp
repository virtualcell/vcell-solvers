/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include "VCellSmoldynOutput.h"

#include <VCELL/SimulationMessaging.h>
#include "DataProcessorRoiTimeSeriesSmoldyn.h"

#define SIM_FILE_EXT "sim"
#define LOG_FILE_EXT "log"
#define ZIP_FILE_EXT "zip"
#define DATAPROCOUTPUT_EXT "dataProcOutput"
#define ZIP_FILE_LIMIT 1E9

typedef int int32;
typedef unsigned int uint32;

int zip32(int filecnt, char* zipfile, ...);
#include <sys/stat.h>
#include <math.h>

#include <sstream>
using namespace std;

static int getZipCount(char* zipFileName) {
	char* p = strstr(zipFileName, ZIP_FILE_EXT);
	if (p == NULL) {
		return -1;
	}

	char str[3];
	strncpy(str, p - 2, 2 * sizeof(char));
	str[2] = 0;
	return atoi(str);
}

VCellSmoldynOutput::VCellSmoldynOutput(simptr sim){
	smoldynSim = sim;

	simFileCount = 0;
	zipFileCount = 0;
	Nx = Ny = Nz = 1;
	dimension = 0;
	volRegionSize = 0;
	smoldynDataProcessor = 0;

	varSize = numVars = numBlocks = 0;
	dataBlock = 0;
	outputData = 0;
	totalCounts = 0;
	outputDataSize = 0;
}

VCellSmoldynOutput::~VCellSmoldynOutput() {
	for (int i = 0; i < numVars*2; i ++) {
		delete[] varNames[i];
	}
	delete[] varNames;
	delete[] outputData;
	delete[] totalCounts;
	delete[] dataBlock;

	delete smoldynDataProcessor;
}

void VCellSmoldynOutput::parseDataProcessingInput(string& name, string& input) {
	if (name == "RoiTimeSeries") {
		smoldynDataProcessor = new DataProcessorRoiTimeSeriesSmoldyn(this, name, input);
	} else {
		throw "unknown DataProcessor";
	}
}

void VCellSmoldynOutput::parseInput(char* input) {
	if (dimension > 0) {
		return;
	}

	char* rootdir = smoldynSim->cmds->root;
	char* fname = smoldynSim->cmds->fname[0];
	strcpy(baseFileName, rootdir);
	strcat(baseFileName, fname);
	char* p = strrchr(baseFileName, '.');
	*p = '\0';

	strcpy(baseSimName, fname);
	p = strrchr(baseSimName, '.');
	*p = '\0';

	clearLog();

	if (input == NULL || strlen(input) == 0) {
		throw "writeOutput : no dimension specified.";
	}
	dimension = sscanf(input, "%d %d %d %d", &Nx, &Ny, &Nz, &volRegionSize);
	dimension --;
	if (dimension == 0) {
		char errMsg[256];
		sprintf(errMsg, "writeOutput : no dimension specified. %d %d %d", Nx, Ny, Nz);
		throw errMsg;
	}
	if (dimension == 1) {
		volRegionSize = Ny;
		Ny = 1;
	} else if (dimension == 2) {
		volRegionSize = Nz;
		Nz = 1;
	}

	origin[0] = smoldynSim->wlist[0]->pos;
	extent[0] = smoldynSim->wlist[1]->pos - origin[0];
	if (dimension > 1) {
		origin[1] = smoldynSim->wlist[2]->pos;
		extent[1] = smoldynSim->wlist[3]->pos - origin[1];
		if (dimension > 2) {
			origin[2] = smoldynSim->wlist[4]->pos;
			extent[2] = smoldynSim->wlist[5]->pos - origin[2];
		}
	}

	molssptr mols = smoldynSim->mols;
	numVars = mols->nspecies - 1;
	varNames = new char*[numVars * 2];
	for (int i = 0; i < numVars; i ++) {
		varNames[i] = new char[128];
		strcpy(varNames[i], mols->spname[i + 1]);
	}
	for (int i = 0; i < numVars; i++) {
		varNames[i + numVars] = new char[128];
		sprintf(varNames[i + numVars], "%s_totalCount\0", varNames[i]);
	}
	strcpy(fileHeader.magicString, MAGIC_STRING);
	strcpy(fileHeader.versionString, VERSION_STRING);
	varSize = Nx * Ny * Nz;
	numBlocks = numVars * 2;

	fileHeader.sizeX = Nx;
	fileHeader.sizeY = Ny;
	fileHeader.sizeZ = Nz;
	fileHeader.numBlocks = numBlocks;
	fileHeader.firstBlockOffset = sizeof(FileHeader);

	dataBlock = new DataBlock[numBlocks];
	outputDataSize = numVars * (varSize + volRegionSize);
	outputData = new double[outputDataSize];
	totalCounts = new int[numVars];
}

void VCellSmoldynOutput::write() {	
	computeOutputData();
	if (smoldynDataProcessor == 0 || smoldynDataProcessor->isStoreEnabled()) {
		// write sim file
		char simFileName[256];
		char zipFileName[256];
		sprintf(simFileName, "%s%.4d.%s", baseSimName, simFileCount, SIM_FILE_EXT);
		sprintf(zipFileName, "%s%.2d.%s", baseFileName, zipFileCount, ZIP_FILE_EXT);

		writeSim(simFileName, zipFileName);	

		// write log file
		char logfilename[256];
		sprintf(logfilename, "%s.%s", baseFileName, LOG_FILE_EXT);
		FILE* logfp = NULL;
		if (simFileCount == 0) {
			logfp = fopen(logfilename, "w");
		} else {
			logfp = fopen(logfilename, "a");
		}
		if (logfp == NULL) {
			throw "can't open logfile for write";
		}
		int iteration = (int)(smoldynSim->time/smoldynSim->dt + 0.5);
		fprintf(logfp,"%4d %s %s %.15lg\n", iteration, simFileName, zipFileName, smoldynSim->time);
		fclose(logfp);

		// print message
		simFileCount ++;

		struct stat buf;
		if (stat(zipFileName, &buf) == 0) { // if exists
			if (buf.st_size > ZIP_FILE_LIMIT) {
				zipFileCount ++;
			}
		}
	}
	if (smoldynDataProcessor != 0) {
		if (smoldynSim->time == 0) {
			smoldynDataProcessor->onStart();
		}
		smoldynDataProcessor->onWrite();
		if (fabs(smoldynSim->time + smoldynSim->dt - smoldynSim->tmax) > 1e-12) {
			char fileName[256];
			sprintf(fileName, "%s.%s", baseFileName, DATAPROCOUTPUT_EXT);
			smoldynDataProcessor->onComplete(fileName);
		}
	}

	double progress = (smoldynSim->time - smoldynSim->tmin) / (smoldynSim->tmax - smoldynSim->tmin);
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_DATA, progress, smoldynSim->time));
}

void VCellSmoldynOutput::clearLog() {

	FILE *fp;
	char logFileName[256];

	sprintf(logFileName,"%s.%s",baseFileName, LOG_FILE_EXT);
	if ((fp=fopen(logFileName, "r"))==NULL){
		printf("error opening log file <%s>\n", logFileName);
		return;
	}

	char simFileName[128];
	char zipFileName[128];
	int iteration, oldCount=-1, count;
	double time;

	while (!feof(fp)) {
		fscanf(fp,"%4d %s %s %lg\n", &iteration, simFileName, zipFileName, &time);
		count = getZipCount(zipFileName);
		if (oldCount != count && count >= 0) {
			printf("clearLog(), removing zip file %s\n", zipFileName);
			remove(zipFileName);
			oldCount = count;
		}
	}
	fclose(fp);

	printf("clearLog(), removing log file %s\n", logFileName);
	remove(logFileName);

	char dataProcOutput[128];
	sprintf(dataProcOutput,"%s.%s",baseFileName, DATAPROCOUTPUT_EXT);
	remove(dataProcOutput);
}

void VCellSmoldynOutput::computeOutputData() {
	molssptr mols = smoldynSim->mols;

	memset(outputData, 0, outputDataSize * sizeof(double));
	memset(totalCounts, 0, numVars * sizeof(int));

	for(int ll=0;ll<mols->nlist;ll++) {
		for(int m=0;m<mols->nl[ll];m++) {
			moleculeptr mptr=mols->live[ll][m];
			int varIndex = mptr->ident - 1;
			if (varIndex >= 0 ) {
				double* coord = mptr->pos;
				int i = 0, j = 0, k = 0;
				i = (int)((coord[0] - origin[0]) * (Nx - 1)/extent[0] + 0.5);
				if (dimension > 1) {
					j = (int)((coord[1] - origin[1]) * (Ny - 1)/extent[1] + 0.5);
					if (dimension > 2) {
						k = (int)((coord[2] - origin[2]) * (Nz - 1)/extent[2] + 0.5);
					}
				}

				int volIndex = k * Nx * Ny + j * Nx + i;
				outputData[varIndex * varSize + volIndex] ++;

				totalCounts[varIndex] ++;
			}
		}
	}

	int totalCountOffSet = numVars * varSize;
	// for total count region variable
	for (int v = 0; v < numVars; v ++) {
		for (int r = 0; r < volRegionSize; r ++) {
			outputData[totalCountOffSet + v * volRegionSize + r] = totalCounts[v];
		}
	}	
}

void VCellSmoldynOutput::writeSim(char* simFileName, char* zipFileName) {

	FILE* simfp = fopen(simFileName, "wb");
	if (simfp == NULL){
		throw "Cannot open .sim file to write";
	}

	DataSet::writeHeader(simfp, &fileHeader);
	long ftell_pos = ftell(simfp);
	if (ftell_pos != fileHeader.firstBlockOffset){
		char errMsg[256];
		sprintf(errMsg, "DataSet::write() - file offset for first block is incorrect, ftell() says %ld, should be %d", ftell_pos, fileHeader.firstBlockOffset);
		throw errMsg;
	}

	//
	// write data blocks (describing data)
	//
	int blockIndex = 0;
	int dataOffset = fileHeader.firstBlockOffset + numBlocks * sizeof(DataBlock);
	int v;
	for (v = 0; v < numVars; v ++) {
		memset(dataBlock[blockIndex].varName, 0, DATABLOCK_STRING_SIZE * sizeof(char));
		strcpy(dataBlock[blockIndex].varName, varNames[v]);

		dataBlock[blockIndex].varType = VAR_VOLUME;
		dataBlock[blockIndex].size = varSize;
		dataBlock[blockIndex].dataOffset = dataOffset;
		DataSet::writeDataBlock(simfp, dataBlock + blockIndex);
		dataOffset += dataBlock[blockIndex].size * sizeof(double);
		blockIndex ++;
	}
	for (v = numVars; v < numVars * 2; v ++) {
		memset(dataBlock[blockIndex].varName, 0, DATABLOCK_STRING_SIZE * sizeof(char));
		strcpy(dataBlock[blockIndex].varName, varNames[v]);

		dataBlock[blockIndex].varType = VAR_VOLUME_REGION;
		dataBlock[blockIndex].size = volRegionSize;
		dataBlock[blockIndex].dataOffset = dataOffset;
		DataSet::writeDataBlock(simfp, dataBlock + blockIndex);
		dataOffset += dataBlock[blockIndex].size * sizeof(double);
		blockIndex ++;
	}

	//
	// write data
	//
	int totalCountOffSet = numVars * varSize;
	blockIndex = 0;
	for (v = 0; v < numVars; v ++) {
		ftell_pos = ftell(simfp);
		if (ftell_pos != dataBlock[blockIndex].dataOffset){
			char errMsg[256];
			sprintf(errMsg, "DataSet::write() - offset for data is "
				"incorrect (block %d, var=%s), ftell() says %ld, should be %d", blockIndex, dataBlock[blockIndex].varName,
				ftell_pos, dataBlock[blockIndex].dataOffset);
			throw errMsg;
		}
		DataSet::writeDoubles(simfp, outputData + v * varSize, varSize);
		blockIndex ++;
	}
	for (v = 0; v < numVars; v ++) {
		ftell_pos = ftell(simfp);
		if (ftell_pos != dataBlock[blockIndex].dataOffset){
			char errMsg[256];
			sprintf(errMsg, "DataSet::write() - offset for data is "
				"incorrect (block %d, var=%s), ftell() says %ld, should be %d", blockIndex, dataBlock[blockIndex].varName,
				ftell_pos, dataBlock[blockIndex].dataOffset);
			throw errMsg;
		}
		DataSet::writeDoubles(simfp, outputData + totalCountOffSet + v * volRegionSize, volRegionSize);
		blockIndex ++;
	}
	fclose(simfp);

	int retcode = zip32(1, zipFileName, simFileName);
	remove(simFileName);
	if (retcode != 0) {
		char errMsg[256];
		sprintf(errMsg, "Writing zip file <%s> failed, return code is %d", zipFileName, retcode);
		throw errMsg;
	}
}
