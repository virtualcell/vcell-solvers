/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef DATASET_H
#define DATASET_H

#include <VCELL/SimTypes.h>

#define DATABLOCK_STRING_SIZE  124
#define MAGIC_STRING "VCell Data Dump"
#define VERSION_STRING  "2.0.1  "

struct FileHeader {
	char   magicString[16];
	char   versionString[8];
	int32  numBlocks;
	int32   firstBlockOffset;
	int32   sizeX;
	int32   sizeY;
	int32   sizeZ;
};

struct DataBlock {
	char   varName[DATABLOCK_STRING_SIZE];
	int32   varType;
	int32   size;
	int32   dataOffset;
};

void readHeader(FILE *fp, FileHeader *header);
void writeHeader(FILE *fp, FileHeader *header);
void readDataBlock(FILE *fp, DataBlock *block);
void writeDataBlock(FILE *fp, DataBlock *block);
void readDoubles(FILE *fp, double *data, int length);
void writeDoubles(FILE *fp, double *data, int length);

class Simulation;
class SimulationExpression;
class Variable;

class DataSet
{
public:
	static void read(char *filename, Simulation *sim);
	static void write(char *filename, SimulationExpression *sim, bool bCompress);
	static void convolve(Simulation* sim, Variable* var, double* values);

	static void readRandomVariables(char* filename, SimulationExpression* sim);
  
};

#endif
