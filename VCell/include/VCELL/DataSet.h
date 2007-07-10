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

bool readHeader(FILE *fp, FileHeader *header);
bool writeHeader(FILE *fp, FileHeader *header);
bool readDataBlock(FILE *fp, DataBlock *block);
bool writeDataBlock(FILE *fp, DataBlock *block);
bool readDoubles(FILE *fp, double *data, int length);
bool writeDoubles(FILE *fp, double *data, int length);

class Simulation;

class DataSet
{
public:
	DataSet();
	    
	bool read(char *filename, Simulation *sim);
	bool write(char *filename, Simulation *sim, bool bCompress);
  
};

#endif
