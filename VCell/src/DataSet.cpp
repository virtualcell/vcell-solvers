/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifdef WIN32
#define INTEL
#endif

#include <stdlib.h>

#include <VCELL/SimTypes.h>
#include <VCELL/Variable.h>
#include <VCELL/Simulation.h>
#include <VCELL/Mesh.h>
#include <VCELL/DataSet.h>
#include <VCELL/Element.h>
#include <VCELL/Region.h>
#include <VCELL/CartesianMesh.h>

//----------------------------------------------------------------------------
//
// class DataSet
//
//----------------------------------------------------------------------------
DataSet::DataSet() 
{
}

bool DataSet::read(char *filename, Simulation *sim)
{
	FILE *fp=NULL;
	FileHeader fileHeader;
	DataBlock *dataBlock;

	if ((fp=fopen(filename, "rb"))==NULL){
		cout << "DataSet::read() - could not open file '" << filename << "'" << endl;
		return false;
	}
	if (!readHeader(fp,&fileHeader)){
		cout << "DataSet::read() - could not read header from file '" << filename << "'" << endl;
		return false;
	}

	if (strcmp(fileHeader.magicString, MAGIC_STRING)){
		cout << "DataSet::read() - file is not a VCellDump file" << endl;
		return false;
	}

	if (fileHeader.numBlocks <= 0){
		cout << "DataSet::read() - number of blocks (" << fileHeader.numBlocks << ") less than 1" << endl;
		return false;
	}
	   
	dataBlock = new DataBlock[fileHeader.numBlocks];
	   
	if (fseek(fp, fileHeader.firstBlockOffset, SEEK_SET)){
		cout << "DataSet::read() - could not find first block at offset " << fileHeader.firstBlockOffset << endl;
		return false;
	}
	int i=0;
	for (i=0;i<fileHeader.numBlocks;i++){
		if (!readDataBlock(fp,dataBlock+i)){
			cout << "DataSet::read() - could not read dataBlock " << endl;
			return false;
		}
	}

	for (i=0;i<fileHeader.numBlocks;i++){
		Variable *var = sim->getVariableFromName(dataBlock[i].varName);
		if (var==NULL){
			cout << "DataSet::read() - variable '" << dataBlock[i].varName << "' not found in Simulation" << endl;
			continue;
		}
		if (var->getSize()!=dataBlock[i].size){
			cout << "DataSet::read() - size mismatch for var '" << dataBlock[i].varName << "', file=" << dataBlock[i].size << ", var=" << var->getSize() << endl;
			return false;
		}
	      
		if (fseek(fp, dataBlock[i].dataOffset, SEEK_SET)){
			cout << "DataSet::read() - could not find data offset (" << dataBlock[i].dataOffset <<")" << endl;
			return false;
		}
		if (!readDoubles(fp, var->getCurr(), var->getSize())){
			cout << "DataSet::read() - could not read data for var '" << var->getName() << "'" << endl;
			return false;
		}    
		var->update();   
		cout << "read data for variable '" << var->getName() << "'" << endl;
	}
	delete[] dataBlock;
	   
	fclose(fp);
	return true;
}

uint32 reverseLong(uint32 along)
{
	uint32 newLong = ((along>>24)&0x000000ff)|((along>>8)&0x0000ff00)|((along<<8)&0x00ff0000)|((along<<24)&0xff000000);
	return newLong;
}

unsigned char *reverseDouble(double adouble)
{
	static union {
		double dbl;
		unsigned char array[8];
	} longDoubleUnion;

	longDoubleUnion.dbl = adouble;

	for (int i=0;i<4;i++){
		unsigned char temp = longDoubleUnion.array[i];
		longDoubleUnion.array[i] = longDoubleUnion.array[7-i];
		longDoubleUnion.array[7-i] = temp;
	}
	return longDoubleUnion.array;
}

double reverseDouble(unsigned char array[8])
{
	static union {
		double dbl;
		unsigned char array[8];
	} longDoubleUnion;

	memcpy(longDoubleUnion.array,array,8);

	for (int i=0;i<4;i++){
		unsigned char temp = longDoubleUnion.array[i];
		longDoubleUnion.array[i] = longDoubleUnion.array[7-i];
		longDoubleUnion.array[7-i] = temp;
	}
	return longDoubleUnion.dbl;
}

//
// we must read and write in the unix style (endian ... can't remember whether big or small???)
//
//struct FileHeader {
//   char   magicString[16];
//   char   versionString[8];
//   long   numBlocks;
//   long   firstBlockOffset;
//   long   sizeX;
//   long   sizeY;
//   long   sizeZ;
//};
//
bool readHeader(FILE *fp, FileHeader *header)
{
#ifndef INTEL
	if (fread(header, sizeof(FileHeader), 1, fp)!=1){
		cout << "DataSet::readHeader() - could not read header (UNIX)" << endl;
		return false;
	}
#else
	if (fread(header->magicString, sizeof(char), 16, fp)!=16){
		cout << "DataSet::readHeader() - could not read header->magicString (INTEL)" << endl;
		return false;
	}
	if (fread(header->versionString, sizeof(char), 8, fp)!=8){
		cout << "DataSet::readHeader() - could not read header->versionString (INTEL)" << endl;
		return false;
	}
	uint32 newLongs[5];
	if (fread(&newLongs, sizeof(int32), 5, fp)!=5){
		cout << "DataSet::readHeader() - could not read header->offsets.. (INTEL)" << endl;
		return false;
	}
	header->numBlocks = reverseLong(newLongs[0]);
	header->firstBlockOffset = reverseLong(newLongs[1]);
	header->sizeX = reverseLong(newLongs[2]);
	header->sizeY = reverseLong(newLongs[3]);
	header->sizeZ = reverseLong(newLongs[4]);
#endif
	return true;
}

bool writeHeader(FILE *fp, FileHeader *header)
{
#ifndef INTEL
	if (fwrite(header, sizeof(FileHeader), 1, fp)!=1){
		cout << "DataSet::writeHeader() - could not write header (UNIX)" << endl;
		return false;
	}
#else
	if (fwrite(header->magicString, sizeof(char), 16, fp)!=16){
		cout << "DataSet::writeHeader() - could not write header->magicString (INTEL)" << endl;
		return false;
	}
	if (fwrite(header->versionString, sizeof(char), 8, fp)!=8){
		cout << "DataSet::writeHeader() - could not write header->versionString (INTEL)" << endl;
		return false;
	}
	uint32 newLongs[5];
	newLongs[0] = reverseLong(header->numBlocks);
	newLongs[1] = reverseLong(header->firstBlockOffset);
	newLongs[2] = reverseLong(header->sizeX);
	newLongs[3] = reverseLong(header->sizeY);
	newLongs[4] = reverseLong(header->sizeZ);
	if (fwrite(newLongs, sizeof(int32), 5, fp)!=5){
		cout << "DataSet::writeHeader() - could not write header->longs (INTEL)" << endl;
		return false;
	}
#endif
	return true;
}

//
// we must read and write in the unix style (endian ... can't remember whether big or small???)
//
//struct DataBlock {
//   char   varName[DATABLOCK_STRING_SIZE];
//   long   size;
//   long   dataOffset;
//};
//
bool readDataBlock(FILE *fp, DataBlock *block)
{
#ifndef INTEL
	if (fread(block, sizeof(DataBlock), 1, fp)!=1){
		cout << "DataSet::read() - could not read dataBlock (UNIX)" << endl;
		return false;
	}
#else
	if (fread(block->varName, sizeof(char), DATABLOCK_STRING_SIZE, fp)!=DATABLOCK_STRING_SIZE){
		cout << "DataSet::readDataBlock() - could not read block->varName (INTEL)" << endl;
		return false;
	}
	uint32 newLongs[3];
	if (fread(&newLongs, sizeof(int32), 3, fp)!=3){
		cout << "DataSet::read() - could not read dataBlock longs... (INTEL)" << endl;
		return false;
	}
	block->varType = reverseLong(newLongs[0]);
	block->size = reverseLong(newLongs[1]);
	block->dataOffset = reverseLong(newLongs[2]);
#endif
	return true;
}

bool writeDataBlock(FILE *fp, DataBlock *block)
{
#ifndef INTEL
	if (fwrite(block, sizeof(DataBlock), 1, fp)!=1){
		cout << "DataSet::writeDataBlock() - error writing data block (UNIX)" << endl;
		return false;
	}
#else
	if (fwrite(block->varName, sizeof(char), DATABLOCK_STRING_SIZE, fp)!=DATABLOCK_STRING_SIZE){
		cout << "DataSet::writeDataBlock() - could not write block->varName (INTEL)" << endl;
		return false;
	}
	uint32 newLongs[3];
	newLongs[0] = reverseLong(block->varType);
	newLongs[1] = reverseLong(block->size);
	newLongs[2] = reverseLong(block->dataOffset);
	if (fwrite(newLongs, sizeof(int32), 3, fp)!=3){
		cout << "DataSet::writeHeader() - could not write dataBlock->longs (INTEL)" << endl;
		return false;
	}
#endif
	return true;
}

//
// we must read and write in the unix style (endian ... can't remember whether big or small???)
//
bool readDoubles(FILE *fp, double *data, int length)
{
#ifndef INTEL
	if (fread(data, sizeof(double), length, fp)!=length){
		cout << "DataSet::readDoubles() - error reading data (UNIX)" << endl;
		return false;
	}

#else
	unsigned char tempArray[8];
	for (int i=0;i<length;i++){
		if (fread(tempArray, sizeof(char), 8, fp)!=8){
			cout << "DataSet::readDoubles() - could not read double value (INTEL)" << endl;
			return false;
		}
		data[i] = reverseDouble(tempArray);
	}
#endif
	return true;
}

bool writeDoubles(FILE *fp, double *data, int length)
{
#ifndef INTEL
	if (fwrite(data, sizeof(double), length, fp)!=length){
		cout << "DataSet::writeDoubles() - error writing data (UNIX)" << endl;
		return false;
	}

#else
	for (int i=0;i<length;i++){
		unsigned char *tempPtr = reverseDouble(data[i]);
		if (fwrite(tempPtr, sizeof(int32), 2, fp)!=2){
			cout << "DataSet::writeDoubles() - could not write double value (INTEL)" << endl;
			return false;
		}
	}
#endif
	return true;
}


bool DataSet::write(char *filename, Simulation *sim, bool bCompress)
{
	FILE *fp=NULL;
	FileHeader fileHeader;
	DataBlock *dataBlock;
	static double *writeBuffer = NULL;

	if (writeBuffer==NULL){
		writeBuffer = new double[sim->getMesh()->getNumVolumeElements()];
	}

	if ((fp=fopen(filename, "wb"))==NULL){
		cout << "DataSet::read() - could not open file '" << filename << "'" << endl;
		return false;
	}

	rewind(fp);

	strcpy(fileHeader.magicString, MAGIC_STRING);
	strcpy(fileHeader.versionString, VERSION_STRING);
	int numBlocks = 0;
	Variable *var=NULL;
	while ((var = sim->getNextVariable(var)) != NULL){
		numBlocks++;
	}

	if (numBlocks<=0){
		cout << "DataSet::write() - no variables defined" << endl;
	}
   
	fileHeader.sizeX = ((CartesianMesh *)sim->getMesh())->getNumVolumeX();
	fileHeader.sizeY = ((CartesianMesh *)sim->getMesh())->getNumVolumeY();
	fileHeader.sizeZ = ((CartesianMesh *)sim->getMesh())->getNumVolumeZ();
	fileHeader.numBlocks = numBlocks;
	fileHeader.firstBlockOffset = sizeof(FileHeader);

	//
	// write file header
	//   
	if (!writeHeader(fp, &fileHeader)){
		cout << "DataSet::write() - could not write header to file '" << filename << "'" << endl;
		return false;
	}

	if (ftell(fp)!=fileHeader.firstBlockOffset){
		cout << "DataSet::write() - file offset for first block is incorrect" << endl;
		cout << " ftell() says " << ftell(fp) <<", should be " << fileHeader.firstBlockOffset << endl;
		return false;
	}
   
	dataBlock = new DataBlock[numBlocks];
	   
	//
	// write data blocks (describing data)
	//   
	var=NULL;
	int32 dataOffset = fileHeader.firstBlockOffset + numBlocks*sizeof(DataBlock);
	int i=0;
	while ((var = sim->getNextVariable(var)) != NULL){
		memset(dataBlock[i].varName, 0, DATABLOCK_STRING_SIZE * sizeof(char));
		strcpy(dataBlock[i].varName, var->getName().c_str());
	       
		dataBlock[i].varType = var->getVarType();
		dataBlock[i].size = var->getSize();
		dataBlock[i].dataOffset = dataOffset;
		if (!writeDataBlock(fp,dataBlock+i)){
			cout << "DataSet::write() - error writing data block" << endl;
			return false;
		}
		dataOffset += dataBlock[i].size*sizeof(double);
		i++;
	}
	if (i != numBlocks) {
		throw "DataSet::write() : inconsistent total number of data blocks";
	}
	   
	//
	// write data
	//
	for (i=0;i<numBlocks;i++){
		var = sim->getVariableFromName(dataBlock[i].varName);
		if (!var){
			cout << "DataSet::write() - variable '" << dataBlock[i].varName << "' not found during write" << endl;
			return false;
		}
		if (ftell(fp)!=dataBlock[i].dataOffset){
			cout << "DataSet::write() - offset for data is incorrect (block " << i << ", var=" << var->getName() << ")" << endl;
			cout << " ftell() says " << ftell(fp) << ", should be " << dataBlock[i].dataOffset << endl;
			return false;
		}

		if (var->getSize() != dataBlock[i].size) {
			throw "DataSet::write() : inconsistent number of data blocks for variable";
		}
		if (!writeDoubles(fp, var->getCurr(), var->getSize())){
			cout << "DataSet::write() - could not write data for var '" << var->getName() << "'" << endl;
			return false;
		}
	}

	fclose(fp);
	if (bCompress){
		char commandBuffer[200];
		sprintf(commandBuffer,"compress %s",filename);
		system(commandBuffer);
	}

	delete[] dataBlock;
	return true;
}



