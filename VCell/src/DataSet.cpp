/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#if ( defined(WIN32) || defined(WIN64) )
#define INTEL
#endif

#include <stdlib.h>

#include <VCELL/SimTypes.h>
#include <VCELL/Variable.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/Mesh.h>
#include <VCELL/DataSet.h>
#include <VCELL/Element.h>
#include <VCELL/Region.h>
#include <VCELL/CartesianMesh.h>
#include <VCELL/FieldData.h>
#include <VCELL/MembraneRegion.h>
#include <VCELL/RandomVariable.h>
using std::endl;

#define CONVOLVE_SUFFIX "_Convolved"

FieldData* getPSFFieldData();

void DataSet::readRandomVariables(char *filename, SimulationExpression *sim)
{
	FILE *fp=NULL;
	FileHeader fileHeader;
	DataBlock *dataBlock;

	if ((fp=fopen(filename, "rb"))==NULL){
		char errmsg[512];
		sprintf(errmsg, "DataSet::read() - could not open file '%s'.", filename); 
		throw errmsg;
	}
	readHeader(fp,&fileHeader);

	if (strcmp(fileHeader.magicString, MAGIC_STRING)){
		throw "DataSet::read() - file is not a VCellDump file";
	}

	if (fileHeader.numBlocks <= 0){
		char errmsg[512];
		sprintf(errmsg, "DataSet::read() - number of blocks ( %d ) less than 1.", fileHeader.numBlocks); 
		throw errmsg;
	}
	   
	dataBlock = new DataBlock[fileHeader.numBlocks];
	   
	if (fseek(fp, fileHeader.firstBlockOffset, SEEK_SET)){
		char errmsg[512];
		sprintf(errmsg, "DataSet::read() - could not find first block at offset %d.", fileHeader.firstBlockOffset); 
		throw errmsg;
	}
	for (int i=0;i<fileHeader.numBlocks;i++){
		readDataBlock(fp,dataBlock+i);
	}

	for (int i=0;i<fileHeader.numBlocks;i++){
		RandomVariable *rv = sim->getRandomVariableFromName(dataBlock[i].varName);
		if (rv==NULL){
			cout << "DataSet::read() - variable '" << dataBlock[i].varName << "' not found in Simulation" << endl;
			continue;
		}
		if (rv->getSize()!=dataBlock[i].size){
			char errmsg[512];
			sprintf(errmsg, "DataSet::read() - size mismatch for var '%s', file=%d, var=%d.", dataBlock[i].varName, dataBlock[i].size, rv->getSize()); 
			throw errmsg;
		}
	      
		if (fseek(fp, dataBlock[i].dataOffset, SEEK_SET)){
			char errmsg[512];
			sprintf(errmsg, "DataSet::read() - could not find data offset ( %d ).", dataBlock[i].dataOffset); 
			throw errmsg;
		}

		readDoubles(fp, rv->getRandomNumbers(), rv->getSize());
		cout << "read data for random variable '" << rv->getName() << "'" << endl;
	}
	delete[] dataBlock;
	   
	fclose(fp);
}


void DataSet::read(char *filename, Simulation *sim)
{
	FILE *fp=NULL;
	FileHeader fileHeader;
	DataBlock *dataBlock;

	if ((fp=fopen(filename, "rb"))==NULL){
		char errmsg[512];
		sprintf(errmsg, "DataSet::read() - could not open file '%s'.", filename); 
		throw errmsg;
	}
	readHeader(fp,&fileHeader);

	if (strcmp(fileHeader.magicString, MAGIC_STRING)){
		throw "DataSet::read() - file is not a VCellDump file";
	}

	if (fileHeader.numBlocks <= 0){
		char errmsg[512];
		sprintf(errmsg, "DataSet::read() - number of blocks ( %d ) less than 1.", fileHeader.numBlocks); 
		throw errmsg;
	}
	   
	dataBlock = new DataBlock[fileHeader.numBlocks];
	   
	if (fseek(fp, fileHeader.firstBlockOffset, SEEK_SET)){
		char errmsg[512];
		sprintf(errmsg, "DataSet::read() - could not find first block at offset %d.", fileHeader.firstBlockOffset); 
		throw errmsg;
	}
	for (int i=0;i<fileHeader.numBlocks;i++){
		readDataBlock(fp,dataBlock+i);
	}

	for (int i=0;i<fileHeader.numBlocks;i++){
		Variable *var = sim->getVariableFromName(dataBlock[i].varName);
		if (var==NULL){
			cout << "DataSet::read() - variable '" << dataBlock[i].varName << "' not found in Simulation" << endl;
			continue;
		}
		if (var->getSize()!=dataBlock[i].size){
			char errmsg[512];
			sprintf(errmsg, "DataSet::read() - size mismatch for var '%s', file=%d, var=%d.", dataBlock[i].varName, dataBlock[i].size, var->getSize()); 
			throw errmsg;
		}
	      
		if (fseek(fp, dataBlock[i].dataOffset, SEEK_SET)){
			char errmsg[512];
			sprintf(errmsg, "DataSet::read() - could not find data offset ( %d ).", dataBlock[i].dataOffset); 
			throw errmsg;
		}
		readDoubles(fp, var->getCurr(), var->getSize());
		var->update();   
		cout << "read data for variable '" << var->getName() << "'" << endl;
	}
	delete[] dataBlock;
	   
	fclose(fp);
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
void readHeader(FILE *fp, FileHeader *header)
{
#ifndef INTEL
	if (fread(header, sizeof(FileHeader), 1, fp)!=1){
		throw "DataSet::readHeader() - could not read header (UNIX)";
	}
#else
	if (fread(header->magicString, sizeof(char), 16, fp)!=16){
		throw "DataSet::readHeader() - could not read header->magicString (INTEL)";
	}
	if (fread(header->versionString, sizeof(char), 8, fp)!=8){
		throw "DataSet::readHeader() - could not read header->versionString (INTEL)";
	}
	uint32 newLongs[5];
	if (fread(&newLongs, sizeof(int32), 5, fp)!=5){
		throw "DataSet::readHeader() - could not read header->offsets.. (INTEL)";
	}
	header->numBlocks = reverseLong(newLongs[0]);
	header->firstBlockOffset = reverseLong(newLongs[1]);
	header->sizeX = reverseLong(newLongs[2]);
	header->sizeY = reverseLong(newLongs[3]);
	header->sizeZ = reverseLong(newLongs[4]);
#endif
}

void writeHeader(FILE *fp, FileHeader *header)
{
#ifndef INTEL
	if (fwrite(header, sizeof(FileHeader), 1, fp)!=1){
		throw "DataSet::writeHeader() - could not write header (UNIX)";
	}
#else
	if (fwrite(header->magicString, sizeof(char), 16, fp)!=16){
		throw "DataSet::writeHeader() - could not write header->magicString (INTEL)";
	}
	if (fwrite(header->versionString, sizeof(char), 8, fp)!=8){
		throw "DataSet::writeHeader() - could not write header->versionString (INTEL)";
	}
	uint32 newLongs[5];
	newLongs[0] = reverseLong(header->numBlocks);
	newLongs[1] = reverseLong(header->firstBlockOffset);
	newLongs[2] = reverseLong(header->sizeX);
	newLongs[3] = reverseLong(header->sizeY);
	newLongs[4] = reverseLong(header->sizeZ);
	if (fwrite(newLongs, sizeof(int32), 5, fp)!=5){
		throw "DataSet::writeHeader() - could not write header->longs (INTEL)";
	}
#endif
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
void readDataBlock(FILE *fp, DataBlock *block)
{
#ifndef INTEL
	if (fread(block, sizeof(DataBlock), 1, fp)!=1){
		throw "DataSet::read() - could not read dataBlock (UNIX)";
	}
#else
	if (fread(block->varName, sizeof(char), DATABLOCK_STRING_SIZE, fp)!=DATABLOCK_STRING_SIZE){
		throw "DataSet::readDataBlock() - could not read block->varName (INTEL)";
	}
	uint32 newLongs[3];
	if (fread(&newLongs, sizeof(int32), 3, fp)!=3){
		throw "DataSet::read() - could not read dataBlock longs... (INTEL)";
	}
	block->varType = reverseLong(newLongs[0]);
	block->size = reverseLong(newLongs[1]);
	block->dataOffset = reverseLong(newLongs[2]);
#endif
}

void writeDataBlock(FILE *fp, DataBlock *block)
{
#ifndef INTEL
	if (fwrite(block, sizeof(DataBlock), 1, fp)!=1){
		throw "DataSet::writeDataBlock() - error writing data block (UNIX)";
	}
#else
	if (fwrite(block->varName, sizeof(char), DATABLOCK_STRING_SIZE, fp)!=DATABLOCK_STRING_SIZE){
		throw "DataSet::writeDataBlock() - could not write block->varName (INTEL)";
	}
	uint32 newLongs[3];
	newLongs[0] = reverseLong(block->varType);
	newLongs[1] = reverseLong(block->size);
	newLongs[2] = reverseLong(block->dataOffset);
	if (fwrite(newLongs, sizeof(int32), 3, fp)!=3){
		throw "DataSet::writeHeader() - could not write dataBlock->longs (INTEL)";
	}
#endif
}

//
// we must read and write in the unix style (endian ... can't remember whether big or small???)
//
void readDoubles(FILE *fp, double *data, int length)
{
#ifndef INTEL
	if (fread(data, sizeof(double), length, fp)!=length){
		throw "DataSet::readDoubles() - error reading data (UNIX)";
	}

#else
	unsigned char tempArray[8];
	for (int i=0;i<length;i++){
		if (fread(tempArray, sizeof(char), 8, fp)!=8){
			throw "DataSet::readDoubles() - could not read double value (INTEL)";
		}
		data[i] = reverseDouble(tempArray);
	}
#endif
}

void writeDoubles(FILE *fp, double *data, int length)
{
#ifndef INTEL
	if (fwrite(data, sizeof(double), length, fp)!=length){
		throw "DataSet::writeDoubles() - error writing data (UNIX)";
	}

#else
	for (int i=0;i<length;i++){
		unsigned char *tempPtr = reverseDouble(data[i]);
		if (fwrite(tempPtr, sizeof(int32), 2, fp)!=2){
			throw "DataSet::writeDoubles() - could not write double value (INTEL)";
		}
	}
#endif
}

void DataSet::convolve(Simulation* sim, Variable* var, double* values) {

	FieldData* psfFieldData = getPSFFieldData();
	if (psfFieldData == 0) {
		throw "psf field data is not defined";
	}

	CartesianMesh* mesh = (CartesianMesh *)sim->getMesh(); 

	int meshX = mesh->getNumVolumeX();
	int meshY = mesh->getNumVolumeY();
	int meshZ = mesh->getNumVolumeZ();
	int meshXY = meshX * meshY;

	int psfX = psfFieldData->getSizeX();
	int psfY = psfFieldData->getSizeY();
	int psfZ = psfFieldData->getSizeZ();
	int psfZOffset = -psfZ/2;
	int psfYOffset = -psfY/2;
	int psfXOffset = -psfX/2;

	double* psfData = psfFieldData->getData();
	memset(values, 0, meshX * meshY * meshZ * sizeof(double));

	if (var->getVarType() == VAR_VOLUME || var->getVarType() == VAR_VOLUME_REGION) {
		for (int z = 0; z < meshZ; z ++) {
			for (int y = 0; y < meshY; y ++) {
				for (int x = 0; x < meshX; x ++) {
					int volIndex = z * meshXY + y * meshX + x;
					int psfindex = 0;
					for (int zz = 0; zz < psfZ; zz ++) {						
						for (int yy = 0; yy < psfY; yy ++) {
							for (int xx = 0; xx < psfX; xx ++) {
								int volIndex2X = x + psfXOffset + xx;
								int volIndex2Y = y + psfYOffset + yy;
								int volIndex2Z = z + psfZOffset + zz;
								double psf_val = psfData[psfindex ++];

								if (volIndex2X >= 0 && volIndex2Y >= 0 
										&& volIndex2Z >= 0 && volIndex2X < meshX && volIndex2Y < meshY && volIndex2Z < meshZ) {
									int volIndex2 = volIndex2Z * meshXY + volIndex2Y * meshX + volIndex2X;	
									if (var->getVarType() == VAR_VOLUME_REGION) {
										values[volIndex] += var->getCurr()[mesh->getVolumeElements()[volIndex2].getRegionIndex()] * psf_val;
									} else {
										values[volIndex] += var->getCurr()[volIndex2] * psf_val;
									}
								}
							}
						}
					}
				}
			}
		}
	} else if (var->getVarType() == VAR_MEMBRANE || var->getVarType() == VAR_MEMBRANE_REGION) {				
		for (int m = 0; m < mesh->getNumMembraneElements(); m++) {
			int insideVolIndex = mesh->getMembraneElements()[m].vindexFeatureLo;
			int outsideVolIndex = mesh->getMembraneElements()[m].vindexFeatureHi;
			MeshCoord insideMC = mesh->getMeshCoord(insideVolIndex);
			MeshCoord outsideMC = mesh->getMeshCoord(outsideVolIndex);
			double fullArea = mesh->getXArea_squm();
			int diffVolIndex = abs(outsideVolIndex - insideVolIndex);
			if (diffVolIndex  == meshX) {
				fullArea = mesh->getYArea_squm();
			} else if (diffVolIndex == meshXY) {
				fullArea = mesh->getZArea_squm();
			}

			double memareaRatio = mesh->getMembraneElements()[m].area/fullArea;
			int psfindex = 0;
			for (int zz = 0; zz < psfZ; zz ++) {								
				for (int yy = 0; yy < psfY; yy ++) {									
					for (int xx = 0; xx < psfX; xx ++) {
						double psf_val = psfData[psfindex ++];

						// inside
						int volIndex2X = insideMC.x + psfXOffset + xx;
						int volIndex2Y = insideMC.y + psfYOffset + yy;
						int volIndex2Z = insideMC.z + psfZOffset + zz;
						if (volIndex2X >= 0 && volIndex2Y >= 0 && volIndex2Z >= 0 && volIndex2X < meshX && volIndex2Y < meshY && volIndex2Z < meshZ) {
							int volIndex2 = volIndex2Z * meshXY + volIndex2Y * meshX + volIndex2X;
							if (var->getVarType() == VAR_MEMBRANE_REGION) {
								values[volIndex2] += var->getCurr()[mesh->getMembraneElements()[m].getRegionIndex()] * psf_val/2 * memareaRatio;
							} else {
								values[volIndex2] += var->getCurr()[m] * psf_val/2 * memareaRatio;
							}
						}

						// outside
						volIndex2X = outsideMC.x + psfXOffset + xx;
						volIndex2Y = outsideMC.y + psfYOffset + yy;
						volIndex2Z = outsideMC.z + psfZOffset + zz;
						if (volIndex2X >= 0 && volIndex2Y >= 0 && volIndex2Z >= 0 && volIndex2X < meshX && volIndex2Y < meshY && volIndex2Z < meshZ) {
							int volIndex2 = volIndex2Z * meshXY + volIndex2Y * meshX + volIndex2X;
							if (var->getVarType() == VAR_MEMBRANE_REGION) {
								values[volIndex2] += var->getCurr()[mesh->getMembraneElements()[m].getRegionIndex()] * psf_val/2 * memareaRatio;
							} else {
								values[volIndex2] += var->getCurr()[m] * psf_val/2 * memareaRatio;
							}
						}
					}						
				}
			}
		}
	}			
}

void DataSet::write(char *filename, Simulation *sim, bool bCompress)
{
	bool bStandalone = false;
	SimulationExpression* simStandalone = 0;
	if (typeid(*sim) == typeid(SimulationExpression)) {
		bStandalone = true;
		simStandalone = (SimulationExpression*)sim;
	}

	FILE *fp=NULL;
	FileHeader fileHeader;
	DataBlock *dataBlock;
	static double *writeBuffer = NULL;

	if (writeBuffer==NULL){
		writeBuffer = new double[sim->getMesh()->getNumVolumeElements()];
	}

	if ((fp=fopen(filename, "wb"))==NULL){
		char errmsg[512];
		sprintf(errmsg, "DataSet::write() - could not open file '%s'.", filename); 
		throw errmsg;
	}

	rewind(fp);

	strcpy(fileHeader.magicString, MAGIC_STRING);
	strcpy(fileHeader.versionString, VERSION_STRING);
	int numVars = sim->getNumVariables();
	if (numVars <= 0){
		cout << "DataSet::write() - no variables defined" << endl;
	}
   
	FieldData* psfFieldData = getPSFFieldData();
	int numBlocks = psfFieldData == 0 ? numVars : numVars*2;

	if (bStandalone) {
		numBlocks += simStandalone->getNumRandomVariables();
	}

	fileHeader.sizeX = ((CartesianMesh *)sim->getMesh())->getNumVolumeX();
	fileHeader.sizeY = ((CartesianMesh *)sim->getMesh())->getNumVolumeY();
	fileHeader.sizeZ = ((CartesianMesh *)sim->getMesh())->getNumVolumeZ();
	int volVarSize = fileHeader.sizeX * fileHeader.sizeY * fileHeader.sizeZ;
	fileHeader.numBlocks = numBlocks;
	fileHeader.firstBlockOffset = sizeof(FileHeader);

	//
	// write file header
	//   
	writeHeader(fp, &fileHeader);
	long ftell_pos = ftell(fp);
	if (ftell_pos != fileHeader.firstBlockOffset){
		char errmsg[512];
		sprintf(errmsg, "DataSet::write() - file offset for first block is incorrect, ftell() says %d, should be %d", ftell_pos, fileHeader.firstBlockOffset);
		throw errmsg;
	}
   
	dataBlock = new DataBlock[numBlocks];
	   
	//
	// write data blocks (describing data)
	//   
	int blockIndex = 0;
	int32 dataOffset = fileHeader.firstBlockOffset + numBlocks * sizeof(DataBlock);
	for (int i = 0; i < numVars; i ++) {
		Variable* var = sim->getVariable(i);
		memset(dataBlock[blockIndex].varName, 0, DATABLOCK_STRING_SIZE * sizeof(char));
		strcpy(dataBlock[blockIndex].varName, var->getName().c_str());
		
		dataBlock[blockIndex].varType = var->getVarType();
		dataBlock[blockIndex].size = var->getSize();
		dataBlock[blockIndex].dataOffset = dataOffset;
		writeDataBlock(fp,dataBlock+blockIndex);
		dataOffset += dataBlock[blockIndex].size*sizeof(double);
		blockIndex ++;
	}

	// write data blocks for _Convolved variables
	if (psfFieldData != 0) {
		for (int i = 0; i < numVars; i ++) {
			Variable* var = sim->getVariable(i);			
			string varz_name = var->getName() + CONVOLVE_SUFFIX;
			memset(dataBlock[blockIndex].varName, 0, DATABLOCK_STRING_SIZE * sizeof(char));
			strcpy(dataBlock[blockIndex].varName, varz_name.c_str());
	       
			dataBlock[blockIndex].varType = VAR_VOLUME;
			dataBlock[blockIndex].size = volVarSize;
			dataBlock[blockIndex].dataOffset = dataOffset;
			writeDataBlock(fp,dataBlock + blockIndex);
			dataOffset += dataBlock[blockIndex].size*sizeof(double);
			blockIndex ++;
		}
	}
	
	if (bStandalone) {
		int numRandVars = simStandalone->getNumRandomVariables();
		for (int i = 0; i < numRandVars; i ++) {
			RandomVariable* rv = simStandalone->getRandomVariable(i);			
			memset(dataBlock[blockIndex].varName, 0, DATABLOCK_STRING_SIZE * sizeof(char));
			strcpy(dataBlock[blockIndex].varName, rv->getName().c_str());
		
			dataBlock[blockIndex].varType = rv->getVariableType();
			dataBlock[blockIndex].size = rv->getSize();
			dataBlock[blockIndex].dataOffset = dataOffset;
			writeDataBlock(fp,dataBlock + blockIndex);
			dataOffset += dataBlock[blockIndex].size*sizeof(double);
			blockIndex ++;
		}
	}	   

	//
	// write data
	//
	blockIndex = 0;
	for (int i = 0; i < numVars; i ++) {
		Variable* var = sim->getVariable(i);
		if (!var){
			char errmsg[512];
			sprintf(errmsg, "DataSet::write() - variable '%s' not found during write", dataBlock[blockIndex].varName);
			throw errmsg;
		}
		ftell_pos = ftell(fp);
		if (ftell_pos != dataBlock[blockIndex].dataOffset){
			char errmsg[512];
			sprintf(errmsg, "DataSet::write() - offset for data is incorrect (block %d, var=%s), ftell() says %d, should be %d", blockIndex, dataBlock[blockIndex].varName, ftell_pos, dataBlock[blockIndex].dataOffset);
			throw errmsg;
		}

		if (var->getSize() != dataBlock[blockIndex].size) {
			throw "DataSet::write() : inconsistent number of data blocks for variable";
		}
		writeDoubles(fp, var->getCurr(), var->getSize());
		blockIndex ++;
	}
	
	//
	// write data for _Convolved variables
	//
	if (psfFieldData != 0) {
		double* values = new double[volVarSize];

		for (int i = 0; i < numVars; i ++) {
			Variable* var = sim->getVariable(i);
			convolve(sim, var, values);

			ftell_pos = ftell(fp);
			if (ftell_pos != dataBlock[blockIndex].dataOffset){
				char errmsg[512];
				sprintf(errmsg, "DataSet::write() - offset for data is "
					"incorrect (block %d, var=%s), ftell() says %d, should be %d", 
					blockIndex, dataBlock[blockIndex].varName, ftell_pos, dataBlock[blockIndex].dataOffset);
				throw errmsg;
			}
			if (volVarSize != dataBlock[blockIndex].size) {
				throw "DataSet::write() : inconsistent number of data blocks for variable";
			}

			writeDoubles(fp, values, volVarSize);
			blockIndex ++;
		}	
	}

	//
	// write data for random variables
	//
	if (bStandalone) {
		int numRandVars = simStandalone->getNumRandomVariables();
		for (int i = 0; i < numRandVars; i ++) {
			RandomVariable* rv = simStandalone->getRandomVariable(i);						
			ftell_pos = ftell(fp);
			if (ftell_pos != dataBlock[blockIndex].dataOffset){
				char errmsg[512];
				sprintf(errmsg, "DataSet::write() - offset for data is "
					"incorrect (block %d, var=%s), ftell() says %d, should be %d", 
					blockIndex, dataBlock[blockIndex].varName, ftell_pos, dataBlock[blockIndex].dataOffset);
				throw errmsg;
			}

			if (rv->getSize() != dataBlock[blockIndex].size) {
				throw "DataSet::write() : inconsistent number of data blocks for variable";
			}
			writeDoubles(fp, rv->getRandomNumbers(), rv->getSize());
			blockIndex ++;
		}
	}

	fclose(fp);
	if (bCompress){
		char commandBuffer[200];
		sprintf(commandBuffer,"compress %s",filename);
		system(commandBuffer);
	}

	delete[] dataBlock;
}
