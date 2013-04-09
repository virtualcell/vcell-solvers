/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */

#include <stdlib.h>
#include <string.h>
#include <VCELL/DataSet.h>
#include <VCELL/SimTool.h>
#include <iostream>
#include <string>
using std::cout;
using std::endl;
using std::string;
#include <VCELL/Variable.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/ChomboGeometry.h>
#include <hdf5.h>
#include <H5Tpublic.h>

/*
 * Little-endian operating systems:
 * Linux on x86, x64, MIPSEL, Alpha and Itanium
 * Mac OS X on x86, x64
 * OpenVMS on VAX, Alpha and Itanium
 * Solaris on x86, x64, PowerPC
 * Tru64 UNIX on Alpha
 * Windows on x86, x64 and Itanium
 * Microsoft Xbox 1
 *
 * Big-endian operating systems:
 * AIX on POWER
 * AmigaOS on PowerPC and 680x0
 * HP-UX on Itanium and PA-RISC
 * Linux on MIPS, SPARC, PA-RISC, POWER, PowerPC, 680x0, ESA/390, and z/Architecture
 * Mac OS on PowerPC and 680x0
 * Mac OS X on PowerPC
 * MVS and DOS/VSE on ESA/390, and z/VSE and z/OS on z/Architecture
 * Solaris on SPARC
 * Microsoft Xbox 360, PlayStation 3, Nintendo Wii
 *
 */
Endian DataSet::endian = endian_not_set;
bool DataSet::isBigEndian() {
	if (endian == endian_not_set) {
		union {
			uint32 i;
			char c[4];
		} testint = {0x01020304};
		endian = (testint.c[0] == 1) ? big_endian : little_endian;
		cout << "**This is a " << (endian == big_endian ? "big" : "little") << " endian machine.**" << endl;
	}
	return endian == big_endian;
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
void DataSet::readHeader(FILE *fp, FileHeader *header)
{
	if (isBigEndian()) {
		if (fread(header, sizeof(FileHeader), 1, fp)!=1){
			throw "DataSet::readHeader() - could not read header (big endian)";
		}
	} else {
		if (fread(header->magicString, sizeof(char), 16, fp)!=16){
			throw "DataSet::readHeader() - could not read header->magicString (little endian)";
		}
		if (fread(header->versionString, sizeof(char), 8, fp)!=8){
			throw "DataSet::readHeader() - could not read header->versionString (little endian)";
		}
		uint32 newLongs[5];
		if (fread(&newLongs, sizeof(int32), 5, fp)!=5){
			throw "DataSet::readHeader() - could not read header->offsets.. (little endian)";
		}
		header->numBlocks = reverseLong(newLongs[0]);
		header->firstBlockOffset = reverseLong(newLongs[1]);
		header->sizeX = reverseLong(newLongs[2]);
		header->sizeY = reverseLong(newLongs[3]);
		header->sizeZ = reverseLong(newLongs[4]);
	}
}

void DataSet::writeHeader(FILE *fp, FileHeader *header)
{
	if (isBigEndian()) {
		if (fwrite(header, sizeof(FileHeader), 1, fp)!=1){
			throw "DataSet::writeHeader() - could not write header (big endian)";
		}
	} else {
		if (fwrite(header->magicString, sizeof(char), 16, fp)!=16){
			throw "DataSet::writeHeader() - could not write header->magicString (little endian)";
		}
		if (fwrite(header->versionString, sizeof(char), 8, fp)!=8){
			throw "DataSet::writeHeader() - could not write header->versionString (little endian)";
		}
		uint32 newLongs[5];
		newLongs[0] = reverseLong(header->numBlocks);
		newLongs[1] = reverseLong(header->firstBlockOffset);
		newLongs[2] = reverseLong(header->sizeX);
		newLongs[3] = reverseLong(header->sizeY);
		newLongs[4] = reverseLong(header->sizeZ);
		if (fwrite(newLongs, sizeof(int32), 5, fp)!=5){
			throw "DataSet::writeHeader() - could not write header->longs (little endian)";
		}
	}
}

//struct DataBlock {
//   char   varName[DATABLOCK_STRING_SIZE];
//   long   size;
//   long   dataOffset;
//};
//
void DataSet::readDataBlock(FILE *fp, DataBlock *block)
{
	if (isBigEndian()) {
		if (fread(block, sizeof(DataBlock), 1, fp)!=1){
			throw "DataSet::read() - could not read dataBlock (big endian)";
		}
	} else {
		if (fread(block->varName, sizeof(char), DATABLOCK_STRING_SIZE, fp)!=DATABLOCK_STRING_SIZE){
			throw "DataSet::readDataBlock() - could not read block->varName (little endian)";
		}
		uint32 newLongs[3];
		if (fread(&newLongs, sizeof(int32), 3, fp)!=3){
			throw "DataSet::read() - could not read dataBlock longs... (little endian)";
		}
		block->varType = reverseLong(newLongs[0]);
		block->size = reverseLong(newLongs[1]);
		block->dataOffset = reverseLong(newLongs[2]);
	}
}

void DataSet::writeDataBlock(FILE *fp, DataBlock *block)
{
	if (isBigEndian()) {
		if (fwrite(block, sizeof(DataBlock), 1, fp)!=1){
			throw "DataSet::writeDataBlock() - error writing data block (big endian)";
		}
	} else {
		if (fwrite(block->varName, sizeof(char), DATABLOCK_STRING_SIZE, fp)!=DATABLOCK_STRING_SIZE){
			throw "DataSet::writeDataBlock() - could not write block->varName (little endian)";
		}
		uint32 newLongs[3];
		newLongs[0] = reverseLong(block->varType);
		newLongs[1] = reverseLong(block->size);
		newLongs[2] = reverseLong(block->dataOffset);
		if (fwrite(newLongs, sizeof(int32), 3, fp)!=3){
			throw "DataSet::writeHeader() - could not write dataBlock->longs (little endian)";
		}
	}
}

void DataSet::readDoubles(FILE *fp, double *data, int length)
{
	if (isBigEndian()) {
		if (fread(data, sizeof(double), length, fp)!=length){
			throw "DataSet::readDoubles() - error reading data (big endian)";
		}
	} else {
		unsigned char tempArray[8];
		for (int i=0;i<length;i++){
			if (fread(tempArray, sizeof(char), 8, fp)!=8){
				throw "DataSet::readDoubles() - could not read double value (little endian)";
			}
			data[i] = reverseDouble(tempArray);
		}
	}
}

void DataSet::writeDoubles(FILE *fp, double *data, int length)
{
	if (isBigEndian()) {
		if (fwrite(data, sizeof(double), length, fp)!=length){
			throw "DataSet::writeDoubles() - error writing data (big endian)";
		}
	} else {
		for (int i=0;i<length;i++){
			unsigned char *tempPtr = reverseDouble(data[i]);
			if (fwrite(tempPtr, sizeof(int32), 2, fp)!=2){
				throw "DataSet::writeDoubles() - could not write double value (little endian)";
			}
		}
	}
}

/**
  * the variabe name in the data set can be Cell::Dex,
  * we need to extract Dex
**/
static string extractVarNameFromQualifiedName(char* varName) {
	string str(varName);
	string::size_type pos = str.find("::");
	if (pos != string::npos) {
		str = str.substr(pos + 2);
	}
	return str;
}

void DataSet::read(char *filename, SimulationExpression *sim)
{
	FILE *fp=NULL;
	FileHeader fileHeader;
	DataBlock *dataBlock;

	if ((fp=fopen(filename, "rb"))==NULL){
		char errmsg[512];
		sprintf(errmsg, "DataSet::read() - could not open file '%s'.", filename); 
		throw errmsg;
	}
	DataSet::readHeader(fp,&fileHeader);

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
		DataSet::readDataBlock(fp,dataBlock+i);
	}

	for (int i=0;i<fileHeader.numBlocks;i++){
		string varName = extractVarNameFromQualifiedName(dataBlock[i].varName);
		Variable *var = sim->getVariableFromName(varName);
		if (var==NULL){
			cout << "DataSet::read() - variable '" << dataBlock[i].varName << "' not found in Simulation" << endl;
			continue;
		}
		if (var->getSize()!=dataBlock[i].size){
			char errmsg[512];
			sprintf(errmsg, "DataSet::read() - size mismatch for var '%s', file=%d, var=%ld.", dataBlock[i].varName, dataBlock[i].size, var->getSize());
			throw errmsg;
		}
	      
		if (fseek(fp, dataBlock[i].dataOffset, SEEK_SET)){
			char errmsg[512];
			sprintf(errmsg, "DataSet::read() - could not find data offset ( %d ).", dataBlock[i].dataOffset); 
			throw errmsg;
		}
		DataSet::readDoubles(fp, var->getCurr(), var->getSize());
		cout << "read data for variable '" << var->getName() << "'" << endl;
	}
	delete[] dataBlock;
	   
	fclose(fp);
}

void DataSet::write(SimulationExpression *sim, bool bWriteSimFile)
{

	int numX = sim->getChomboGeometry()->getNumX();
	int numY = sim->getChomboGeometry()->getNumY();
	int numZ = sim->getChomboGeometry()->getNumZ();

	int numVars = sim->getNumVariables();
	if (numVars <= 0){
		cout << "DataSet::write() - no variables defined" << endl;
	}
	
	char filename[128];
	if (bWriteSimFile)
	{
		FILE *fp=NULL;
		FileHeader fileHeader;
		DataBlock *dataBlock;
		SimTool::getInstance()->getSimFileName(filename);
		if ((fp=fopen(filename, "wb"))==NULL){
			char errmsg[512];
			sprintf(errmsg, "DataSet::write() - could not open file '%s'.", filename);
			throw errmsg;
		}

		rewind(fp);

		strcpy(fileHeader.magicString, MAGIC_STRING);
		strcpy(fileHeader.versionString, VERSION_STRING);

		int numBlocks = numVars;
		for (int i = 0; i < numVars; ++ i)
		{
			Variable* var = sim->getVariable(i);
			if (var->getExactErrorVariable() != NULL)
			{
				++ numBlocks; // error
				++ numBlocks; // relative error
			}
		}

		fileHeader.sizeX = numX;
		fileHeader.sizeY = numY;
		fileHeader.sizeZ = numZ;
		fileHeader.numBlocks = numBlocks;
		fileHeader.firstBlockOffset = sizeof(FileHeader);

		//
		// write file header
		//
		DataSet::writeHeader(fp, &fileHeader);
		long ftell_pos = ftell(fp);
		if (ftell_pos != fileHeader.firstBlockOffset){
			char errmsg[512];
			sprintf(errmsg, "DataSet::write() - file offset for first block is incorrect, ftell() says %ld, should be %d", ftell_pos, fileHeader.firstBlockOffset);
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
			strcpy(dataBlock[blockIndex].varName, var->getQualifiedName().c_str());

			dataBlock[blockIndex].varType = var->getVarType();
			dataBlock[blockIndex].size = var->getSize();
			dataBlock[blockIndex].dataOffset = dataOffset;
			DataSet::writeDataBlock(fp,dataBlock+blockIndex);
			dataOffset += dataBlock[blockIndex].size*sizeof(double);
			blockIndex ++;

			var = var->getExactErrorVariable();
			if (var != NULL)
			{
				memset(dataBlock[blockIndex].varName, 0, DATABLOCK_STRING_SIZE * sizeof(char));
				strcpy(dataBlock[blockIndex].varName, var->getQualifiedName().c_str());

				dataBlock[blockIndex].varType = var->getVarType();
				dataBlock[blockIndex].size = var->getSize();
				dataBlock[blockIndex].dataOffset = dataOffset;
				DataSet::writeDataBlock(fp,dataBlock+blockIndex);
				dataOffset += dataBlock[blockIndex].size*sizeof(double);
				blockIndex ++;

				var = var->getRelativeErrorVariable();
				memset(dataBlock[blockIndex].varName, 0, DATABLOCK_STRING_SIZE * sizeof(char));
				strcpy(dataBlock[blockIndex].varName, var->getQualifiedName().c_str());

				dataBlock[blockIndex].varType = var->getVarType();
				dataBlock[blockIndex].size = var->getSize();
				dataBlock[blockIndex].dataOffset = dataOffset;
				DataSet::writeDataBlock(fp,dataBlock+blockIndex);
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
			if (var == NULL){
				char errmsg[512];
				sprintf(errmsg, "DataSet::write() - variable '%s' not found during write", dataBlock[blockIndex].varName);
				throw errmsg;
			}
			ftell_pos = ftell(fp);
			if (ftell_pos != dataBlock[blockIndex].dataOffset){
				char errmsg[512];
				sprintf(errmsg, "DataSet::write() - offset for data is incorrect (block %d, var=%s), ftell() says %ld, should be %d", blockIndex, dataBlock[blockIndex].varName, ftell_pos, dataBlock[blockIndex].dataOffset);
				throw errmsg;
			}
			if (var->getSize() != dataBlock[blockIndex].size) {
				throw "DataSet::write() : inconsistent number of data blocks for variable";
			}
			DataSet::writeDoubles(fp, var->getCurr(), var->getSize());
			blockIndex ++;

			Variable* errVar = var->getExactErrorVariable();
			if (errVar != NULL)
			{
				ftell_pos = ftell(fp);
				if (ftell_pos != dataBlock[blockIndex].dataOffset){
					char errmsg[512];
					sprintf(errmsg, "DataSet::write() - offset for data is incorrect (block %d, var=%s), ftell() says %ld, should be %d", blockIndex, dataBlock[blockIndex].varName, ftell_pos, dataBlock[blockIndex].dataOffset);
					throw errmsg;
				}
				if (errVar->getSize() != dataBlock[blockIndex].size) {
					throw "DataSet::write() : inconsistent number of data blocks for variable";
				}
				DataSet::writeDoubles(fp, errVar->getCurr(), errVar->getSize());
				blockIndex ++;

				errVar = var->getRelativeErrorVariable();
				ftell_pos = ftell(fp);
				if (ftell_pos != dataBlock[blockIndex].dataOffset){
					char errmsg[512];
					sprintf(errmsg, "DataSet::write() - offset for data is incorrect (block %d, var=%s), ftell() says %ld, should be %d", blockIndex, dataBlock[blockIndex].varName, ftell_pos, dataBlock[blockIndex].dataOffset);
					throw errmsg;
				}
				if (errVar->getSize() != dataBlock[blockIndex].size) {
					throw "DataSet::write() : inconsistent number of data blocks for variable";
				}
				DataSet::writeDoubles(fp, errVar->getCurr(), errVar->getSize());
				blockIndex ++;
			}
		}

		fclose(fp);
		delete[] dataBlock;
	}
	
	static const char* SOLUTION_GROUP = "/solution";
	static const char* SOLUTION_ATTR_TIME = "time";
	static const char* SOLUTION_ATTR_VARIABLES = "variables";
	static const char* SOLUTION_ATTR_VARIABLE_TYPES = "variable types";
	
	static const char* SOLUTION_DATASET_ATTR_DOMAIN = "domain";
	static const char* SOLUTION_DATASET_ATTR_VARIABLE_TYPE = "variable type";
	static const char* SOLUTION_DATASET_ATTR_MEAN = "mean";
	static const char* SOLUTION_DATASET_ATTR_SUM_VOLFRAC = "sum of volume fraction";
	static const char* SOLUTION_DATASET_ATTR_RELATIVE_L2ERROR = "relative L2 error";
	static const char* SOLUTION_DATASET_ATTR_MAX_ERROR = "max error";

	SimTool::getInstance()->getSimHdf5FileName(filename);
	hid_t h5SimFile = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	hid_t solGroup = H5Gcreate(h5SimFile, SOLUTION_GROUP, H5P_DEFAULT);
	
	hid_t scalarDataSpace = H5Screate(H5S_SCALAR); // shared among all attributes

	// attribute: time
	hid_t attribute = H5Acreate(solGroup, SOLUTION_ATTR_TIME, H5T_NATIVE_DOUBLE, scalarDataSpace, H5P_DEFAULT);
	double t = sim->getTime_sec();
	H5Awrite(attribute, H5T_NATIVE_DOUBLE, &t);
	H5Aclose(attribute);

	int rank = 1;
	hsize_t dim[1] = {sim->getOutputVarCount()};
	hid_t strType = H5Tcopy(H5T_C_S1);
  H5Tset_size(strType, H5T_VARIABLE);
	hsize_t space = H5Screate_simple(rank, dim, NULL);
	// attribute : variables
	attribute = H5Acreate(solGroup, SOLUTION_ATTR_VARIABLES, strType, space, H5P_DEFAULT);
	H5Awrite(attribute, strType, sim->getOutputVarNames());
	H5Aclose(attribute);
	H5Tclose(strType);

	// attribute : variable types
	attribute = H5Acreate(solGroup, SOLUTION_ATTR_VARIABLE_TYPES, H5T_NATIVE_INT, space, H5P_DEFAULT);
	H5Awrite(attribute, H5T_NATIVE_INT, sim->getOutputVarTypes());
	H5Aclose(attribute);

	H5Sclose(space);
	
	for (int i = 0; i < numVars; i ++) {
		Variable* var = sim->getVariable(i);
		dim[0] = var->getSize();   /* Dataspace dimensions */
		space = H5Screate_simple(rank, dim, NULL);
		char dsName[128];
		sprintf(dsName, "%s/%s", SOLUTION_GROUP, var->getName().c_str());
		hid_t varDataset = H5Dcreate (h5SimFile, dsName, H5T_NATIVE_DOUBLE, space, H5P_DEFAULT);
		H5Dwrite(varDataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, var->getCurr());

		// attribute: domain
		hid_t strtype = H5Tcopy(H5T_C_S1);
		char domainName[50];
		sprintf(domainName, "%s", var->getStructure()->getName().c_str());
    H5Tset_size(strtype, strlen(domainName));
		hid_t attribute = H5Acreate(varDataset, SOLUTION_DATASET_ATTR_DOMAIN, strtype, scalarDataSpace, H5P_DEFAULT);
		H5Awrite(attribute, strtype, domainName);
		H5Aclose(attribute);

		// attribute: variable type
		attribute = H5Acreate(varDataset, SOLUTION_DATASET_ATTR_VARIABLE_TYPE, H5T_NATIVE_INT, scalarDataSpace, H5P_DEFAULT);
		VariableType varType = var->getVarType();
		H5Awrite(attribute, H5T_NATIVE_INT, &varType);
		H5Aclose(attribute);
		
		// attribute: mean
		attribute = H5Acreate(varDataset, SOLUTION_DATASET_ATTR_MEAN, H5T_NATIVE_DOUBLE, scalarDataSpace, H5P_DEFAULT);
		double d = var->getMean();
		H5Awrite(attribute, H5T_NATIVE_DOUBLE, &d);
		H5Aclose(attribute);

		// attribute: sum of vol fraction
		attribute = H5Acreate(varDataset, SOLUTION_DATASET_ATTR_SUM_VOLFRAC, H5T_NATIVE_DOUBLE, scalarDataSpace, H5P_DEFAULT);
		d = var->getStructure()->getSizeFrac();
		H5Awrite(attribute, H5T_NATIVE_DOUBLE, &d);
		H5Aclose(attribute);

		Variable* errVar = var->getExactErrorVariable();
		if (errVar != NULL)
		{
			// attribute: max error
			attribute = H5Acreate(varDataset, SOLUTION_DATASET_ATTR_MAX_ERROR, H5T_NATIVE_DOUBLE, scalarDataSpace, H5P_DEFAULT);
			d = var->getMaxError();
			H5Awrite(attribute, H5T_NATIVE_DOUBLE, &d);
			H5Aclose(attribute);

			// attribute: l2 error
			attribute = H5Acreate(varDataset, SOLUTION_DATASET_ATTR_RELATIVE_L2ERROR, H5T_NATIVE_DOUBLE, scalarDataSpace, H5P_DEFAULT);
			d = var->getL2Error();
			H5Awrite(attribute, H5T_NATIVE_DOUBLE, &d);
			H5Aclose(attribute);

			// dataset : error variable
			sprintf(dsName, "%s/%s", SOLUTION_GROUP, errVar->getName().c_str());
			hid_t errorVarDataSet = H5Dcreate (h5SimFile, dsName, H5T_NATIVE_DOUBLE, space, H5P_DEFAULT);
			H5Dwrite(errorVarDataSet, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, errVar->getCurr());

			// attribute: domain
			hid_t attribute = H5Acreate(errorVarDataSet, SOLUTION_DATASET_ATTR_DOMAIN, strtype, scalarDataSpace, H5P_DEFAULT);
			H5Awrite(attribute, strtype, domainName);
			H5Aclose(attribute);
			
			// attribute: variable type
			attribute = H5Acreate(errorVarDataSet, SOLUTION_DATASET_ATTR_VARIABLE_TYPE, H5T_NATIVE_INT, scalarDataSpace, H5P_DEFAULT);
			H5Awrite(attribute, H5T_NATIVE_INT, &varType);
			H5Aclose(attribute);

			H5Dclose(errorVarDataSet);

			errVar = var->getRelativeErrorVariable();
			sprintf(dsName, "%s/%s", SOLUTION_GROUP, errVar->getName().c_str());
			errorVarDataSet = H5Dcreate (h5SimFile, dsName, H5T_NATIVE_DOUBLE, space, H5P_DEFAULT);
			H5Dwrite(errorVarDataSet, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, errVar->getCurr());

			// attribute: domain
			attribute = H5Acreate(errorVarDataSet, SOLUTION_DATASET_ATTR_DOMAIN, strtype, scalarDataSpace, H5P_DEFAULT);
			H5Awrite(attribute, strtype, domainName);
			H5Aclose(attribute);

			// attribute: variable type
			attribute = H5Acreate(errorVarDataSet, SOLUTION_DATASET_ATTR_VARIABLE_TYPE, H5T_NATIVE_INT, scalarDataSpace, H5P_DEFAULT);
			H5Awrite(attribute, H5T_NATIVE_INT, &varType);
			H5Aclose(attribute);

			H5Dclose(errorVarDataSet);
		}
		H5Dclose(varDataset);
		H5Tclose(strtype);
	}
	H5Sclose(space);
	H5Sclose(scalarDataSpace);
	H5Gclose(solGroup);
	H5Fclose(h5SimFile);
}

