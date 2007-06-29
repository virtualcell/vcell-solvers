#include <VCELL/Variable.h>
#include <VCELL/DataSet.h>
#include <VCELL/FieldData.h>
#include <string>
#include <iostream>
using namespace std;

bool readHeader(FILE *fp, FileHeader *header);
bool readDataBlock(FILE *fp, DataBlock *block);
bool readDoubles(FILE *fp, double *data, int length);

FieldData::FieldData(int arg_fdIndex, string arg_fdID, string arg_fdName, string arg_fdVarName, double arg_fdTime, string arg_fdFile) {
	fdIndex = arg_fdIndex;
	fdID = arg_fdID;
	fdName = arg_fdName;
	fdVarName = arg_fdVarName;
	fdTime = arg_fdTime;
	fdFile = arg_fdFile;
	data = 0;
}

FieldData::~FieldData() {
	delete[] data;
}

double* FieldData::getData() {
	if (data != 0) {
		return data;
	}

	FILE *fp=NULL;
	FileHeader fileHeader;
	DataBlock *dataBlock;

	if ((fp=fopen(fdFile.c_str(), "rb"))==NULL){
		cout << "FieldData::getData() - could not open file '" << fdFile << "'." << endl;
		return 0;
	}
	if (!readHeader(fp,&fileHeader)){
		cout << "FieldData::getData - could not read header from file '" << fdFile << "'." << endl;
		return 0;
	}

	if (strcmp(fileHeader.magicString, MAGIC_STRING)){
		cout << "FieldData::getData - file is not a VCellDump file." << endl;
		return 0;
	}

	if (fileHeader.numBlocks <= 0){
		cout << "FieldData::getData - number of blocks (" << fileHeader.numBlocks << ") less than 1." << endl;
		return 0;
	}
	   
	dataBlock = new DataBlock[fileHeader.numBlocks];
	   
	if (fseek(fp, fileHeader.firstBlockOffset, SEEK_SET)){
		cout << "FieldData::getData - could not find first block at offset " << fileHeader.firstBlockOffset << "." << endl; 
		return 0;
	}
	for (int i = 0; i < fileHeader.numBlocks; i ++){
		if (!readDataBlock(fp,dataBlock+i)){
			cout << "FieldData::getData - could not read dataBlock " << i << "." << endl;
			return 0;
		}
	}

	for (int i = 0; i < fileHeader.numBlocks; i ++){
		if (strcmp(fdVarName.c_str(), dataBlock[i].varName)) {
			continue;
		}
		data = new double[dataBlock[i].size];
	      
		if (fseek(fp, dataBlock[i].dataOffset, SEEK_SET)){
			cout << "FieldData::getData - could not find data offset (" << dataBlock[i].dataOffset << ")." << endl;
			return false;
		}
		if (!readDoubles(fp, data, dataBlock[i].size)){
			cout << "getFieldData - could not read data for field '" << fdName << "'." << endl;
			return false;
		}    
		cout << "read data for field '" << fdName << "'." << endl;
		break;
	}
	delete[] dataBlock;
	   
	if (data == 0) {
		char errMsg[512];
		sprintf(errMsg, "Data not found for variable '%s' in field '%s'", fdVarName, fdName);		
		throw errMsg;
	}
	fclose(fp);
	return data;
}
