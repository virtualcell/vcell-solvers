#include <VCELL/Variable.h>
#include <VCELL/DataSet.h>
#include <VCELL/FieldData.h>
#include <string>
#include <iostream>
using namespace std;

void readHeader(FILE *fp, FileHeader *header);
void readDataBlock(FILE *fp, DataBlock *block);
void readDoubles(FILE *fp, double *data, int length);

FieldData::FieldData(int arg_fdIndex, VariableType arg_varType, string arg_fdID, string arg_fdName, string arg_fdVarName, double arg_fdTime, string arg_fdFile) {	
	fdIndex = arg_fdIndex;
	varType = arg_varType;
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
		char errmsg[512];
		sprintf(errmsg, "FieldData::getData() - could not open file '%s'.", fdFile.c_str()); 
		throw errmsg;
	}

	readHeader(fp,&fileHeader);

	if (strcmp(fileHeader.magicString, MAGIC_STRING)){
		throw "FieldData::getData() - file is not a VCellDump file.";
	}

	if (fileHeader.numBlocks <= 0){
		char errmsg[512];
		sprintf(errmsg, "FieldData::getData() - number of blocks ( %d ) less than 1.", fileHeader.numBlocks); 
		throw errmsg;
	}
	   
	dataBlock = new DataBlock[fileHeader.numBlocks];
	   
	if (fseek(fp, fileHeader.firstBlockOffset, SEEK_SET)){
		char errmsg[512];
		sprintf(errmsg, "FieldData::getData() - could not find first block at offset %d.", fileHeader.firstBlockOffset); 
		throw errmsg;
	}
	for (int i = 0; i < fileHeader.numBlocks; i ++){
		readDataBlock(fp,dataBlock+i);
	}

	for (int i = 0; i < fileHeader.numBlocks; i ++){
		if (strcmp(fdVarName.c_str(), dataBlock[i].varName)) {
			continue;
		}
		data = new double[dataBlock[i].size];
	      
		if (fseek(fp, dataBlock[i].dataOffset, SEEK_SET)){
			char errmsg[512];
			sprintf(errmsg, "FieldData::getData() - could not find data offset ( %d ).", dataBlock[i].dataOffset); 
			throw errmsg;
		}
		readDoubles(fp, data, dataBlock[i].size);
		cout << "read data for field '" << fdName << "'." << endl;
		break;
	}
	delete[] dataBlock;
	   
	if (data == 0) {
		char errMsg[512];
		sprintf(errMsg, "Data not found for variable '%s' in field '%s'", fdVarName.c_str(), fdName.c_str());
		throw errMsg;
	}
	fclose(fp);
	return data;
}
