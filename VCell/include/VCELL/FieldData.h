/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef FIELDDATA_H
#define FIELDDATA_H

class FieldData {

private:
	int fdIndex;
	VariableType varType;
	string fdID;
	string fdName;
	string fdVarName;
	double fdTime;
	string fdFile;
	double* data;
	
public:
	FieldData(int arg_fdIndex, VariableType arg_varType, string arg_fdID, string arg_fdName, string arg_fdVarName, double arg_fdTime, string arg_fdFile);
	~FieldData();

	string getID() { return fdID; }	
	VariableType getVariableType() {
		return varType;
	}

	double* getData();
};

#endif
