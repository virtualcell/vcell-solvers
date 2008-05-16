#include "SpatialReferenceData.h"
#include <assert.h>

#include <sstream>
#include <iostream>
using namespace std;

SpatialReferenceData::SpatialReferenceData(int s) {
	dataSize = s;
	currentTimeIndex = -1;
	currentVarIndex = -1;
}

SpatialReferenceData::~SpatialReferenceData() {
	for (int i = 0; i < (int)dataList.size(); i ++) {
		delete[] dataList.at(i);
	}
}

void SpatialReferenceData::addVariable(string& var) {
	variableList.push_back(var);
}

void SpatialReferenceData::addTimePoint(double time) {
	currentTimeIndex ++;
	timePointList.push_back(time);
	double** newData = new double*[variableList.size()];
	dataList.push_back(newData);
}

void SpatialReferenceData::addVariableData(int varIndex, double* data) {
	dataList.at(currentTimeIndex)[varIndex] = data;
}

const double* SpatialReferenceData::getData(int timeIndex, int varIndex) {
	return dataList.at(timeIndex)[varIndex];
}

int SpatialReferenceData::getNumTimePoints(){
	return (int)timePointList.size();
}

double SpatialReferenceData::getTimePoint(int index){
	return timePointList.at(index);
}

int SpatialReferenceData::findVariable(string& varName) {
	for (int i = 0; i < (int)variableList.size(); i ++) {
		if (variableList[i] == varName) {
			return i;
		}
	}
	stringstream ss;
	ss << "SpatialReferenceData::findVariable() : no variable [" << varName  << "] found in data";
	throw ss.str();
}

void SpatialReferenceData::setWeights(double* w) {
	weights = w;
}

void SpatialReferenceData::show(){
	int numTimePoints = getNumTimePoints();
	int numVariables = getNumVariables();
	for (int i=0;i<numTimePoints;i++){
		std::cout << "time " << getTimePoint(i);
		for (int j=0;j<numVariables;j++){
			std::cout << ", \"" << getVariable(j) << "\"=[";
			const double *data = getData(i,j);
			for (int k=0;k<dataSize;k++){
				std::cout << data[k] << " ";
			}
			std::cout << "] ";
		}
		std::cout << std::endl;
	}
}
