/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/DataGenerator.h>
#include <VCELL/Feature.h>

#include <limits>
const double DataGenerator::double_max = std::numeric_limits<double>::max();
const double DataGenerator::double_min = std::numeric_limits<double>::min();

DataGenerator::DataGenerator(string& name, Feature* f) {
	this->name = name;
	this->feature = f;
	dataSize = 0;
	data = NULL;
}

DataGenerator::~DataGenerator() {
	delete[] data;
}

string DataGenerator::getQualifiedName(){
	if (feature != 0){
		return feature->getName() + "::" + name;
	}else{
		return name;
	}
}