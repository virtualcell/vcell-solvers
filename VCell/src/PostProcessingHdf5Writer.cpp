/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/PostProcessingBlock.h>
#include <VCELL/PostProcessingHdf5Writer.h>
#include <VCELL/VariableStatisticsDataGenerator.h>
#include <VCELL/Variable.h>
#include <typeinfo>
#include <H5Cpp.h>

const char* PostProcessingHdf5Writer::PPGroupName  = "/PostProcessing";
const char* PostProcessingHdf5Writer::TimesDataSetName  = "Times";

PostProcessingHdf5Writer::PostProcessingHdf5Writer(char* h5PPFileName, PostProcessingBlock* ppb) {
	this->postProcessingBlock = ppb;
	h5PPFile = new H5::H5File(h5PPFileName, H5F_ACC_TRUNC);
	timesDataSet = NULL;
	bFirstTime = true;
}

PostProcessingHdf5Writer::~PostProcessingHdf5Writer() {
	delete timesDataSet;
	delete h5PPFile;
}

void PostProcessingHdf5Writer::writeOutput() {
	try {
		int timesRank = 1;
		hsize_t timesDims = 1;	
		hsize_t maxDims = H5S_UNLIMITED;
		H5::DataSpace timesDataSpace(timesRank, &timesDims, &maxDims);

		if (bFirstTime) {
			// create post processing group /PostProcessing
			h5PPFile->createGroup(PPGroupName);

			// enable chunking
			H5::DSetCreatPropList cparms;
			hsize_t chunkDims = 500;
			cparms.setChunk(timesRank, &chunkDims);
			int fill_val = -1;
			cparms.setFillValue(H5::PredType::NATIVE_INT, &fill_val);
			// create dataset
			char timesDataSetName[128];
			sprintf(timesDataSetName, "%s/%s", PPGroupName, TimesDataSetName);
			timesDataSet = new H5::DataSet(h5PPFile->createDataSet(timesDataSetName, H5::PredType::NATIVE_DOUBLE, timesDataSpace, cparms));
		}

		// write current time
		double currTime = postProcessingBlock->simulation->getTime_sec();
		hsize_t  size = timeList.size() + 1;
		timesDataSet->extend(&size);
		hsize_t dim = 1;
		hsize_t offset = timeList.size();
		H5::DataSpace fspace = timesDataSet->getSpace();
		fspace.selectHyperslab(H5S_SELECT_SET, &dim, &offset);
		timesDataSet->write(&currTime, H5::PredType::NATIVE_DOUBLE, timesDataSpace, fspace);

		timeList.push_back(currTime);

		int timeIndex = timeList.size() - 1;
		vector<DataGenerator*>::iterator it;
		for(it = postProcessingBlock->dataGeneratorList.begin(); it < postProcessingBlock->dataGeneratorList.end(); ++ it) {
			DataGenerator* dataGenerator = *it;
			dataGenerator->computePPData(postProcessingBlock->simulation);
			writeDataSet(dataGenerator, timeIndex);
		}
		
		h5PPFile->flush(H5F_SCOPE_GLOBAL);
	} catch(H5::Exception error ) {
		throw error.getDetailMsg();
	}
	bFirstTime = false;
}

void PostProcessingHdf5Writer::writeDataSet(DataGenerator* dataGenerator, int timeIndex) {
	H5::DataSpace attributeDataSpace(H5S_SCALAR);
	H5::StrType attributeNameStrType(0, 64);

	char dataGeneratorGroupName[128];
	sprintf(dataGeneratorGroupName, "%s/%s", PPGroupName, dataGenerator->getQualifiedName().c_str());
	if (bFirstTime) {
		// create group /PostProcessing/fluor
		H5::Group hdf5Group = h5PPFile->createGroup(dataGeneratorGroupName);

		if (typeid(*dataGenerator) == typeid(VariableStatisticsDataGenerator)) {
			// attributes : all the components
			int numVar = postProcessingBlock->simulation->getNumVariables();
			for (int i = 0; i < numVar; i ++) {
				Variable* var = postProcessingBlock->simulation->getVariable(i);
				char attrName[64];
				char compName[64];

				sprintf(attrName, "comp_%d", 2 * i);
				H5::Attribute attribute = hdf5Group.createAttribute(attrName, attributeNameStrType, attributeDataSpace);
				sprintf(compName, "%s_average", var->getName().c_str());
				attribute.write(attributeNameStrType, compName);

				sprintf(attrName, "comp_%d", 2 * i + 1);
				attribute = hdf5Group.createAttribute(attrName, attributeNameStrType, attributeDataSpace);
				sprintf(compName, "%s_total", var->getName().c_str());
				attribute.write(attributeNameStrType, compName);
			}
		}
	}

	// create dataset /PostProcessing/fluor/time0
	char timeName[128];
	sprintf(timeName, "time%d", timeIndex);	

	char dataSetName[128];
	sprintf(dataSetName, "%s/%s/%s", PPGroupName, dataGenerator->getQualifiedName().c_str(), timeName);	

	// create dataspace
	H5::DataSpace dataspace(dataGenerator->hdf5Rank, dataGenerator->hdf5Dims); 
	H5::DataSet dataSet = h5PPFile->createDataSet(dataSetName, H5::PredType::NATIVE_DOUBLE, dataspace);

	// write dataset
	dataSet.write(dataGenerator->getData(), H5::PredType::NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT);

	// close dataset
	dataspace.close();
	dataSet.close();
}
