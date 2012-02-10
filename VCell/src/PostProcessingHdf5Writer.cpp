/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/PostProcessingBlock.h>
#include <VCELL/PostProcessingHdf5Writer.h>
#include <VCELL/VariableStatisticsDataGenerator.h>
#include <VCELL/Variable.h>
#include <H5Cpp.h>

const char* PostProcessingHdf5Writer::PPGroupName  = "/PostProcessing";
const char* PostProcessingHdf5Writer::TimesDataSetName  = "Times";

PostProcessingHdf5Writer::PostProcessingHdf5Writer(char* h5PPFileName, PostProcessingBlock* ppb) {
	this->postProcessingBlock = ppb;
	h5PPFile = new H5::H5File(h5PPFileName, H5F_ACC_TRUNC);
	bFirstTime = true;
}

PostProcessingHdf5Writer::~PostProcessingHdf5Writer() {
	delete h5PPFile;
}

void PostProcessingHdf5Writer::writeOutput() {
	try {
		if (bFirstTime) {
			// create post processing group /PostProcessing
			h5PPFile->createGroup(PPGroupName);
		}

		timeList.push_back(postProcessingBlock->simulation->getTime_sec());

		int timeIndex = timeList.size() - 1;
		vector<DataGenerator*>::iterator it;
		for(it = postProcessingBlock->dataGeneratorList.begin(); it < postProcessingBlock->dataGeneratorList.end(); ++ it) {
			DataGenerator* dataGenerator = *it;
			dataGenerator->computePPData(postProcessingBlock->simulation);
			writeDataSet(dataGenerator, timeIndex);
		}
		// close time group
		
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

void PostProcessingHdf5Writer::onComplete() {
	try {
		int rank = 1;
		hsize_t hdf5Dims = timeList.size();	
		H5::DataSpace dataspace(rank, &hdf5Dims);
		char timesDataSetName[128];
		sprintf(timesDataSetName, "%s/%s", PPGroupName, TimesDataSetName);
		H5::DataSet timesDataSet = h5PPFile->createDataSet(timesDataSetName, H5::PredType::NATIVE_DOUBLE, dataspace);

		double* times = new double[timeList.size()];
		int timeIndex = -1;
		for (vector<double>::iterator iter = timeList.begin(); iter < timeList.end(); ++iter) {
			times[++ timeIndex] = *iter;
		}
		timesDataSet.write(times, H5::PredType::NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT);
		timesDataSet.close();
		delete[] times;
	} catch(H5::Exception error ) {
		throw error.getDetailMsg();
	}
}
