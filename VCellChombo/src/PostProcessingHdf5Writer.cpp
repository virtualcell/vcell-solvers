/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/PostProcessingHdf5Writer.h>
#include <VCELL/PostProcessingBlock.h>
#include <VCELL/VariableStatisticsDataGenerator.h>
#include <VCELL/Variable.h>
#include <typeinfo>
#include <hdf5.h>
#include <iostream>
#include <fstream>
using std::cout;
using std::endl;
#include <sys/stat.h>

#define POST_PROCESSING_ROOT "/PostProcessing"

const char* PostProcessingHdf5Writer::PPGroupName  = POST_PROCESSING_ROOT;
const char* PostProcessingHdf5Writer::TimesDataSetName  = POST_PROCESSING_ROOT"/Times";

PostProcessingHdf5Writer::PostProcessingHdf5Writer(char* fileName, PostProcessingBlock* ppb)
{
	this->h5PPFileName = fileName;
	this->postProcessingBlock = ppb;
	h5PPFile = H5I_INVALID_HID;
	timesDataSet = H5I_INVALID_HID;
}

PostProcessingHdf5Writer::~PostProcessingHdf5Writer()
{
	H5Dclose(timesDataSet);
	H5Fclose(h5PPFile);
}

void PostProcessingHdf5Writer::createGroups()
{
	if (h5PPFile != H5I_INVALID_HID)
	{
		return;
	}
	hid_t attributeDataSpace = H5Screate(H5S_SCALAR);
	hid_t attributeStrType = H5Tcreate(H5T_STRING, sizeof(char) * 64);

	h5PPFile = H5Fcreate(h5PPFileName.c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

	// create post processing group /PostProcessing
	H5Gcreate(h5PPFile, PPGroupName, H5P_DEFAULT);

	// create /PostProcessing/Times
	int timesRank = 1;  // number of dimensions
	hsize_t timesDims = 10;
	hsize_t maxDims = H5S_UNLIMITED;
	hid_t timesDataSpace = H5Screate_simple(timesRank, &timesDims, &maxDims);
	
	// enable chunking
	int ndims = 1; // number of dimensions of each chunk
	hsize_t chunkDims = 500;  // dimensions of each chunk
	// data creation property list
	hid_t dcpl = H5Pcreate(H5P_DATASET_CREATE);
	H5Pset_chunk(dcpl, ndims, &chunkDims);

	//	double fill_val = -1;
	//	H5Pset_fill_value(dcpl, H5T_NATIVE_DOUBLE, &fill_val);
	// create dataset
	timesDataSet = H5Dcreate (h5PPFile, TimesDataSetName, H5T_NATIVE_DOUBLE, timesDataSpace, dcpl);

	// create a group for each data generator
	char dataGeneratorGroupName[128];
	vector<DataGenerator*>::iterator it;
	for(it = postProcessingBlock->dataGeneratorList.begin(); it < postProcessingBlock->dataGeneratorList.end(); ++ it)
	{
		DataGenerator* dataGenerator = *it;

		sprintf(dataGeneratorGroupName, "%s/%s", PPGroupName, dataGenerator->getQualifiedName().c_str());
		hid_t dataGeneratorGroup = H5Gcreate(h5PPFile, dataGeneratorGroupName, H5P_DEFAULT); 
		
		if (typeid(*dataGenerator) == typeid(VariableStatisticsDataGenerator))
		{
			// attributes : all the components, e.g. comp_0_name = varName, comp_0_unit = varUnit, comp_1_name = ... comp_1_unit = ...
			const vector<string>& compNames = ((VariableStatisticsDataGenerator*)dataGenerator)->getComponentNames();
			const vector<string>& compUnits = ((VariableStatisticsDataGenerator*)dataGenerator)->getComponentUnits();
			for (int i = 0; i < compNames.size(); i ++) {
				char attrName[64];
				char compName[64];
				char compUnit[64];

				sprintf(attrName, "comp_%d_name", i);
				hid_t attribute = H5Acreate(dataGeneratorGroup, attrName, attributeStrType, attributeDataSpace, H5P_DEFAULT);
				sprintf(compName, "%s", compNames[i].c_str());
				H5Awrite(attribute, attributeStrType, compName);
				H5Aclose(attribute);

				sprintf(attrName, "comp_%d_unit", i);
				attribute = H5Acreate(dataGeneratorGroup, attrName, attributeStrType, attributeDataSpace, H5P_DEFAULT);
				sprintf(compUnit, "%s", compUnits[i].c_str());
				H5Awrite(attribute, attributeStrType, compUnit);
				H5Aclose(attribute);
			}
		}
	}
}

void PostProcessingHdf5Writer::writeOutput()
{
	int timesRank = 1;
	hsize_t timesDims = 1;
	hsize_t maxDims = H5S_UNLIMITED;
	hid_t timesDataSpace = H5Screate_simple(timesRank, &timesDims, &maxDims);

	createGroups();

	// write current time
	double currTime = postProcessingBlock->simulation->getTime_sec();
	hsize_t size = timeList.size() + 1;
	H5Dset_extent(timesDataSet, &size);

	hsize_t count = 1;
	hsize_t offset = timeList.size();
	hid_t fspace = H5Dget_space(timesDataSet);
	H5Sselect_hyperslab(fspace, H5S_SELECT_SET, &offset, NULL, &count, NULL);
	H5Dwrite(timesDataSet, H5T_NATIVE_DOUBLE, timesDataSpace, fspace, H5P_DEFAULT, &currTime);

	timeList.push_back(currTime);

	int timeIndex = timeList.size() - 1;
	vector<DataGenerator*>::iterator it;
	for(it = postProcessingBlock->dataGeneratorList.begin(); it < postProcessingBlock->dataGeneratorList.end(); ++ it)
	{
		DataGenerator* dataGenerator = *it;
		dataGenerator->computePPData(postProcessingBlock->simulation);
		writeDataGenerator(dataGenerator, timeIndex);
	}

	H5Fflush(h5PPFile, H5F_SCOPE_GLOBAL);
}

void PostProcessingHdf5Writer::writeDataGenerator(DataGenerator* dataGenerator, int timeIndex)
{
	// create dataset /PostProcessing/VariableStatistics/time0
	char dataSetName[128];
	sprintf(dataSetName, "%s/%s/time%06d", PPGroupName, dataGenerator->getQualifiedName().c_str(), timeIndex);	

	// create data space
	hid_t dataspace = H5Screate_simple(dataGenerator->hdf5Rank, dataGenerator->hdf5Dims, NULL);
	hid_t dataSet = H5Dcreate (h5PPFile, dataSetName, H5T_NATIVE_DOUBLE, dataspace, H5P_DEFAULT);

	// write dataset
  H5Dwrite(dataSet, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dataGenerator->getData());
	// close dataset
	H5Dclose(dataSet);
	H5Sclose(dataspace);
}
