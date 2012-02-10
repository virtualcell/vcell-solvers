/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef POST_PROCESSING_HDF5_WRITER_H
#define POST_PROCESSING_HDF5_WRITER_H

#include <VCELL/SimulationExpression.h>
#include <SymbolTable.h>
#include <vector>
using std::vector;

class DataGenerator;
class PostProcessingBlock;
namespace H5 {
	class H5File;
	class DataSet;
}

class PostProcessingHdf5Writer
{
public:
	PostProcessingHdf5Writer(char* h5PPFileName, PostProcessingBlock* postProcessingBlock);
	virtual ~PostProcessingHdf5Writer();

	void writeOutput();

	static const char* PPGroupName;
	static const char* TimesDataSetName;

private:
	vector<double> timeList;
	PostProcessingBlock* postProcessingBlock;

	H5::H5File* h5PPFile;
	bool bFirstTime;
	H5::DataSet* timesDataSet;
	void writeDataSet(DataGenerator* dataGenerator, int timeIndex);
};

#endif
