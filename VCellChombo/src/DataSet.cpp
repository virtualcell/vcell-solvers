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
#include <vector>
using std::cout;
using std::endl;
using std::string;
#include <VCELL/VolumeVariable.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/ChomboGeometry.h>
#include <hdf5.h>
#include <H5Tpublic.h>

#include "VCELL/VCellModel.h"

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

void DataSet::write(SimulationExpression *sim, char* filename)
{
  vector<Variable*> allVarList;
	// sim variables
	for (int i = 0; i < sim->getNumVariables(); ++ i)
	{
		allVarList.push_back(sim->getVariable(i));
	}
	// IF variables
	for (int i = 0; i < sim->getChomboGeometry()->getNumSubdomains(); ++ i)
	{
		allVarList.push_back(sim->getChomboGeometry()->getChomboIF(i)->getFeature()->getIFVariable());
	}
	if (allVarList.size() <= 0)
	{
		cout << "DataSet::write() - no variables defined" << endl;
	}
	
	static const char* SOLUTION_GROUP = "/solution";
	static const char* SOLUTION_ATTR_TIME = "time";
	static const char* SOLUTION_ATTR_VARIABLES = "variables";
	static const char* SOLUTION_ATTR_VARIABLE_TYPES = "variable types";
	
	static const char* SOLUTION_DATASET_ATTR_DOMAIN = "domain";
	static const char* DATASET_ATTR_VARIABLE_TYPE = "variable type";
	static const char* SOLUTION_DATASET_ATTR_MEAN = "mean";
	static const char* SOLUTION_DATASET_ATTR_SUM_VOLFRAC = "sum of volume fraction";
	static const char* SOLUTION_DATASET_ATTR_RELATIVE_L2ERROR = "relative L2 error";
	static const char* SOLUTION_DATASET_ATTR_MAX_ERROR = "max error";

	static const char* EXTRAPOLATED_VOLUMES_GROUP = "/extrapolated_volumes";

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
	
	for (int i = 0; i < allVarList.size(); i ++)
	{
		Variable* var = allVarList[i];
		dim[0] = var->getSize();   /* Dataspace dimensions */
		space = H5Screate_simple(rank, dim, NULL);
		char dsName[128];
		sprintf(dsName, "%s/%s", SOLUTION_GROUP, var->getName().c_str());
		hid_t varDataset = H5Dcreate (h5SimFile, dsName, H5T_NATIVE_DOUBLE, space, H5P_DEFAULT);
		H5Dwrite(varDataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, var->getCurr());

		// attribute: domain
		hid_t strtype = H5Tcopy(H5T_C_S1);
		char domainName[50];
		if (var->getStructure() != NULL)
		{
			sprintf(domainName, "%s", var->getStructure()->getName().c_str());
			H5Tset_size(strtype, strlen(domainName));
			hid_t attribute = H5Acreate(varDataset, SOLUTION_DATASET_ATTR_DOMAIN, strtype, scalarDataSpace, H5P_DEFAULT);
			H5Awrite(attribute, strtype, domainName);
			H5Aclose(attribute);
		}
		
		// attribute: variable type
		attribute = H5Acreate(varDataset, DATASET_ATTR_VARIABLE_TYPE, H5T_NATIVE_INT, scalarDataSpace, H5P_DEFAULT);
		VariableType varType = var->getVarType();
		H5Awrite(attribute, H5T_NATIVE_INT, &varType);
		H5Aclose(attribute);
		
		// attribute: mean
		attribute = H5Acreate(varDataset, SOLUTION_DATASET_ATTR_MEAN, H5T_NATIVE_DOUBLE, scalarDataSpace, H5P_DEFAULT);
		double d = var->getMean();
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
			if (var->getStructure() != NULL)
			{
				hid_t attribute = H5Acreate(errorVarDataSet, SOLUTION_DATASET_ATTR_DOMAIN, strtype, scalarDataSpace, H5P_DEFAULT);
				H5Awrite(attribute, strtype, domainName);
				H5Aclose(attribute);
			}
			
			// attribute: variable type
			attribute = H5Acreate(errorVarDataSet, DATASET_ATTR_VARIABLE_TYPE, H5T_NATIVE_INT, scalarDataSpace, H5P_DEFAULT);
			H5Awrite(attribute, H5T_NATIVE_INT, &varType);
			H5Aclose(attribute);

			H5Dclose(errorVarDataSet);

			// relative error variable
			errVar = var->getRelativeErrorVariable();
			sprintf(dsName, "%s/%s", SOLUTION_GROUP, errVar->getName().c_str());
			errorVarDataSet = H5Dcreate (h5SimFile, dsName, H5T_NATIVE_DOUBLE, space, H5P_DEFAULT);
			H5Dwrite(errorVarDataSet, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, errVar->getCurr());

			if (var->getStructure() != NULL)
			{
				// attribute: domain
				attribute = H5Acreate(errorVarDataSet, SOLUTION_DATASET_ATTR_DOMAIN, strtype, scalarDataSpace, H5P_DEFAULT);
				H5Awrite(attribute, strtype, domainName);
				H5Aclose(attribute);
			}

			// attribute: variable type
			attribute = H5Acreate(errorVarDataSet, DATASET_ATTR_VARIABLE_TYPE, H5T_NATIVE_INT, scalarDataSpace, H5P_DEFAULT);
			H5Awrite(attribute, H5T_NATIVE_INT, &varType);
			H5Aclose(attribute);

			H5Dclose(errorVarDataSet);
		}
		H5Dclose(varDataset);
		H5Tclose(strtype);
		H5Sclose(space);
	}
	H5Gclose(solGroup);
	
	if (SimTool::getInstance()->getModel()->getNumMembranes() > 0)
	{
		hid_t extrapolatedVolumesGroup = H5Gcreate(h5SimFile, EXTRAPOLATED_VOLUMES_GROUP, H5P_DEFAULT);
		int numVolVar = sim->getNumVolVariables();
		for (int i = 0; i < numVolVar; i ++)
		{
			VolumeVariable* volVar = sim->getVolVariable(i);
			dim[0] = volVar->getExtrapolatedSize();
			if (dim[0] > 0)
			{
				space = H5Screate_simple(rank, dim, NULL);
				char dsName[128];
				sprintf(dsName, "%s/__%s_extrapolated__", EXTRAPOLATED_VOLUMES_GROUP, volVar->getName().c_str());
				hid_t varDataset = H5Dcreate (h5SimFile, dsName, H5T_NATIVE_DOUBLE, space, H5P_DEFAULT);
				H5Dwrite(varDataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, volVar->getExtrapolated());

				// attribute: variable type
				attribute = H5Acreate(varDataset, DATASET_ATTR_VARIABLE_TYPE, H5T_NATIVE_INT, scalarDataSpace, H5P_DEFAULT);
				VariableType varType = VAR_MEMBRANE;
				H5Awrite(attribute, H5T_NATIVE_INT, &varType);
				H5Aclose(attribute);

				H5Dclose(varDataset);
				H5Sclose(space);
			}
		}
		H5Gclose(extrapolatedVolumesGroup);
	}
	
	H5Sclose(scalarDataSpace);
	H5Fclose(h5SimFile);
}

