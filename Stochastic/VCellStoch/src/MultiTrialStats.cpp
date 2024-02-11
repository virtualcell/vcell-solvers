//
// Created by Jim Schaff on 8/11/23.
//

#include "MultiTrialStats.h"
#include <vector>
#include <iostream>
#include <limits>
#include <math.h>

#ifdef __APPLE__
#include "/usr/local/opt/hdf5/include/hdf5.h"
#else
#include <hdf5.h>
#endif

using std::vector;
using std::string;


MultiTrialStats::MultiTrialStats(int numVars, int numTimePoints) {
    this->numVars = numVars;
    this->numTimePoints = numTimePoints;
    currentTrial = 0;
    init();
}

void MultiTrialStats::init() {
    mean.resize(numTimePoints);
    M2.resize(numTimePoints);
    variance.resize(numTimePoints);
    statMin.resize(numTimePoints);
    statMax.resize(numTimePoints);
    for (int i = 0; i < numTimePoints; ++i) {
        mean[i].resize(numVars,0);
        M2[i].resize(numVars,0);
        variance[i].resize(numVars,0);
        statMin[i].resize(numVars,std::numeric_limits<double>::max());
        statMax[i].resize(numVars,std::numeric_limits<double>::min());
    }
}

void MultiTrialStats::addSample(int timeIndex, double timeValue, double *varVals) {
//    std::cout << "addSample(timeIndex: " << timeIndex << " timeValue: " << timeValue << " varVals: ";
//    for (int i = 0; i < numVars; ++i) {
//        std::cout << varVals[i] << " ";
//    }
//    std::cout << ")" << std::endl;
//    std::cout.flush();
    if (timeValues.size() <= timeIndex){
        timeValues.push_back(timeValue);
    }
    for (int i = 0; i < numVars; ++i) {
        double currValue = varVals[i];
        double delta = currValue - mean[timeIndex][i];
        mean[timeIndex][i] += delta / (currentTrial);
        M2[timeIndex][i] += delta * (currValue - mean[timeIndex][i]);
        variance[timeIndex][i] = M2[timeIndex][i] / (currentTrial-1);
        statMin[timeIndex][i] = std::min(currValue, statMin[timeIndex][i]) ;
        statMax[timeIndex][i] = std::max(currValue, statMax[timeIndex][i]) ;
    }
}

double MultiTrialStats::getMean(int varIndex, int timeIndex) {
    return mean[timeIndex][varIndex];
}

double MultiTrialStats::getVariance(int varIndex, int timeIndex) {
    return variance[timeIndex][varIndex];
}

double MultiTrialStats::getMin(int varIndex, int timeIndex) {
    return statMin[timeIndex][varIndex];
}

double MultiTrialStats::getMax(int varIndex, int timeIndex) {
    return statMax[timeIndex][varIndex];
}

void MultiTrialStats::startNewTrial() {
    currentTrial += 1;
//    std::cout << std::endl << "startNewTrial(currentTrial: " << currentTrial << ")" << std::endl;
//    std::cout.flush();
}

void MultiTrialStats::writeHDF5(string outfilename, vector<string> listOfVarNames){
    //
    //Create HDF5 file
    //
    string ofhdf5(outfilename);
    ofhdf5.append("_hdf5");
    try{
        hid_t file; //file handle
        file = H5Fcreate(ofhdf5.c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

        hid_t dataspace,datatype,dataset; /* general data structure handles */
        herr_t status;
        hid_t doubleDataType;
        doubleDataType = H5Tcopy (H5T_NATIVE_DOUBLE);

        //
        //Save varnames to hdf5 file
        //
        hid_t varLenStr;   /* variable length string datatype */
        int rank = 1; //num dimensions
        hsize_t varNamesDim[rank]; /*container for size of VarNames  1-d array */
        string varName("VarNames");


        varNamesDim[0] = listOfVarNames.size();//set size of dims in container
        dataspace = H5Screate_simple(rank, varNamesDim, NULL);
        varLenStr = H5Tcopy (H5T_C_S1);
        H5Tset_size (varLenStr, H5T_VARIABLE);
        dataset = H5Dcreate1(file, varName.c_str(), varLenStr, dataspace, H5P_DEFAULT);
        //For variable-names, create vector of pointers to c-style strings
        std::vector<const char*> chars;
        for (int i=0;i < listOfVarNames.size();i++) {
            chars.push_back(listOfVarNames[i].c_str());
        }
        status = H5Dwrite(dataset, varLenStr, H5S_ALL, H5S_ALL, H5P_DEFAULT, chars.data());

        H5Sclose(dataspace);
        H5Tclose(varLenStr);
        H5Dclose(dataset);

        //
        //Save times
        //
        rank = 1;
        hsize_t timesDim[rank];
        string timeName("SimTimes");


        timesDim[0] = timeValues.size();
        dataspace = H5Screate_simple(rank, timesDim, NULL);
        dataset = H5Dcreate1(file, timeName.c_str(), doubleDataType, dataspace, H5P_DEFAULT);
        status = H5Dwrite(dataset, doubleDataType, H5S_ALL, H5S_ALL, H5P_DEFAULT, timeValues.data());

        H5Sclose(dataspace);
        //H5Tclose(varLenStr);
        H5Dclose(dataset);

        //
        //Save Stats for all times and variables
        //
        rank = 2;
        hsize_t meanDim[rank];
        meanDim[0] = timeValues.size();
        meanDim[1] = listOfVarNames.size();
        dataspace = H5Screate_simple(rank, meanDim, NULL);
        string statsNames[4];
        int varianceIndex = 3;
        statsNames[0]="StatMean";
        statsNames[1]="StatMin";
        statsNames[2]="StatMax";
        statsNames[varianceIndex]="StatStdDev";//converted to stddev during write to file
        vector< vector<double> > statTypes[4];
        statTypes[0] = mean;
        statTypes[1] = statMin;
        statTypes[2] = statMax;
        statTypes[varianceIndex] = variance;
        for (int statIndex=0; statIndex < 4; statIndex++) {
            dataset = H5Dcreate1(file, statsNames[statIndex].c_str(), doubleDataType, dataspace, H5P_DEFAULT);
            double allData[meanDim[0]][meanDim[1]];
            for (int timeIndex = 0; timeIndex < meanDim[0]; ++timeIndex) {
                for (int varIndex = 0; varIndex < meanDim[1]; ++varIndex) {
                    allData[timeIndex][varIndex] = statTypes[statIndex][timeIndex][varIndex];
                    if(statIndex == varianceIndex){//turn variance into stddev
                        allData[timeIndex][varIndex] = sqrt(allData[timeIndex][varIndex]);
                    }
                }
            }
            status = H5Dwrite(dataset, doubleDataType, H5S_ALL, H5S_ALL, H5P_DEFAULT, allData);
            H5Dclose(dataset);
        }
        H5Sclose(dataspace);

        H5Fclose(file);
    } catch (const std::exception& ex) {
        std::cout << " Error writing HDF5 file " << ofhdf5 << " " << ex.what() << "'\n";
    } catch (const std::string& ex) {
        std::cout << " Error writing HDF5 file " << ofhdf5 << " " << ex << "'\n";
    } catch (...) {
        std::cout << " Error writing HDF5 file " << ofhdf5 << "" << "unknown exception" << "'\n";
    }

}


