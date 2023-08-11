//
// Created by Jim Schaff on 8/11/23.
//

#include "MultiTrialStats.h"


MultiTrialStats::MultiTrialStats(int numVars, int numTimePoints) {
    this->numVars = numVars;
    this->numTimePoints = numTimePoints;
    currentTrial = -1;
    init();
}

void MultiTrialStats::init() {
    mean.resize(numTimePoints);
    for (int i = 0; i < numTimePoints; ++i) {
        mean[i].resize(numVars,0);
    }
}

void MultiTrialStats::addSample(int timeIndex, double timeValue, double *varVals) {
    if (timeValues.size() <= timeIndex){
        timeValues.push_back(timeValue);
    }
    for (int i = 0; i < numVars; ++i) {
        double delta = varVals[i] - mean[timeIndex].data()[i];
        mean[timeIndex].data()[i] += delta / (currentTrial + 1);
    }
}

double MultiTrialStats::getMean(int varIndex, int timeIndex) {
    return mean[timeIndex][varIndex];
}

void MultiTrialStats::startNewTrial() {
    currentTrial += 1;

}


