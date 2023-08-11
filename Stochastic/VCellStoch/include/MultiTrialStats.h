//
// Created by Jim Schaff on 8/11/23.
//

#ifndef NUMERICS_MULTITRIALSTATS_H
#define NUMERICS_MULTITRIALSTATS_H
#include <vector>
using std::vector;

class MultiTrialStats {
public:
    MultiTrialStats(int numVars, int numTimePoints);
    void addSample(int timeIndex, double timeValue, double* varVals);
    double getMean(int varIndex, int timeIndex);
    void startNewTrial();
private:
    void init();
    int numVars;
    int numTimePoints;
    int currentTrial;
    vector<vector<double> > mean;
    vector<double> timeValues;
};


#endif //NUMERICS_MULTITRIALSTATS_H
