//
// Created by Jim Schaff on 4/25/23.
//
#include <stdexcept>
#include "gtest/gtest.h"
#include "MultiTrialStats.h"

TEST(multitrialstats,test1) {
    MultiTrialStats stats(2,2);
    {
        double varVals_1[2] = {1, 2};
        double varVals_2[2] = {3, 4};
        stats.startNewTrial();
        stats.addSample(0, 1.0, varVals_1);
        stats.addSample(1, 2.0, varVals_2);
    }
    {
        double varVals_1[2] = {10, 20};
        double varVals_2[2] = {40, 30};
        stats.startNewTrial();
        stats.addSample(0, 1.0, varVals_1);
        stats.addSample(1, 2.0, varVals_2);
    }
    double epsilon = 1.0E-9;
    assert(std::abs(stats.getMean(0,0) - 11.0/2) < epsilon);
    assert(std::abs(stats.getMean(1,0) - 22.0/2) < epsilon);
    assert(std::abs(stats.getMean(0,1) - 43.0/2) < epsilon);
    assert(std::abs(stats.getMean(1,1) - 34.0/2) < epsilon);
}