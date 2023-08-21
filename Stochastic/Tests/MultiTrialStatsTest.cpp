//
// Created by Jim Schaff on 4/25/23.
//
#include <stdexcept>
#include "gtest/gtest.h"
#include "MultiTrialStats.h"
#include <random>


TEST(multitrialstats_test, test1) {
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
    ASSERT_NEAR(stats.getTimePoint(0), 1.0, 1.0E-12);
    ASSERT_NEAR(stats.getTimePoint(1), 2.0, 1.0E-12);
    int n = 2;
    double epsilon = 1.0E-12;
    double mu_0_0 = (1.0+10.0)/2;
    double mu_1_0 = (2.0+20.0)/2;
    double mu_0_1 = (3.0+40.0)/2;
    double mu_1_1 = (4.0+30.0)/2;
    ASSERT_NEAR(stats.getMean(0,0), mu_0_0, epsilon);
    ASSERT_NEAR(stats.getMean(1,0), mu_1_0, epsilon);
    ASSERT_NEAR(stats.getMean(0,1), mu_0_1, epsilon);
    ASSERT_NEAR(stats.getMean(1,1), mu_1_1, epsilon);
    double var_0_0 = (pow(1-mu_0_0,2) + pow(10-mu_0_0,2))/(n-1);
    double var_1_0 = (pow(2-mu_1_0,2) + pow(20-mu_1_0,2))/(n-1);
    double var_0_1 = (pow(3-mu_0_1,2) + pow(40-mu_0_1,2))/(n-1);
    double var_1_1 = (pow(4-mu_1_1,2) + pow(30-mu_1_1,2))/(n-1);
    ASSERT_NEAR(stats.getVariance(0,0), var_0_0, epsilon);
    ASSERT_NEAR(stats.getVariance(1,0), var_1_0, epsilon);
    ASSERT_NEAR(stats.getVariance(0,1), var_0_1, epsilon);
    ASSERT_NEAR(stats.getVariance(1,1), var_1_1, epsilon);
}

TEST(multitrialstats_test, testGaussian) {
    double expected_mean = 100.0;
    double expected_stddev = 1.77;
    std::default_random_engine generator(1634997497);
    std::normal_distribution<double> distribution(expected_mean,expected_stddev);

    MultiTrialStats stats(1,1);
    int NUM_TRIALS = 100000;
    double sample_min = 1.0E99;
    double sample_max = -1.0E99;
    for (int i=0; i<NUM_TRIALS; i++) {
        double sample = distribution(generator);
        sample_min = std::min(sample_min, sample);
        sample_max = std::max(sample_max, sample);
        double varVals_1[1] = {sample};
        stats.startNewTrial();
        stats.addSample(0, 0.0, varVals_1);
    }
    double z_score_99999 = 4.417173; // 99.999% confidence
    double epsilon_mean = z_score_99999 * expected_stddev / sqrt(NUM_TRIALS);
    ASSERT_NEAR(stats.getMean(0,0), expected_mean, epsilon_mean);
    double epsilon_variance = 0.05;
    ASSERT_NEAR(stats.getVariance(0,0), expected_stddev*expected_stddev, epsilon_variance);
    ASSERT_NEAR(stats.getMin(0,0), sample_min, 1e-12);
    ASSERT_NEAR(stats.getMax(0,0), sample_max, 1e-12);
}
