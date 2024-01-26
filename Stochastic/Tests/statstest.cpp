//
// Created by Jim Schaff on 4/25/23.
//
#include <stdexcept>
#include "gtest/gtest.h"
#include "../VCellStoch/include/Gibson.h"
#include <iostream>
#include <cstdio>

#include "VCELL/SimulationMessaging.h"

const char* input_file_contents = R"INPUT_FILE(
<control>
STARTING_TIME	0.0
ENDING_TIME 	20.0
TOLERANCE 	1.0E-9
SAVE_PERIOD	0.02
MAX_SAVE_POINTS	1001.0
NUM_TRIAL	50000
SEED	1634997497
BMULTIBUTNOTHISTO	1
</control>

<model>
<discreteVariables>
TotalVars	3
s0_Count	1
s1_Count	0
s2_Count	1
</discreteVariables>

<jumpProcesses>
TotalProcesses	2
r0
r0_reverse
</jumpProcesses>

<processDesc>
TotalDescriptions	2
JumpProcess	r0
	Propensity	(0.9996443474639611 * s0_Count * s2_Count)
	Effect	3
		s0_Count	inc	-1.0

		s2_Count	inc	-1.0

		s1_Count	inc	1.0

	DependentProcesses	0

JumpProcess	r0_reverse
	Propensity	0.0
	Effect	3
		s0_Count	inc	1.0

		s2_Count	inc	1.0

		s1_Count	inc	-1.0

	DependentProcesses	1
		r0

</processDesc>
</model>
)INPUT_FILE";

/**
index   t	  (Var=s0_Count) s0_Count
0       0.0	    1.0
50      1.0	    0.36868
100     2.0	    0.1349
...
950     19.0	0.0
1000    20.0	0.0
 */

const std::map<int, double> expected_S0 = {
        {0, 1.0},       // t=0.0
        {50, 0.36868},  // t=1.0
        {100, 0.1349},  // t=2.0
        {150, 0.04972}, // t=3.0
        {200, 0.0187},  // t=4.0
        {250, 0.00678}, // t=5.0
        {300, 0.00234}, // t=6.0
        {350, 7.6E-4},  // t=7.0
        {400, 2.8E-4},  // t=8.0
        {450, 8.0E-5},  // t=9.0
        {500, 2.0E-5},  // t=10.0
        {550, 2.0E-5},  // t=11.0
        {600, 0.0},     // t=12.0
        {650, 0.0},     // t=13.0
        {700, 0.0},     // t=14.0
        {750, 0.0},     // t=15.0
        {800, 0.0},     // t=16.0
        {850, 0.0},     // t=17.0
        {900, 0.0},     // t=18.0
        {950, 0.0},     // t=19.0
        {1000, 0.0},    // t=20.0
};

TEST(statstest,test1) {
	std::string inputFileName = std::tmpnam(nullptr);
	std::string outputFileName = std::tmpnam(nullptr);
	std::map<int,double> results;
	std::fstream inputFileStream;
	inputFileStream.open(inputFileName, fstream::out);
	if (inputFileStream.fail()) {
		std::perror(("File <" + inputFileName + "> could not be created.").c_str());
		ASSERT_FALSE(inputFileStream.fail());
	}
	std::fstream outputFileStream;
	outputFileStream.open(outputFileName, fstream::out);
	if (outputFileStream.fail()) {
		std::perror(("File <" + inputFileName + "> could not be created.").c_str());
		ASSERT_FALSE(outputFileStream.fail());
	}

	// Setup the Gibson Solver input file
	if (outputFileStream.is_open()) outputFileStream.close();
	inputFileStream << input_file_contents;
	inputFileStream.close();

	// Create the Gibson Solver
    auto *gb = new Gibson(inputFileName.c_str(), outputFileName.c_str());

	// Launch the test
    gb->march();

	// Verify file contents
    outputFileStream.open(outputFileName, fstream::in);
    std::string line;
    std::getline(outputFileStream, line); // remove header line
	for (int i = 0; !outputFileStream.eof(); i++) {
		std::getline(outputFileStream, line);
		// if index found in expected_S0 map, store in results map
		if (expected_S0.find(i) != expected_S0.end()){
			float t, s0, s1, s2;
			// extract space separated values for t, s0, s1 and s2 from line
			std::stringstream line_stream(line);
			line_stream >> t >> s0 >> s1 >> s2;
			results[i] = s0;
		}
	}
	outputFileStream.close();

	// compare the expected and actual values
    double accumulatedError = 0.0, maxIndividualError = 0.0;
    for (auto const& expected : expected_S0){
        double absoluteError = std::abs(expected.second - results[expected.first]);
        // std::cout << "t=" << expected.first << " expected=" << expected.second << " computed=" << results[expected.first] << " abserr=" << abserr << std::endl;
        accumulatedError += absoluteError;
        maxIndividualError = std::max(maxIndividualError, absoluteError);
    }
    assert(accumulatedError < 0.015);
    assert(maxIndividualError < 0.005);

    delete gb;
    if (inputFileStream.is_open()) inputFileStream.close();
	if (outputFileStream.is_open()) outputFileStream.close();
}