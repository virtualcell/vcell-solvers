//
// Created by Jim Schaff on 4/25/23.
//
#include <stdexcept>
#include "gtest/gtest.h"
#include "../VCellStoch/include/Gibson.h"
#include <iostream>
#include <cstdio>

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
        {0, 0.0},       // t=0.0
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
    char *temp_input_file_name = new char[200] {0};
    char *temp_output_file_name = new char[200] {0};
    std::string in_prefix = testing::TempDir() + "input_XXXXXX";
    std::string out_prefix = testing::TempDir() + "output_XXXXXX";
    strncpy(temp_input_file_name, in_prefix.c_str(), in_prefix.length());
    strncpy(temp_output_file_name, out_prefix.c_str(), out_prefix.length());
    assert(mkstemp(temp_input_file_name) != -1);
    assert(mkstemp(temp_output_file_name) != -1);
    std::ofstream input_file (temp_input_file_name);
    bool bWroteFile = false;
    if (input_file.is_open()){
        input_file << input_file_contents;
        input_file.close();
        bWroteFile = true;
    }
    ASSERT_TRUE(bWroteFile);

    Gibson *gb= new Gibson(temp_input_file_name, temp_output_file_name);
    gb->march();
    delete gb;

    // verify file contents
    std::ifstream outfile(temp_output_file_name);
    const int num_timepoints = 1/0.02 + 1;
    string line;
    getline(outfile, line); // remove header line
    std::cout << line << std::endl;
    std::cout.flush();

    std::map<int,double> results;
    for (int i=0; i<num_timepoints; i++){

        getline(outfile, line);
        std::cout << line << std::endl;
        std::cout.flush();

        // if index found in map, compare and accumulate error
        if (expected_S0.find(i) != expected_S0.end()){
            float t, s0, s1, s2;
            // extract space separated values for t, s0, s1 and s2 from line
            std::stringstream line_stream(line);
            line_stream >> t >> s0 >> s1 >> s2;
            results[i] = s0;
        }
    }
    // compare the expected and actual values
    double accum_error = 0.0;
    double max_error = 0.0;
    for (auto const& expected : expected_S0){
        double s0_given = expected.second;
        double s0_computed = results[expected.first];
        double abserr = std::abs(s0_given - s0_computed);
        accum_error += abserr;
        max_error = std::max(max_error, abserr);
    }
    assert(accum_error < 10.0);  // ridiculously large error just to test execution.

    delete temp_input_file_name;
    delete temp_output_file_name;
}