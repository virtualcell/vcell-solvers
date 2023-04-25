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
SEED	566564762
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


TEST(statstest,test1) {
    std::string temp_input_file_name = std::tmpnam(nullptr);
    std::ofstream input_file (temp_input_file_name);
    bool bWroteFile = false;
    if (input_file.is_open()){
        input_file << input_file_contents;
        input_file.close();
        bWroteFile = true;
    }
    ASSERT_TRUE(bWroteFile);

    std::string temp_output_file_name = std::tmpnam(nullptr);

    Gibson *gb= new Gibson(temp_input_file_name.c_str(), temp_output_file_name.c_str());
    gb->march();
    delete gb;
}