#!/usr/bin/env python3

import os
import posixpath
import subprocess
import sys
from pathlib import Path


# function to numerically compare the contents of two text files to determine approximate equality
# each line is a space delimited list of numbers, compare them to within 8 significant figures
def compare_files(file1: Path, file2: Path, tolerance: float):
    with open(file1, 'r') as f1, open(file2, 'r') as f2:
        for line1, line2 in zip(f1, f2):
            if (line1 := line1.strip()) == (line2 := line2.strip()):
                continue
            array1 = [float(x) for x in line1.split()]
            array2 = [float(x) for x in line2.split()]
            for x, y in zip(array1, array2):
                if x != y and abs(x - y) > tolerance * max(abs(x), abs(y)):
                    return False
    return True


# get the directory of this script
test_dir = os.path.dirname(os.path.realpath(__file__))
# in the path replace \ with /, D:\ with /d/
test_dir = test_dir.replace("\\", "/")
# tell os.path.join to use / as the path separator
os.path.sep = "/"
exe = sys.argv[1]

print(f"test_dir: {test_dir}")
print(f"exe: {exe}")

input_file = posixpath.join(test_dir, "SimID_1489333437_0_.cvodeInput")
output_file = posixpath.join(test_dir, "SimID_1489333437_0_.ida")
expected_output_file = posixpath.join(test_dir, "SimID_1489333437_0_.ida.expected")

if not posixpath.exists(exe):
    print(f"SundialsSolverStandalone_x64 executable {exe} not found. Exiting...")
    sys.exit(1)

if not posixpath.exists(input_file):
    print(f"Input file {input_file} not found. Exiting...")
    sys.exit(1)

if not posixpath.exists(expected_output_file):
    print(f"Expected output file {expected_output_file} not found. Exiting...")
    sys.exit(1)

command = [exe, input_file, output_file]
print(" ".join(command))

try:
    subprocess.check_call(command)
except subprocess.CalledProcessError:
    print("SundialsSolverStandalone_x64 failed to run. Exiting...")
    sys.exit(1)

# verify that the output files exist
if not os.path.isfile(output_file):
    print(f"Output file {output_file} not found. Exiting...")
    sys.exit(1)

# verify that the output files match the expected output files
if not compare_files(file1=Path(output_file), file2=Path(expected_output_file), tolerance=1e-8):
    print(f"Output file {output_file} does not match expected output {expected_output_file}. Exiting...")
    sys.exit(1)

print("SundialsSolverStandalone_x64 solver completed and solution matched expected output. Exiting...")
sys.exit(0)
