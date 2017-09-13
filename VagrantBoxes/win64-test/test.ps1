# TESTING
# don't add solvers to path (dependent libraries are in same directory as the solvers)
#

Write-Host "testing solvers using binaries and libraries in build-win64/bin and tests in testFiles"

$PROJECTDIR = "\vagrant_numerics"
$SOLVERDIR = $PROJECTDIR + "\build-win64\bin"
$TEST_DIR = $PROJECTDIR + "\testFiles"
$TEST_INPUT_DIR = $TEST_DIR + "\input"
$TEST_WORK_DIR = $TEST_DIR + "\working"
$TEST_ARCHIVE_DIR = $TEST_DIR + "\archive"
$TEST_ARCHIVE_WIN64_DIR = $TEST_ARCHIVE_DIR + "\win64"

Write-Host "solver dir is $SOLVER_DIR"
Write-Host "test dir is $TEST_DIR"
Write-Host "working dir is $TEST_WORK_DIR"
Write-Host "results win64 dir is $TEST_RESULTS_WIN64_DIR"

if(!(Test-Path $TEST_WORK_DIR)){
	mkdir $TEST_WORK_DIR
}
if(!(Test-Path $TEST_ARCHIVE_DIR)){
	mkdir $TEST_ARCHIVE_DIR
}
if(!(Test-Path $TEST_ARCHIVE_WIN64_DIR)){
	mkdir $TEST_ARCHIVE_WIN64_DIR
}

cd $PROJECTDIR

#
# Test $PROJECTDIR\build-win64\bin\FiniteVolume_x64.exe
#
$TEST_WORK_FINITEVOLUME_DIR = $TEST_WORK_DIR + "\FiniteVolume"
if(!(Test-Path $TEST_WORK_FINITEVOLUME_DIR)){
	mkdir $TEST_WORK_FINITEVOLUME_DIR
}
$CMD_DEL_WORK_FILES = "del " + $TEST_WORK_FINITEVOLUME_DIR + '\*'
$CMD_COPY_INPUT_FILES = "copy " + $TEST_INPUT_DIR + '\FiniteVolume\SimID_11538992_0_* ' + $TEST_WORK_DIR + '\FiniteVolume'
$CMD_SOLVE = $SOLVERDIR + '\FiniteVolume_x64 ' + $TEST_WORK_DIR + '\FiniteVolume\SimID_11538992_0_.fvinput'
Invoke-Expression  $CMD_DEL_WORK_FILES
Invoke-Expression  $CMD_COPY_INPUT_FILES
Invoke-Expression  $CMD_SOLVE

$TEST_ARCHIVE_WIN64_FINITEVOLUME_DIR = $TEST_ARCHIVE_WIN64_DIR + '\FiniteVolume'
if(!(Test-Path $TEST_ARCHIVE_WIN64_FINITEVOLUME_DIR)){
	mkdir $TEST_ARCHIVE_WIN64_FINITEVOLUME_DIR
}

$CMD_COPY_ALL_FILES = "copy " + $TEST_WORK_DIR + '\FiniteVolume\SimID_11538992_0_* ' + $TEST_ARCHIVE_WIN64_DIR + '\FiniteVolume'
$CMD_DEL_WORK_FILES = "del " + $TEST_WORK_FINITEVOLUME_DIR + '\*'
Invoke-Expression $CMD_COPY_ALL_FILES
Invoke-Expression $CMD_DEL_WORK_FILES

#
# Test $PROJECTDIR\build-win64\bin\MovingBoundary_x64.exe
#
$TEST_WORK_MOVINGBOUNDARY_DIR = $TEST_WORK_DIR + "\MovingBoundary"
if(!(Test-Path $TEST_WORK_MOVINGBOUNDARY_DIR)){
	mkdir $TEST_WORK_MOVINGBOUNDARY_DIR
}
$CMD_DEL_WORK_FILES = "del " + $TEST_WORK_MOVINGBOUNDARY_DIR + '\*'
$CMD_COPY_INPUT_FILES = "copy " + $TEST_INPUT_DIR + '\MovingBoundary\SimID_599489767_0_* ' + $TEST_WORK_DIR + '\MovingBoundary'
$CMD_SOLVE = $SOLVERDIR + '\MovingBoundary_x64 --config ' + $TEST_WORK_DIR + '\MovingBoundary\SimID_599489767_0_mb.xml'
Invoke-Expression  $CMD_DEL_WORK_FILES
Invoke-Expression  $CMD_COPY_INPUT_FILES
Invoke-Expression  $CMD_SOLVE

$TEST_ARCHIVE_WIN64_MOVINGBOUNDARY_DIR = $TEST_ARCHIVE_WIN64_DIR + '\MovingBoundary'
if(!(Test-Path $TEST_ARCHIVE_WIN64_MOVINGBOUNDARY_DIR)){
	mkdir $TEST_ARCHIVE_WIN64_MOVINGBOUNDARY_DIR
}

$CMD_COPY_ALL_FILES = "copy " + $TEST_WORK_DIR + '\MovingBoundary\SimID_599489767_0_* ' + $TEST_ARCHIVE_WIN64_DIR + '\MovingBoundary'
$CMD_DEL_WORK_FILES = "del " + $TEST_WORK_MOVINGBOUNDARY_DIR + '\*'
Invoke-Expression $CMD_COPY_ALL_FILES
Invoke-Expression $CMD_DEL_WORK_FILES

