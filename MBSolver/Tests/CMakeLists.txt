project(MovingBoundaryTest)
cmake_policy(SET CMP0046 OLD)
enable_testing()

set(SRC_FILES
	algotest.cpp
	alloctest.cpp
	biotest.cpp
	boosttest.cpp
	chronotest.cpp
	clippertest.cpp
	compiletest.cpp
	distancetest.cpp
	example_2d.cpp
	exptest.cpp
	fronttest.cpp
	ftest.cpp
	hdf5test.cpp
	insidetest.cpp
	logtest.cpp
	loopiteratortest.cpp
	meshelementnodetest.cpp
#	meshtest.cpp
#	mockpoint.inc
	movingboundarysetuptest.cpp
	numerictest.cpp
	segmenttest.cpp
	slopetest.cpp
	v64.cpp
	vcellpersist.cpp
	vcellutil.cpp
	vdemo.cpp
	vectortest.cpp
        vi64.cpp
	voltest.cpp
	worldtest.cpp
)

file(GLOB HDR_FILES *h)

set(EXE_FILE TestMovingBoundary)

set_source_files_properties( ${SRC_FILES} PROPERTIES LANGUAGE CXX)
#include_directories(
#	../ccamHDF5
#	../ExpressionParser
#	../Solver/include)
#include_directories(SYSTEM
#	../FronTierLib
#	../FronTierLib/util
#	../boost_1_53_0
#	../${GTEST_VERSION}/include
#	${CMAKE_BINARY_DIR}/${HDF_VERSION}
#	../${HDF_VERSION}/src
#	../${HDF_VERSION}/c++/src)
#include_directories(../vcommons/include)

message(STATUS "HDF5 VERSION IS ${HDF5_VERSION}, LIBS = ${HDF5_LIBRARIES}, HL_LIBS = ${HDF5_HL_LIBRARIES}")

add_executable(TestMovingBoundary ${SRC_FILES} ${HDR_FILES})
add_dependencies(TestMovingBoundary MovingBoundaryLib gtest frontier ExpressionParser HDF5 vcommons )
target_link_libraries(TestMovingBoundary MovingBoundaryLib GTest::gtest_main frontier ExpressionParser ${HDF5_LIBRARIES} ${HDF5_HL_LIBRARIES} vcommons)
install(TARGETS TestMovingBoundary 
	RUNTIME DESTINATION bin)

if (VLD)
#	add_definitions(VISUAL_LEAK_DETECTION)
	message("VLD activated")
	include_directories(${VLD_INSTALL}/include)
	target_link_libraries(TestMovingBoundary  ${VLD_INSTALL}/lib/Win64/vld.lib)
endif (VLD)




include(GoogleTest)
gtest_discover_tests(TestMovingBoundary)