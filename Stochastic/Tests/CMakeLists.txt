cmake_minimum_required (VERSION 3.0)
project(MovingBoundaryTest)
set (CMAKE_CXX_STANDARD 11)
cmake_policy(SET CMP0046 OLD)
enable_testing()

set(SRC_FILES
		statstest.cpp
)

file(GLOB HDR_FILES *h)

set(EXE_FILE Test TestVCellStoch)

set_source_files_properties( ${SRC_FILES} PROPERTIES LANGUAGE CXX)

message(STATUS "HDF5 VERSION IS ${HDF5_VERSION}, LIBS = ${HDF5_LIBRARIES}, HL_LIBS = ${HDF5_HL_LIBRARIES}")

add_executable(TestVCellStoch ${SRC_FILES} ${HDR_FILES})
add_dependencies(TestVCellStoch VCellStochLib gtest)
target_link_libraries(TestVCellStoch VCellStochLib GTest::gtest_main)
install(TARGETS TestVCellStoch
	RUNTIME DESTINATION bin)

include(GoogleTest)
gtest_discover_tests(TestVCellStoch)