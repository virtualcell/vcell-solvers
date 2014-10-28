project(MovingBoundaryTest)

file(GLOB SRC_FILES *cpp) 
file(GLOB HDR_FILES *h) 
if (MSVC)
	set(VLD_INSTALL "C:\\Program Files (x86)\\Visual Leak Detector")
	option(VLD "Visual Leak Detector" OFF)
endif (MSVC)

#google test main
list(APPEND SRC_FILES ../${GTEST_VERSION}/src/gtest_main.cc)
set(EXE_FILE TestMovingBoundary)

set_source_files_properties( ${SRC_FILES} PROPERTIES LANGUAGE CXX)
include_directories(
	../ccamHDF5
	../ExpressionParser
	../Solver/include)
include_directories(SYSTEM 
	../FronTierLib 
	../FronTierLib/util 
	../boost_1_53_0
	../${GTEST_VERSION}/include
	${CMAKE_BINARY_DIR}/${HDF_VERSION} 
	../${HDF_VERSION}/src 
	../${HDF_VERSION}/c++/src)


add_executable(TestMovingBoundary ${SRC_FILES} ${HDR_FILES})
add_dependencies(TestMovingBoundary MovingBoundaryLib gtest frontier ExpressionParser HDF5)
target_link_libraries(TestMovingBoundary MovingBoundaryLib gtest frontier ExpressionParser hdf5_cpp)
install(TARGETS TestMovingBoundary 
	RUNTIME DESTINATION bin)

if (VLD)
#	add_definitions(VISUAL_LEAK_DETECTION)
	message("VLD activated")
	include_directories(${VLD_INSTALL}/include)
	target_link_libraries(TestMovingBoundary  ${VLD_INSTALL}/lib/Win64/vld.lib)
endif (VLD)
