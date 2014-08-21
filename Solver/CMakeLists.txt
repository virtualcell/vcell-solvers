project(MovingBoundarySolver)

#glob to get full path of driver.cpp so REMOVE_ITEM works correctly
file(GLOB SOLVER_MAIN src/driver.cpp)
file(GLOB SRC_FILES src/*cpp) 
list(REMOVE_ITEM SRC_FILES ${SOLVER_MAIN}) 

file(GLOB HEADER_FILES include/*h include/*hpp) 
file(GLOB BRIDGE_HEADER_FILES include/MBridge/*h) 
set(EXE_FILE MovingBoundary)

set_source_files_properties( ${SRC_FILES} PROPERTIES LANGUAGE CXX)
include_directories(include 
	../ccamHDF5
	../ExpressionParser)
include_directories(SYSTEM 
	../FronTierLib 
	../FronTierLib/util 
	../boost_1_53_0
	${CMAKE_BINARY_DIR}/${HDF_VERSION} 
	../${HDF_VERSION}/src 
	../${HDF_VERSION}/c++/src)

add_library(MovingBoundaryLib ${SRC_FILES} ${HEADER_FILES} ${BRIDGE_HEADER_FILES})
add_dependencies(MovingBoundaryLib FronTierLib ExpressionParser)


add_executable(MovingBoundary ${SOLVER_MAIN})
add_dependencies(MovingBoundary MovingBoundaryLib frontier ExpressionParser HDF5)
target_link_libraries(MovingBoundary MovingBoundaryLib frontier ExpressionParser hdf5_cpp)
#target_link_libraries(VCellFronTier ${DEP_LIBS})
install(TARGETS MovingBoundaryLib MovingBoundary 
	ARCHIVE DESTINATION bin
	RUNTIME DESTINATION bin)
