project(MovingBoundarySolver)

OPTION(VARIABLE_SPECIES_STORAGE "DYNAMIC SPECIES STORAGE" OFF)
if (VARIABLE_SPECIES_STORAGE) 
	add_definitions(-DMB_VARY_MASS)
endif (VARIABLE_SPECIES_STORAGE) 

#glob to get full path of driver.cpp so REMOVE_ITEM works correctly
file(GLOB SOLVER_MAIN src/driver.cpp)
file(GLOB ALLOC_MAIN src/alloctestmain.cpp)
file(GLOB SRC_FILES src/*cpp) 
list(REMOVE_ITEM SRC_FILES ${SOLVER_MAIN} ${ALLOC_MAIN}) 

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
include_directories(../vcommons/include)

add_library(MovingBoundaryLib ${SRC_FILES} ${HEADER_FILES} ${BRIDGE_HEADER_FILES})
add_dependencies(MovingBoundaryLib FronTierLib ExpressionParser)


add_executable(MovingBoundary ${SOLVER_MAIN})
add_dependencies(MovingBoundary MovingBoundaryLib frontier ExpressionParser HDF5 vcommons)
target_link_libraries(MovingBoundary MovingBoundaryLib frontier ExpressionParser hdf5_cpp vcommons)
#target_link_libraries(VCellFronTier ${DEP_LIBS})
add_executable(AllocTest ${ALLOC_MAIN})
add_dependencies(AllocTest MovingBoundaryLib frontier ExpressionParser HDF5)
target_link_libraries(AllocTest MovingBoundaryLib frontier ExpressionParser hdf5_cpp)

install(TARGETS MovingBoundaryLib MovingBoundary AllocTest
	ARCHIVE DESTINATION bin
	RUNTIME DESTINATION bin)
