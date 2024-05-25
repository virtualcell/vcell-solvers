project(MovingBoundarySolver)
cmake_minimum_required(VERSION 2.8)
cmake_policy(SET CMP0046 OLD) 

set(SUPPORTS_MATLAB OFF)
set(SUPPORTS_MOVING_BOUNDARY ON)

if (SUPPORTS_MATLAB)
	option(BUILD_INCLUDE_MATLAB_DIRS "Include matlab integration directory" OFF)
endif (SUPPORTS_MATLAB)

if (MINGW)
	add_definitions(-DMINGW)
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DMINGW")
endif (MINGW)

if(DEFINED CMAKE_BUILD_TYPE)
   SET(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE} CACHE STRING "Choose the type of build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel.")
else()
   SET(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel.")
endif()

if (UNIX)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -fpermissive")
  set(CXX_FLAGS "${CXX_FLAGS} -std=c++11 -fpermissive")
  add_definitions("-fPIC")
endif (UNIX) 

#if (${CMAKE_CROSSCOMPILING})
	set(CXX_FLAGS "${CXX_FLAGS} -std=c++11 -std=gnu++11 -fpermissive")
#endif()

if(MINGW)
    add_definitions(-U__STRICT_ANSI__)
    remove_definitions("-fPIC")
endif(MINGW)

message(STATUS "Operating system ${CMAKE_SYSTEM_NAME}")
message(STATUS "c++ flags ${CMAKE_CXX_FLAGS}")

find_package(Boost REQUIRED)
include_directories(${Boost_INCLUDE_DIRS})

OPTION(VARIABLE_SPECIES_STORAGE "DYNAMIC SPECIES STORAGE" OFF)
if (VARIABLE_SPECIES_STORAGE) 
	add_definitions(-DMB_VARY_MASS)
endif (VARIABLE_SPECIES_STORAGE) 

#glob to get full path of driver.cpp so REMOVE_ITEM works correctly
file(GLOB MOVINGBOUNDARY_SOLVER_MAIN Solver/src/driver.cpp)
file(GLOB MOVINGBOUNDARY_ALLOC_MAIN Solver/src/alloctestmain.cpp)
file(GLOB MOVINGBOUNDARY_SRC_FILES Solver/src/*cpp) 
list(REMOVE_ITEM MOVINGBOUNDARY_SRC_FILES ${MOVINGBOUNDARY_SOLVER_MAIN} ${MOVINGBOUNDARY_ALLOC_MAIN}) 

file(GLOB MOVINGBOUNDARY_HEADER_FILES Solver/include/*h Solver/include/*hpp Solver/include/vhdf5/*.h) 
file(GLOB MOVINGBOUNDARY_BRIDGE_HEADER_FILES Solver/include/MBridge/*h) 
set(EXE_FILE MovingBoundary)
if (ARCH_64bit) 
	set(EXE_FILE ${EXE_FILE}_x64)
endif()

set_source_files_properties( ${MOVINGBOUNDARY_SRC_FILES} PROPERTIES LANGUAGE CXX)
message(STATUS "HDF5 VERSION IS ${HDF5_VERSION}, LIBS = ${HDF5_LIBRARIES}, HL_LIBS = ${HDF5_HL_LIBRARIES}")
add_library(MovingBoundaryLib ${MOVINGBOUNDARY_SRC_FILES} ${MOVINGBOUNDARY_HEADER_FILES} ${MOVINGBOUNDARY_BRIDGE_HEADER_FILES})
target_link_libraries(MovingBoundaryLib frontier ExpressionParser vcommons ${HDF5_LIBRARIES} ${HDF5_HL_LIBRARIES} vcellmessaging)
target_include_directories(MovingBoundaryLib PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/Solver/include>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/Solver/include/MBridge>
  ${HDF5_INCLUDE_DIRS}
  $<INSTALL_INTERFACE:Solver/include>  # <prefix>/Solver/include
  $<INSTALL_INTERFACE:Solver/include/MBridge>  # <prefix>/Solver/include/MBridge
)


add_executable(${EXE_FILE} ${MOVINGBOUNDARY_SOLVER_MAIN} ${MOVINGBOUNDARY_HEADER_FILES} ${MOVINGBOUNDARY_BRIDGE_HEADER_FILES})
target_link_libraries(${EXE_FILE} MovingBoundaryLib)

#target_link_libraries(VCellFronTier ${DEP_LIBS})
#add_executable(AllocTest ${MOVINGBOUNDARY_ALLOC_MAIN})
#add_dependencies(AllocTest MovingBoundaryLib frontier ExpressionParser HDF5)
#target_link_libraries(AllocTest MovingBoundaryLib frontier ExpressionParser hdf5_cpp)

install(TARGETS MovingBoundaryLib ${EXE_FILE}
	ARCHIVE DESTINATION bin
	RUNTIME DESTINATION bin)

if (SUPPORTS_MOVING_BOUNDARY)
		find_file(SQL3 sqlite3.h)
		if (SQL3 MATCHES SQL3-NOTFOUND)
			message("Warning!!! sqlite3 not found in the system, using our own copy")
			include_directories(sqlite)
			set(SQL3 sqlite/sqlite3.h)
		endif (SQL3 MATCHES SQL3-NOTFOUND)
		
#	add_subdirectory(Tests)
endif (SUPPORTS_MOVING_BOUNDARY)
if (BUILD_INCLUDE_MATLAB_DIRS)
	message(STATUS "Matlab link directories active")
	add_subdirectory(matlab)
endif (BUILD_INCLUDE_MATLAB_DIRS)
enable_testing()
add_subdirectory(Tests)



