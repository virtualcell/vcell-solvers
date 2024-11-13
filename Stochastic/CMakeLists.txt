cmake_minimum_required (VERSION 3.0)
project(Stochastic)

set (SRC_FILES
		VCellStoch/src/Gibson.cpp
		VCellStoch/src/IndexedTree.cpp
		VCellStoch/src/Jump.cpp
		VCellStoch/src/StochModel.cpp
		VCellStoch/src/StochVar.cpp
        VCellStoch/src/MultiTrialStats.cpp
)

set (SRC_MAIN_FILE VCellStoch/src/VCellStoch.cpp)

set (HEADER_FILES
		VCellStoch/include/Gibson.h
		VCellStoch/include/IndexedTree.h
		VCellStoch/include/Jump.h
		VCellStoch/include/StochModel.h
		VCellStoch/include/StochVar.h
        VCellStoch/include/MultiTrialStats.h
)

set(EXE_FILE VCellStoch)
if (ARCH_64bit) 
	set(EXE_FILE ${EXE_FILE}_x64)
endif()


#set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
#include_directories(${JMS_INCS})
#add_definitions(${JMS_DEFS})
#set(DEP_LIBS ExpressionParser ${VCELLMESSAGING_LIBS})
#include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../ExpressionParser)
#include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../vcommons/include)
#include_directories(${VCELL_MESSAGING_INCLUDE_DIR})
if (APPLE)
	execute_process(COMMAND uname -m OUTPUT_VARIABLE PROCESSOR_ARCHITECTURE OUTPUT_STRIP_TRAILING_WHITESPACE)
	message("Processor Architecture: ${PROCESSOR_ARCHITECTURE}")
	if (PROCESSOR_ARCHITECTURE STREQUAL "arm64")
		link_directories("/opt/homebrew/lib")
	else ()
		link_directories("/usr/local/lib")
	endif()
endif()

# PETSC_ARCH is defined as ENV var in https://hub.docker.com/r/virtualcell/vcell-solvers/dockerfile basefile
# so we must be in the docker vcell-solvers environment if PETSC_ARCH is defined
if (DEFINED ENV{PETSC_ARCH} OR LINUX)
        include_directories(/usr/include/hdf5/serial)
        link_directories("/usr/lib/x86_64-linux-gnu/hdf5/serial")
endif()

add_library(VCellStochLib ${SRC_FILES} ${HEADER_FILES})
target_link_libraries(VCellStochLib ExpressionParser vcommons vcellmessaging hdf5)
target_include_directories(VCellStochLib PUBLIC
		$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/VCellStoch/include>
		$<INSTALL_INTERFACE:VCellStoch/include>  # <prefix>/VCellStoch/include
		)


add_executable(${EXE_FILE} ${SRC_MAIN_FILE} ${SRC_FILES} ${HEADER_FILES})
#target_link_libraries(${EXE_FILE} ExpressionParser vcommons vcellmessaging ${CMAKE_SOURCE_DIR}/mylibs/libhdf5_serial.so.10)
target_link_libraries(${EXE_FILE} VCellStochLib)
#add_dependencies(${EXE_FILE} ExpressionParser)
install(TARGETS VCellStochLib ${EXE_FILE} RUNTIME DESTINATION ${OPTION_EXE_DIRECTORY})

enable_testing()
add_subdirectory(Tests)
