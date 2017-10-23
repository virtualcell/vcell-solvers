#############################################
#
#  Build 64bit binaries on Mac and target Macos 10.7 or later
#
##############################################
if (APPLE)
	set(CMAKE_OSX_ARCHITECTURES "x86_64")
#	set(CMAKE_Fortran_OSX_DEPLOYMENT_TARGET_FLAG "-mmacosx-version-min=10.7" CACHE PATH "")
	set(CMAKE_OSX_DEPLOYMENT_TARGET "10.7" CACHE PATH "")
#############################################
#
#  Choose 32bit or 64bit target arch on Linux
#
##############################################
elseif (LINUX)
	option(LINUX_32bit_BINARIES "Build 32bit Linux BINARIES" OFF)
	option(LINUX_64bit_BINARIES "Build 64bit Linux BINARIES" ON)

	if (LINUX_32bit_BINARIES AND LINUX_64bit_BINARIES)
		message(FATAL_ERROR "It is required to select either 32bit or 64bit Linux Binaries, not both")
	endif()

	if (LINUX_32bit_BINARIES) 
		set (ARCH_64bit FALSE)
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -m32")
		set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -m32")
		set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -m32")
	endif()
	
	if (LINUX_64bit_BINARIES) 
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -m64")
		set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -m64")
		set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -m64")
	endif()	
endif()

project(numerics)
cmake_minimum_required(VERSION 2.8.9)
enable_language(CXX)
enable_language(C)
enable_language(Fortran)

set(SVERSION "unversionedbuild")
message("svnversion ${SVERSION}")
add_definitions(-DSVNVERSION=svn${SVERSION})

#include (LocalJNI.cmake)

set(RULE_MESSAGES OFF)
set(CMAKE_COLOR_MAKEFILE OFF)
set(_ECLIPSE_VERSION "4.4")
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

#include (VCellCMakeShowVariables.txt)

if(DEFINED CMAKE_BUILD_TYPE)
   SET(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE} CACHE STRING "Choose the type of build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel.")
else()
   SET(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel.")
endif()

#include (CMakeTestCCompiler)
#include (CheckCSourceCompiles)
#include (CheckCXXSourceCompiles)
#include (CheckStructHasMember)
#include (CheckLibraryExists)
#include (CheckFunctionExists)
#include (CheckCCompilerFlag)
#include (CheckCSourceRuns)
#include (CheckSymbolExists)
#include (CheckTypeSize)
include (FindZLIB)
#message("gen "${CMAKE_GENERATOR})
#message("extra "${CMAKE_EXTRA_GENERATOR})
#message("system "${CMAKE_SYSTEM_NAME})
#message("windows "${WINDOWS})

option(OPTION_TARGET_MESSAGING "Parallel Solver (only chombo for now)" off)
option(OPTION_TARGET_PARALLEL "Parallel Solver (only chombo for now)" off)
option(OPTION_TARGET_DOCS "Generate Doxygen documentation" on)
option(OPTION_TARGET_CHOMBO2D_SOLVER "Chombo2D Solver" off)
option(OPTION_TARGET_CHOMBO3D_SOLVER "Chombo3D Solver" off)
option(OPTION_TARGET_SMOLDYN_SOLVER "only smoldyn" on)
option(OPTION_TARGET_FV_SOLVER on)
option(OPTION_TARGET_STOCHASTIC_SOLVER on)
option(OPTION_TARGET_NFSIM_SOLVER on)
option(OPTION_TARGET_MOVINGBOUNDARY_SOLVER on)
option(OPTION_TARGET_SUNDIALS_SOLVER on) #idawin
option(OPTION_TARGET_HY3S_SOLVERS on)
 
                  
if (${OPTION_TARGET_DOCS})
#	if (DOXYGEN_FOUND AND ${CMAKE_VERSION} GREATER_EQUAL 3.9)
#		find_package(Doxygen
#				REQUIRED dot
#				OPTIONAL_COMPONENTS mscgen dia)
#		set(DOXYGEN_GENERATE_HTML YES)
#		set(DOXYGEN_GENERATE_MAN NO)
#		doxygen_add_docs(
#   			doxygen
#    		${PROJECT_SOURCE_DIR}
#    		COMMENT "Generate man pages"
#    	)
#	else()
		add_custom_target(graphviz ALL
                  "${CMAKE_COMMAND}" "--graphviz=dependencies.dot" .
                  WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")
#    endif()
endif()

if ("${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.7" CACHE STRING "Choose minimum deploy target for Macos machines")
endif()
 
if (${OPTION_TARGET_CHOMBO2D_SOLVER} OR ${OPTION_TARGET_CHOMBO3D_SOLVER})     
	if (${OPTION_TARGET_PARALLEL})
		find_package(MPI)
		set(HDF5_ENABLE_PARALLEL ON CACHE BOOL "parallel hdf5 library")
		set(HDF5_BUILD_CPP_LIB OFF CACHE BOOL "" )
	else (${OPTION_TARGET_PARALLEL})
		message("serials solvers")
		set(HDF5_BUILD_CPP_LIB ON CACHE BOOL "" )
	endif (${OPTION_TARGET_PARALLEL})
else()
	set(HDF5_BUILD_CPP_LIB OFF CACHE BOOL "" )
endif()

#if (APPLE)
#endif()

message("hdf5 cpp ${HDF5_BUILD_CPP_LIB}")

set(LINUX FALSE)
if (${CMAKE_SYSTEM_NAME} MATCHES Linux)
	set(LINUX TRUE)
endif()

set(WINDOWS FALSE)
if (${CMAKE_SYSTEM_NAME} MATCHES Windows)
	set(WINDOWS TRUE)
	set(WIN32 TRUE)
	if (${CMAKE_GENERATOR} MATCHES "Unix Makefiles") 
		set(MINGW TRUE)
	endif (${CMAKE_GENERATOR} MATCHES "Unix Makefiles") 
endif()

set (ARCH_64bit FALSE)
if (CMAKE_SIZEOF_VOID_P EQUAL 8)
	set (ARCH_64bit TRUE)
endif()

if (NOT APPLE AND NOT LINUX AND NOT MINGW)
	message(FATAL_ERROR "Unsupported Operating System or C/C++ Compiler.")
endif()

#f (LINUX)
#set(BUILD_SHARED_LIBRARIES OFF)
#set(CMAKE_FIND_LIBRARY_SUFFIXES ".a")
#endif()

#---------------------------
#  IDE SUPPORT
#---------------------------
if (MINGW) 
	set(NETBEANS_WINDOWS TRUE)
	set(CMAKE_RC_FLAGS "-DGCC_WINDRES")
endif(MINGW)

add_definitions(-DFORTRAN_UNDERSCORE)

#############################################
# installation directories
#############################################
if (NOT OPTION_TARGET_MESSAGING)
	set(OPTION_EXE_DIRECTORY "bin" CACHE PATH "installation directory")
else (NOT OPTION_TARGET_MESSAGING)
	set(OPTION_EXE_DIRECTORY "../bin" CACHE PATH "installation directory")
endif (NOT OPTION_TARGET_MESSAGING)

set(OPTION_LIB_DIRECTORY "lib" CACHE PATH "library directory")
set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}")


add_definitions(-DUNIX)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC")
set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")


if (OPTION_TARGET_MESSAGING)
	if (APPLE OR MINGW)
		message(FATAL_ERROR "TARGET_SOLVERS_WITH_MESSAGING is not supported on MAC OS X and MINGW")
	endif(APPLE OR MINGW)
	
	#don't need graphics for server side solvers
	set(OPTION_USE_OPENGL OFF CACHE BOOL "")
	
	# set(APR_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/apr-1.4.6/include ${CMAKE_BINARY_DIR}/apr-1.4.6/include)
	# set(ACTIVEMQ_CPP_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/activemq-cpp-library-3.4.4/src/main ${CMAKE_BINARY_DIR}/activemq-cpp-library-3.4.4)
	# set(JMS_INCS ${ACTIVEMQ_CPP_INCLUDE_DIR} ${APR_INCLUDE_DIR})

	# set(JMS_LIBS activemq-cpp aprutil-1 apriconv-1 apr-1)

	# set(JMS_DEFS -DHAVE_CONFIG_H -DLINUX=2 -D_REENTRANT -D_GNU_SOURCE)
	#include(FindOpenSSL)
	#if (NOT OPENSSL_FOUND)
	#	message(FATAL_ERROR, "OpenSSL not found.")
	#endif()
	#list(APPEND JMS_LIBS ${OPENSSL_LIBRARIES} crypt pthread)
	include(FindActiveMQcpp)
	find_package(activemqcpp REQUIRED)
	# add_subdirectory(apr-1.4.6)
	# add_subdirectory(apr-iconv-1.2.1)
	# add_subdirectory(apr-util-1.4.1)
	# add_subdirectory(activemq-cpp-library-3.4.4)
	add_definitions(-DUSE_MESSAGING)
endif()

######################################
#
# Add subdirectories
# 
#######################################

	if (${OPTION_TARGET_MOVINGBOUNDARY_SOLVER} OR
		${OPTION_TARGET_FV_SOLVER} OR
		${OPTION_TARGET_CHOMBO2D_SOLVER} OR
		${OPTION_TARGET_CHOMBO3D_SOLVER}
		)
		
		if (${HDF5_BUILD_CPP_LIB})
			add_subdirectory(hdf5-1.8.11)
		else()
#			set(HDF5_USE_STATIC_LIBRARIES ON)
			#CMakeList.txt that comes with hdf5 switches library name if compiled with debug
			find_package(HDF5 1.8.11 REQUIRED COMPONENTS HL CXX C)
			set(HDF_VERSION ${HDF5_VERSION})
			######    ${HDF5_HL_LIBRARIES} ${HDF5_CXX_LIBRARIES}
			if (${OPTION_TARGET_CHOMBO2D_SOLVER} OR
				${OPTION_TARGET_CHOMBO3D_SOLVER}
				)
		   			set(_hdf5_libs hdf5 hdf5_cpp)
	    			set(HDF_LIB hdf5 hdf5_cpp)
	    	endif()
	    endif()
	endif()

	add_subdirectory(VCellMessaging)
	set(VCELLMESSAGING_INCLUDE_DIR VCellMessaging/include)
	if (OPTION_TARGET_MESSAGING)
		set(VCELLMESSAGING_LIBS activemq-cpp aprutil-1 apriconv-1 apr-1)
	else()
		set(VCELLMESSAGING_LIBS vcellmessaging)
	endif()
 
    if (${OPTION_TARGET_MOVINGBOUNDARY_SOLVER} OR 
    	${OPTION_TARGET_NFSIM_SOLVER}
    	)
    	add_subdirectory(vcommons)
    endif()
    
 	if (${OPTION_TARGET_FV_SOLVER} OR
		${OPTION_TARGET_SMOLDYN_SOLVER}
		)
		add_subdirectory(VCellZipUtils)
		add_subdirectory(libzip-1.2.0)
	endif()
	
    if (${OPTION_TARGET_FV_SOLVER} OR 
    	${OPTION_TARGET_SMOLDYN_SOLVER} OR
    	${OPTION_TARGET_SUNDIALS_SOLVER} OR 
    	${OPTION_TARGET_STOCHASTIC_SOLVER} OR 
    	${OPTION_TARGET_MOVINGBOUNDARY_SOLVER}
    	)
    	message(STATUS "adding ExpressionParser")
		add_subdirectory(ExpressionParser)
	endif()
	
	if (${OPTION_TARGET_CHOMBO2D_SOLVER} OR
		${OPTION_TARGET_CHOMBO3D_SOLVER}
		) 
		if (${OPTION_TARGET_CHOMBO3D_SOLVER})
			set(CHOMBO_3D TRUE)
		else()
			set(CHOMBO_3D FALSE)
		endif()
		
		add_subdirectory(VCellChombo)
		if (${OPTION_TARGET_PARALLEL})
			set(PARALLEL_CHOMBO_SOLVER TRUE)
#			add_subdirectory(ChomboParallel)
		else()
			set(PARALLEL_CHOMBO_SOLVER FALSE)
#			add_subdirectory(Chombo)
		endif()
	endif()

	if (${OPTION_TARGET_SUNDIALS_SOLVER} OR
		${OPTION_TARGET_SMOLDYN_SOLVER} OR
		${OPTION_TARGET_FV_SOLVER}
		)
    	message(STATUS "adding sundials")
		add_subdirectory(sundials)
	endif ()
	
	if (${OPTION_TARGET_SUNDIALS_SOLVER})
		add_subdirectory(IDAWin)
	endif ()
	
	if (${OPTION_TARGET_MOVINGBOUNDARY_SOLVER})
    	message(STATUS "adding MovingBoundarySolver")
		add_subdirectory(MBSolver)
    	message(STATUS "adding FronTierLib")
		add_subdirectory(FronTierLib)
	endif()
	
	if (${OPTION_TARGET_FV_SOLVER} OR ${OPTION_TARGET_SMOLDYN_SOLVER})
		if (NOT APPLE)
			add_subdirectory(blas)
		endif()
	endif()

	if (${OPTION_TARGET_HY3S_SOLVERS})
		if (MINGW)
			add_definitions(-ffree-line-length-none)
#			add_definitions(-std=legacy)
		endif()
		add_subdirectory(Hy3S)
		# set (NETCDF_F90 "YES")
		# find_package (NetCDF REQUIRED)
		# add_subdirectory(netcdf-fortran-4.4.4/fortran)
		add_subdirectory(netcdf-3.6.2)
	endif()
	
	if (${OPTION_TARGET_NFSIM_SOLVER})
		add_subdirectory(NFsim_v1.11) 
	endif( )
	
	if (${OPTION_TARGET_STOCHASTIC_SOLVER})
		add_subdirectory(Stochastic)
	endif ()
	
	if (OPTION_TARGET_SMOLDYN_SOLVER OR 
		OPTION_TARGET_FV_SOLVER
		)
		option(OPTION_VCELL "Compile Smoldyn for VCell" ON)
		option(OPTION_NSV "Compile Smoldyn with NextSubvolume functionality" OFF)
		option(OPTION_USE_OPENGL "Build with OpenGL support" OFF)
		option(OPTION_USE_ZLIB "Build with Zlib support" ON)
		option(OPTION_USE_LIBTIFF "Build with LibTiff support" OFF)
		option(OPTION_USE_ICONV "Build with Libiconv support" OFF)
		SET(HAVE_ZLIB TRUE)
		set(OPTION_TARGET_LIBSMOLDYN ON)
		set(OPTION_VCELL ON)
		add_subdirectory(bridgeVCellSmoldyn)
		add_subdirectory(smoldyn-2.38)
	endif ()
	
	if (${OPTION_TARGET_FV_SOLVER} OR
		${OPTION_TARGET_SMOLDYN_SOLVER}
		)
		add_subdirectory(VCell)
		add_subdirectory(PCGPack) 
		add_subdirectory(qhull)
	endif ()
	
