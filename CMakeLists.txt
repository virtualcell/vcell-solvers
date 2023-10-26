#############################################
#
#  Build 64bit binaries on Mac and target Macos 10.7 or later
#
##############################################
if (APPLE)
	set(CMAKE_OSX_ARCHITECTURES "x86_64")
#	set(CMAKE_Fortran_OSX_DEPLOYMENT_TARGET_FLAG "-mmacosx-version-min=10.7" CACHE PATH "")
#	set(CMAKE_OSX_DEPLOYMENT_TARGET "10.7" CACHE PATH "")
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

cmake_minimum_required(VERSION 3.13)
project(numerics)
set(CMAKE_CXX_STANDARD 14)
enable_language(CXX)
enable_language(C)
enable_language(Fortran)

if(POLICY CMP0057)
  cmake_policy(SET CMP0057 NEW)
endif()

set(SVERSION "unversionedbuild")
message("svnversion ${SVERSION}")
add_definitions(-DSVNVERSION=svn${SVERSION})

#include (LocalJNI.cmake)

set(RULE_MESSAGES OFF)
set(CMAKE_COLOR_MAKEFILE OFF)
set(_ECLIPSE_VERSION "4.4")
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/modules/")

include(GetGitRevisionDescription)
git_describe(GIT_DESCRIBE)

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

option(OPTION_TARGET_MESSAGING "Messaging (requires libcurl)" off)
option(OPTION_TARGET_PARALLEL "Parallel Solver (only chombo for now)" off)
option(OPTION_TARGET_PETSC "PETSc Solver (only FV for now)" off)
option(OPTION_TARGET_DOCS "Generate Doxygen documentation" on)
option(OPTION_TARGET_CHOMBO2D_SOLVER "Chombo2D Solver" off)
option(OPTION_TARGET_CHOMBO3D_SOLVER "Chombo3D Solver" on)
option(OPTION_TARGET_SMOLDYN_SOLVER "only smoldyn" on)
option(OPTION_TARGET_FV_SOLVER on)
option(OPTION_TARGET_STOCHASTIC_SOLVER on)
option(OPTION_TARGET_NFSIM_SOLVER on)
option(OPTION_TARGET_MOVINGBOUNDARY_SOLVER off)
option(OPTION_TARGET_SUNDIALS_SOLVER on) #idawin
option(OPTION_TARGET_HY3S_SOLVERS off)
 
                  
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
	else (${OPTION_TARGET_PARALLEL})
		message("serials solvers")
	endif (${OPTION_TARGET_PARALLEL})
endif()

#if (APPLE)
#endif()

set(LINUX FALSE)
if (${CMAKE_SYSTEM_NAME} MATCHES Linux)
	set(LINUX TRUE)
endif()

set(WINDOWS FALSE)
if (${CMAKE_SYSTEM_NAME} MATCHES Windows)
	set(WINDOWS TRUE)
#	set(WIN32 TRUE)
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

#if (MINGW)
#	add_definitions(-DWIN32)
#endif(MINGW)

#set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC")
#set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")


if (OPTION_TARGET_MESSAGING)
#	if (APPLE OR MINGW)
#		message(FATAL_ERROR "TARGET_SOLVERS_WITH_MESSAGING is not supported on MAC OS X and MINGW")
#	endif(APPLE OR MINGW)
	
	#don't need graphics for server side solvers
	set(OPTION_USE_OPENGL OFF CACHE BOOL "")
	
	find_package(CURL REQUIRED)
	set(THREADS_PREFER_PTHREAD_FLAG ON)
	find_package(Threads REQUIRED)
	add_definitions(-DUSE_MESSAGING)
endif()

if (OPTION_TARGET_PETSC)
	find_package(PETSc REQUIRED)
endif()

######################################
#
# Add subdirectories
# 
#######################################

if (OPTION_TARGET_MOVINGBOUNDARY_SOLVER OR
		OPTION_TARGET_FV_SOLVER OR
		OPTION_TARGET_SMOLDYN_SOLVER OR
		OPTION_TARGET_CHOMBO2D_SOLVER OR
		OPTION_TARGET_CHOMBO3D_SOLVER)

	message("!!!!!!!!!!!!!!!! finding hdf5 lib !!!!!!!!!!!!!!!!!!!!!")
	set(HDF5_USE_STATIC_LIBRARIES ON)
	find_package(HDF5 COMPONENTS HL CXX C)
	set(HDF_VERSION ${HDF5_VERSION})
	if (OPTION_TARGET_CHOMBO2D_SOLVER OR OPTION_TARGET_CHOMBO3D_SOLVER)
		set(_hdf5_libs hdf5 hdf5_cpp)
		set(HDF_LIB hdf5 hdf5_cpp)
	endif()
endif()

if (${OPTION_TARGET_CHOMBO3D_SOLVER} OR ${OPTION_TARGET_CHOMBO2D_SOLVER})
	add_subdirectory(hdf5-1.8.11)

	include(ExternalProject)
	message("CMAKE_BINARY_DIR ${CMAKE_BINARY_DIR}")
	ExternalProject_Add(Chombo
			PREFIX ${CMAKE_BINARY_DIR}/Chombo
			URL ${CMAKE_SOURCE_DIR}/Chombo.tgz
			BUILD_COMMAND pushd ${CMAKE_BINARY_DIR}/Chombo/src/Chombo/lib && chmod +x ../configure && ../configure && cp mk/Make.defs.local.release.3D mk/Make.defs.local && make all && popd
			BUILD_IN_SOURCE true
			CONFIGURE_COMMAND ""
			INSTALL_COMMAND "")
	add_dependencies(Chombo HDF5)
endif()

#if (${OPTION_TARGET_CHOMBO3D_SOLVER})
#	include(ExternalProject)
#	message("CMAKE_BINARY_DIR ${CMAKE_BINARY_DIR}")
#	ExternalProject_Add(VCellChombo3D
#			PREFIX ${CMAKE_BINARY_DIR}/VCellChombo3D
#			URL ${CMAKE_SOURCE_DIR}/Chombo.tgz ${CMAKE_BINARY_DIR}/VCellChombo3D"
#			#			SOURCE_DIR ${CMAKE_BINARY_DIR}/VCellChombo3D/src
#			BUILD_IN_SOURCE true)
#
#endif()

#if (${OPTION_TARGET_CHOMBO2D_SOLVER})
#	include(ExternalProject)
#	message("CMAKE_BINARY_DIR ${CMAKE_BINARY_DIR}")
#	ExternalProject_Add(VCellChombo2D
#			URL ${CMAKE_CURRENT_SOURCE_DIR}/VCellChombo
#			PREFIX ${CMAKE_BINARY_DIR}/Chombo2D
#			BUILD_IN_SOURCE true)
#endif()

add_subdirectory(VCellMessaging)
 
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

include(FetchContent)
FetchContent_Declare(
		googletest
		URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)
if (WINDOWS)
	# For Windows: Prevent overriding the parent project's compiler/linker settings
	set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
endif()

FetchContent_MakeAvailable(googletest)
enable_testing()

get_cmake_property(_variableNames VARIABLES)
list (SORT _variableNames)
foreach (_variableName ${_variableNames})
	message(STATUS "${_variableName}=${${_variableName}}")
endforeach()

include(CMakePrintHelpers)
cmake_print_variables(OPTION_TARGET_MESSAGING OPTION_TARGET_PARALLEL OPTION_TARGET_PETSC
		OPTION_TARGET_DOCS OPTION_TARGET_CHOMBO2D_SOLVER OPTION_TARGET_CHOMBO3D_SOLVER
		OPTION_TARGET_SMOLDYN_SOLVER OPTION_TARGET_FV_SOLVER OPTION_TARGET_STOCHASTIC_SOLVER
		OPTION_TARGET_NFSIM_SOLVER OPTION_TARGET_MOVINGBOUNDARY_SOLVER OPTION_TARGET_SUNDIALS_SOLVER
		OPTION_TARGET_HY3S_SOLVERS OPTION_TARGET_LIBSMOLDYN OPTION_TARGET_VCELL OPTION_TARGET_NSV )
cmake_print_variables(CMAKE_CXX_FLAGS CMAKE_C_FLAGS CMAKE_Fortran_FLAGS)
cmake_print_variables(CMAKE_SYSTEM_NAME WINDOWS WIN32 MINGW APPLE ARCH_64bit ARCH_32bit)
cmake_print_variables(CMAKE_CPP_COMPILER CMAKE_C_COMPILER CMAKE_CXX_COMPILER CMAKE_Fortran_COMPILER)
cmake_print_variables(HAVE_ZLIB)

