cmake_minimum_required(VERSION 2.8)

if(DEFINED CMAKE_BUILD_TYPE)
   SET(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE} CACHE STRING "Choose the type of build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel.")
else()
   SET(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel.")
endif()

project(numerics)

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

set(LINUX FALSE)
if (${CMAKE_SYSTEM_NAME} MATCHES Linux)
        set(LINUX TRUE)
endif()

set (ARCH_64bit FALSE)
if (CMAKE_SIZEOF_VOID_P EQUAL 8)
	set (ARCH_64bit TRUE)
endif()

if (NOT MSVC AND NOT APPLE AND NOT LINUX)
	message(FATAL_ERROR "Unsupported Operating System or C/C++ Compiler.")
endif()

#############################################
#
#  Check MSVC version
#  1200 = VS  6.0
#  1300 = VS  7.0
#  1310 = VS  7.1
#  1400 = VS  8.0
#  1500 = VS  9.0
#  1600 = VS 10.0
#
##############################################
if (MSVC)
	if (MSVC_VERSION LESS 1400)
    	message(FATAL_ERROR "Visual Studio 8 or higher is required.")
	endif()
endif()
    
#############################################
#
#  Options, check one and only one is selected
#  1. server side solvers with messaging
#  2. client side quick run solvers
#  3. java binding
#
##############################################
option(TARGET_SOLVERS_WITH_MESSAGING "Server Side Solvers (With Messaging)" OFF)
option(TARGET_SOLVERS_NO_MESSAGING "Client Side Solvers (Without Messaging)" ON)
option(TARGET_JAVA_BINDING "Java Binding" OFF)
option(TARGET_CHOMBO_BUILD "Chombo Build" OFF)

set (TARGET_COUNT 0)
if (TARGET_SOLVERS_WITH_MESSAGING)
	math(EXPR TARGET_COUNT "${TARGET_COUNT} + 1")
endif()
if (TARGET_SOLVERS_NO_MESSAGING)
	math(EXPR TARGET_COUNT "${TARGET_COUNT} + 1")
endif()
if (TARGET_JAVA_BINDING)
	math(EXPR TARGET_COUNT "${TARGET_COUNT} + 1")
endif()
if (TARGET_CHOMBO_BUILD)
	math(EXPR TARGET_COUNT "${TARGET_COUNT} + 1")
endif()

if (TARGET_COUNT GREATER 1 OR TARGET_COUNT EQUAL 0)
	message(FATAL_ERROR "It is required to select one target, either TARGET_SOLVERS_WITH_MESSAGING, or TARGET_SOLVERS_NO_MESSAGING or TARGET_JAVA_BINDING or TARGET_CHOMBO_BUILD")
endif()

#############################################
#
#  Build universal binaries on Mac
#
##############################################
if (APPLE)
	option(MACOSX_UNIVERSAL_BINARIES "Build Universal Binaries" ON)
	if (MACOSX_UNIVERSAL_BINARIES)
		set(CMAKE_OSX_ARCHITECTURES "i386;ppc;x86_64")
	endif()
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

######################################
#
# try to find ifort compiler
#
#######################################
enable_language(Fortran)
set(IFORT_EXECUTABLE NOTFOUND)

######################################
#
# on Windows you are supposed to 
# open Intel Fortran Compiler Command Prompt
# so it is not necessary to find ifort
#
#######################################
if (MSVC)
	set(IFORT_EXECUTABLE ifort)
endif()

if (LINUX_32bit_BINARIES)
	find_program(IFORT_EXECUTABLE ifort /opt/intel/fc/10.1.015/bin/)
elseif (LINUX_64bit_BINARIES)
	find_program(IFORT_EXECUTABLE ifort /opt/intel/fce/10.1.015/bin/ /cm/shared/apps/intel/fce/10.1.015/bin/)
endif()

if (IFORT_EXECUTABLE MATCHES IFORT_EXECUTABLE-NOTFOUND)
	message(FATAL_ERROR "intel fortran compiler (ifort) is required")
endif()
	
######################################
#
# set proper compiler options
#
#######################################
if (MSVC)
	add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_CRT_NONSTDC_NO_DEPRECATE)
	
	if (TARGET_SOLVERS_WITH_MESSAGING OR TARGET_SOLVERS_NO_MESSAGING)
		
		######################################
		#
		# use /MT instead of /MD on Windows
		#
		#######################################	
		foreach(flag_var
				CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE
				CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO
				CMAKE_C_FLAGS_DEBUG CMAKE_C_FLAGS_RELEASE
				CMAKE_C_FLAGS_MINSIZEREL CMAKE_C_FLAGS_RELWITHDEBINFO
				CMAKE_Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_RELEASE
				CMAKE_Fortran_FLAGS_MINSIZEREL CMAKE_Fortran_FLAGS_RELWITHDEBINFO
			)
	
			if(${flag_var} MATCHES "/MD")
				string(REGEX REPLACE "/MD" "/MT" temp_var "${${flag_var}}")
				set(${flag_var} ${temp_var} CACHE STRING "Flags used by the compiler" FORCE)
            elseif(NOT ${flag_var} MATCHES "/MT")
            	######################################
				#
				# no /MT or /MTd at all, append it
				#
				#######################################
				set(temp_var "${${flag_var}}")
            	if (flag_var MATCHES "DEBUG")
            		set(temp_var "${temp_var} /MTd")
            	else()
            		set(temp_var "${temp_var} /MT")
            	endif()
            	set(${flag_var} ${temp_var} CACHE STRING "Flags used by the compiler" FORCE)            	
			endif()			
		endforeach(flag_var)
		
	elseif(TARGET_JAVA_BINDING)
		######################################
		#
		# use /MD instead of /MT on Windows
		#
		#######################################	
        foreach(flag_var
        		CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE
                CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO
                CMAKE_C_FLAGS_DEBUG CMAKE_C_FLAGS_RELEASE
                CMAKE_C_FLAGS_MINSIZEREL CMAKE_C_FLAGS_RELWITHDEBINFO
        		CMAKE_Fortran_FLAGS_DEBUG CMAKE_Fortran_FLAGS_RELEASE
				CMAKE_Fortran_FLAGS_MINSIZEREL CMAKE_Fortran_FLAGS_RELWITHDEBINFO
			)
        
            if(${flag_var} MATCHES "/MT")
                string(REGEX REPLACE "/MT" "/MD" temp_var "${${flag_var}}")
                set(${flag_var} ${temp_var} CACHE STRING "Flags used by the compiler" FORCE)
			elseif(NOT ${flag_var} MATCHES "/MD")
            	######################################
				#
				# no /MD or /MDd at all, append it
				#
				#######################################
				set(temp_var "${${flag_var}}")
            	if (flag_var MATCHES "DEBUG")
            		set(temp_var "${temp_var} /MDd")
            	else()
            		set(temp_var "${temp_var} /MD")
            	endif()
            	set(${flag_var} ${temp_var} CACHE STRING "Flags used by the compiler" FORCE)
			endif()
		endforeach(flag_var)
	endif()
		
else()
	
	add_definitions(-DUNIX)
	if (LINUX)
		######################################
		#
		# SonicCClient uses LINUX
		# unzip uses LINUX
		#
		#######################################	
		add_definitions(-DLINUX)
	endif(LINUX)
	
	if (TARGET_JAVA_BINDING)
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC")
		set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")
	endif()
	
endif(MSVC)


if (TARGET_SOLVERS_WITH_MESSAGING)
	if (APPLE)
		message(FATAL_ERROR "TARGET_SOLVERS_WITH_MESSAGING is not supported on MAC OS X")
	endif(APPLE)
	
	add_definitions(-DUSE_MESSAGING)
	if (LINUX)
		set(SONICMQ_LIBS smq)			
		if (LINUX_32bit_BINARIES)
			set(SONICMQ_DIR ${CMAKE_CURRENT_SOURCE_DIR}/SonicCClient/PROGRESS_SONIC_CCLIENT_8.5.1_RHLNX_32)	
			include_directories(${SONICMQ_DIR}/include ${SONICMQ_DIR}/include/CPP)
			link_directories(${SONICMQ_DIR}/bin/release)
		endif()
		if (LINUX_64bit_BINARIES)
			set(SONICMQ_DIR ${CMAKE_CURRENT_SOURCE_DIR}/SonicCClient/PROGRESS_SONIC_CCLIENT_8.5.1_RHLNX_64)	
			include_directories(${SONICMQ_DIR}/include ${SONICMQ_DIR}/include/CPP)
			link_directories(${SONICMQ_DIR}/bin/release)
		endif()						
	elseif (MSVC)
		set(SONICMQ_LIBS libsmq libsmqj Ws2_32)
		if (ARCH_64bit)
			set(SONICMQ_DIR ${CMAKE_CURRENT_SOURCE_DIR}/SonicCClient/PROGRESS_SONIC_CCLIENT_8.5.1_WIN_64)
		else()
			set(SONICMQ_DIR ${CMAKE_CURRENT_SOURCE_DIR}/SonicCClient/PROGRESS_SONIC_CCLIENT_8.5.1_WIN_32)
		endif()	
		include_directories(${SONICMQ_DIR}/include ${SONICMQ_DIR}/include/CPP)
		link_directories(${SONICMQ_DIR}/lib/release)
	endif()		
endif()

######################################
#
# Add subdirectories
#
#######################################
add_subdirectory(ExpressionParser)
add_subdirectory(IDAWin)
add_subdirectory(sundials)
 
if (TARGET_SOLVERS_WITH_MESSAGING OR TARGET_SOLVERS_NO_MESSAGING)
	if (MSVC OR LINUX)
		add_subdirectory(blas)
	endif()
	add_subdirectory(hdf5-1.8.8)
	add_subdirectory(Hy3S)
	add_subdirectory(netcdf-3.6.2)
	add_subdirectory(PCGPack) 
	add_subdirectory(qhull)
	if (MSVC)
#		add_subdirectory(glut-3.7.6)
	endif()
	add_subdirectory(smoldyn) 
	add_subdirectory(Stochastic)
	add_subdirectory(unzip) 
	add_subdirectory(VCell) 
	add_subdirectory(zip)
	if (NOT ZLIB_FOUND)
		add_subdirectory(zlib)
	endif()
endif() 

if (TARGET_SOLVERS_WITH_MESSAGING)
	add_subdirectory(VCell_CodeGen)
endif(TARGET_SOLVERS_WITH_MESSAGING)

if (TARGET_JAVA_BINDING)
	add_subdirectory(fsqp)
	add_subdirectory(tinyxml)
	add_subdirectory(JavaBinding)
	add_subdirectory(Optimization2)
endif(TARGET_JAVA_BINDING)

if (TARGET_CHOMBO_BUILD)
	add_subdirectory(ExpressionParser)
	add_subdirectory(sundials)

	add_subdirectory(hdf5-1.8.8)
	add_subdirectory(PCGPack) 
	add_subdirectory(qhull)
	add_subdirectory(unzip)
	add_subdirectory(VCell)
	add_subdirectory(zip)
	if (NOT ZLIB_FOUND)
		add_subdirectory(zlib)
	endif()
endif()

set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}")
