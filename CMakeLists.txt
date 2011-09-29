cmake_minimum_required(VERSION 2.8)
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
option(TARGET_SOLVERS_SERVER_SIDE "Server Side Solvers (With Messaging)" ON)
option(TARGET_SOLVERS_CLIENT_SIDE "Client Side Solvers (Without Messaging)" OFF)
option(TARGET_JAVA_BINDING "Java Binding" OFF)

set (TARGET_COUNT 0)
if (TARGET_SOLVERS_SERVER_SIDE)
	math(EXPR TARGET_COUNT "${TARGET_COUNT} + 1")
endif()
if (TARGET_SOLVERS_CLIENT_SIDE)
	math(EXPR TARGET_COUNT "${TARGET_COUNT} + 1")
endif()
if (TARGET_JAVA_BINDING)
	math(EXPR TARGET_COUNT "${TARGET_COUNT} + 1")
endif()

if (TARGET_COUNT GREATER 1 OR TARGET_COUNT EQUAL 0)
	message(FATAL_ERROR "It is required to select one target, either TARGET_SOLVERS_SERVER_SIDE, or TARGET_SOLVERS_CLIENT_SIDE or TARGET_JAVA_BINDING")
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
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -m32")
		set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -m32")
	endif()
	
	if (LINUX_64bit_BINARIES) 
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -m64")
		set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -m64")
	endif()
endif()

######################################
#
# set proper compiler options
#
#######################################
if (MSVC)
	add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_CRT_NONSTDC_NO_DEPRECATE)
endif()
 
if (TARGET_SOLVERS_SERVER_SIDE OR TARGET_SOLVERS_CLIENT_SIDE)

	if (TARGET_SOLVERS_SERVER_SIDE)
		if (APPLE)
			message(FATAL_ERROR "TARGET_SOLVERS_SERVER_SIDE is not supported on MAC OS X")
		endif(APPLE)
		
		add_definitions(-DUSE_MESSAGING)
		include_directories(${CMAKE_CURRENT_SOURCE_DIR}/VCell/include)	
		if (LINUX)
			set(SONICMQ_LIBS smq)			
			if (LINUX_32bit_BINARIES)
				set(SONICMQ_DIR ${CMAKE_CURRENT_SOURCE_DIR}/SonicCClient/Linux_AS21)	
				include_directories(${SONICMQ_DIR}/include ${SONICMQ_DIR}/include/CPP)
				link_directories(${SONICMQ_DIR}/bin/release)
			endif()
			if (LINUX_64bit_BINARIES)
				set(SONICMQ_DIR ${CMAKE_CURRENT_SOURCE_DIR}/SonicCClient64/Linux_AS4_64)	
				include_directories(${SONICMQ_DIR}/include ${SONICMQ_DIR}/include/CPP)
				link_directories(${SONICMQ_DIR}/bin/release)
			endif()						
		elseif (MSVC)
			set(SONICMQ_LIBS libsmq libsmqj Ws2_32)
			set(SONICMQ_DIR ${CMAKE_CURRENT_SOURCE_DIR}/SonicCClient/Win32)	
			include_directories(${SONICMQ_DIR}/include ${SONICMQ_DIR}/include/CPP)
			link_directories(${SONICMQ_DIR}/lib)
		endif()		
	endif(TARGET_SOLVERS_SERVER_SIDE)
	
######################################
#
# use /MT instead of /MD on Windows
#
#######################################	
	if (MSVC)
		foreach(flag_var
			CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE
			CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO
			CMAKE_C_FLAGS CMAKE_C_FLAGS_DEBUG CMAKE_C_FLAGS_RELEASE
			CMAKE_C_FLAGS_MINSIZEREL CMAKE_C_FLAGS_RELWITHDEBINFO)
	
			if(${flag_var} MATCHES "/MD")
				string(REGEX REPLACE "/MD" "/MT" temp_var "${${flag_var}}")
				set(${flag_var} ${temp_var} CACHE STRING "Flags used by the compiler" FORCE)
			endif()			
		endforeach(flag_var)	
	endif (MSVC)
endif() 

if (TARGET_JAVA_BINDING)
	if (UNIX)
######################################
#
# use -fPIC on UNIX
#
#######################################	
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC")
		set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")
	elseif (MSVC)
######################################
#
# use /MD instead of /MT on Windows
#
#######################################		
        foreach(flag_var
        		CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE
                CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO
                CMAKE_C_FLAGS CMAKE_C_FLAGS_DEBUG CMAKE_C_FLAGS_RELEASE
                CMAKE_C_FLAGS_MINSIZEREL CMAKE_C_FLAGS_RELWITHDEBINFO)
        
                if(${flag_var} MATCHES "/MT")
                    string(REGEX REPLACE "/MT" "/MD" temp_var "${${flag_var}}")
                    set(${flag_var} ${temp_var} CACHE STRING "Flags used by the compiler" FORCE)
                endif()
		endforeach(flag_var)
	endif(UNIX)
endif(TARGET_JAVA_BINDING)


######################################
#
# Add subdirectories
#
#######################################
add_subdirectory(ExpressionParser)
add_subdirectory(IDAWin)
######################################
#
# By default Linux and Mac have zlib 
# installed in the system
#
#######################################
if (MSVC)
	add_subdirectory(zlib)
endif(MSVC)

add_subdirectory(sundials) 
if (TARGET_SOLVERS_SERVER_SIDE OR TARGET_SOLVERS_CLIENT_SIDE)
	enable_language(Fortran)
	if (LINUX)
		add_subdirectory(blas)
	endif()
	add_subdirectory(netcdf-3.6.2)
	add_subdirectory(unzip)
	add_subdirectory(zip)
	if (LINUX)	
		add_subdirectory(PCGPack)
	endif() 
	add_subdirectory(qhull) 
	add_subdirectory(Stochastic) 
	add_subdirectory(VCell) 
	add_subdirectory(smoldyn) 
#	add_subdirectory(Hy3S)
endif() 

if (TARGET_JAVA_BINDING)
	add_subdirectory(fsqp)
	add_subdirectory(tinyxml)
	add_subdirectory(JavaBinding)
	add_subdirectory(Optimization2)
endif(TARGET_JAVA_BINDING)
