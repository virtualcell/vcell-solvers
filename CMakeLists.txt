cmake_minimum_required(VERSION 2.8.9)
include (svnversion.txt)
include (LocalJNI.cmake)

set(RULE_MESSAGES OFF)
set(CMAKE_COLOR_MAKEFILE OFF)
set(HDF_VERSION "hdf5-1.8.11")
set(ZLIB_VERSION "zlib-1.2.8")
set(_ECLIPSE_VERSION "4.4")
#include (VCellCMakeShowVariables.txt)

if(DEFINED CMAKE_BUILD_TYPE)
   SET(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE} CACHE STRING "Choose the type of build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel.")
else()
   SET(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel.")
endif()
option(PARALLEL_CHOMBO_SOLVER "build parallel Chombo solver" OFF)

#CMakeList.txt that comes with hdf5 switches library name if compiled with debug
set(HDF_LIB "hdf5")
if (${CMAKE_BUILD_TYPE} STREQUAL "Debug")
	set(HDF_LIB "hdf5_debug")
endif( )

project(numerics)

option(NEW_SMOLDYN "Smoldyn 2.37" ON)
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
option(CHOMBO_SOLVER "Build Chombo" on)
set(FV_SOLVER on)
set(STOCHASTIC_SOLVER on)
set(NFSIM_EXE on)
set(SUNDIALS_SOLVER on) #idawin
set(HY3S_SOLVERS on) 

if (${PARALLEL_CHOMBO_SOLVER})
	message("parallel chombo solver")
	find_package(MPI)
	set(FV_SOLVER off)
	set(STOCHASTIC_SOLVER off)
	set(NFSIM_EXE off)
	set(SUNDIALS_SOLVER off) #idawin
	set(HY3S_SOLVERS off) 
	set(OPTION_TARGET_SMOLDYN OFF CACHE BOOL "build smoldyn executable")
	set(OPTION_TARGET_LIBSMOLDYN OFF CACHE BOOL "build smoldyn library")
	set(HDF5_ENABLE_PARALLEL ON CACHE BOOL "parallel hdf5 library")
	set(HDF5_BUILD_CPP_LIB OFF CACHE BOOL "" )
else (${PARALLEL_CHOMBO_SOLVER})
	message("serials solvers")
	set(HDF5_BUILD_CPP_LIB ON CACHE BOOL "" )
	set(OPTION_TARGET_LIBSMOLDYN ON CACHE BOOL "")
endif (${PARALLEL_CHOMBO_SOLVER})
message("hdf5 cpp ${HDF5_BUILD_CPP_LIB}")

if (CYGWIN) 
	set(NETBEANS_WINDOWS TRUE)
	set(OPTION_TARGET_SMOLDYN ON CACHE BOOL "")
	set(OPTION_TARGET_LIBSMOLDYN ON CACHE BOOL "build smoldyn library")
	set(OPTION_USE_OPENGL OFF CACHE BOOL "")
	set(CMAKE_RC_FLAGS "-DGCC_WINDRES")
	set(CLIENT_SIDE_DEFAULT TRUE)
	set(SERVER_SIDE_DEFAULT FALSE)
	set(HYBRID_DEFAULT FALSE)
endif(CYGWIN)

set(LINUX FALSE)
if (${CMAKE_SYSTEM_NAME} MATCHES Linux)
	set(LINUX TRUE)
	set(CLIENT_SIDE_DEFAULT FALSE)
	set(SERVER_SIDE_DEFAULT TRUE)
	set(HYBRID_DEFAULT FALSE)
endif()

if (APPLE) 
	set(CLIENT_SIDE_DEFAULT TRUE)
	set(SERVER_SIDE_DEFAULT FALSE)
	set(HYBRID_DEFAULT FALSE)
endif(APPLE)
if (MSVC) 
	set(CLIENT_SIDE_DEFAULT FALSE)
	set(SERVER_SIDE_DEFAULT FALSE)
	set(HYBRID_DEFAULT TRUE)
endif(MSVC)

set(WINDOWS FALSE)
if (${CMAKE_SYSTEM_NAME} MATCHES Windows)
	set(WINDOWS TRUE)
	set(WIN32 TRUE)
	if (${CMAKE_GENERATOR} MATCHES "Unix Makefiles") 
		set(CYGWIN TRUE)
	endif (${CMAKE_GENERATOR} MATCHES "Unix Makefiles") 
endif()

set (ARCH_64bit FALSE)
if (CMAKE_SIZEOF_VOID_P EQUAL 8)
	set (ARCH_64bit TRUE)
endif()

if (NOT MSVC AND NOT APPLE AND NOT LINUX AND NOT CYGWIN)
	message(FATAL_ERROR "Unsupported Operating System or C/C++ Compiler.")
endif()

if (MSVC)
	add_definitions(-DFORTRAN_BARE)
else( ) 
	add_definitions(-DFORTRAN_UNDERSCORE)
endif( )

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
	if (MSVC_VERSION LESS 1600)
    	message(FATAL_ERROR "Visual Studio 10 or higher is required.")
	endif()
	if (MSVC_VERSION GREATER 1600)
		message(WARNING "Virtual Cell install program assumes VS 2010")
	endif( )
endif()


#############################################
#
#  Options, check one and only one is selected
#  1. server side solvers with messaging
#  2. client side quick run solvers
#  3. java binding
#
##############################################
if (CYGWIN OR APPLE)
	option(TARGET_SOLVERS_NO_MESSAGING "Client Side Solvers (Without Messaging)" ${CLIENT_SIDE_DEFAULT})
else()
	option(TARGET_SOLVERS_WITH_MESSAGING "Server Side Solvers (With Messaging)" ${SERVER_SIDE_DEFAULT})
	option(TARGET_SOLVERS_NO_MESSAGING "Client Side Solvers (Without Messaging)" ${CLIENT_SIDE_DEFAULT})
endif ()
if (WINDOWS) 
	#need MSVC and IFortran for these
	option(TARGET_HYBRID_SOLVERS "Hybrid solvers (Without Messaging)" ${HYBRID_DEFAULT})
endif( )
option(TARGET_JAVA_BINDING "Java Binding" OFF)

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
if (TARGET_HYBRID_SOLVERS)
	math(EXPR TARGET_COUNT "${TARGET_COUNT} + 1")
endif()

if (TARGET_COUNT GREATER 1 OR TARGET_COUNT EQUAL 0)
	message(FATAL_ERROR "It is required to select one target, either TARGET_SOLVERS_WITH_MESSAGING, or TARGET_SOLVERS_NO_MESSAGING or TARGET_JAVA_BINDING or TARGET_HYBRID_SOLVERS")
endif()

#############################################
# installation directories
#############################################
if (NOT TARGET_SOLVERS_WITH_MESSAGING)
	set(OPTION_EXE_DIRECTORY "bin" CACHE PATH "installation directory")
else (NOT TARGET_SOLVERS_WITH_MESSAGING)
	set(OPTION_EXE_DIRECTORY "../bin" CACHE PATH "installation directory")
endif (NOT TARGET_SOLVERS_WITH_MESSAGING)

set(OPTION_LIB_DIRECTORY "lib" CACHE PATH "library directory")


#############################################
#
#  Build universal binaries on Mac
#
##############################################
if (APPLE)
	option(MACOSX_UNIVERSAL_BINARIES "Build Universal Binaries" OFF)
	if (MACOSX_UNIVERSAL_BINARIES)
		set (ARCH_64bit FALSE)
		set(CMAKE_OSX_ARCHITECTURES "i386;x86_64")
	endif()
	option(MACOSX_BINARIES_32bit "Build i386 Binaries" OFF)
	if (MACOSX_BINARIES_32bit)
		set (ARCH_64bit FALSE)
		set(CMAKE_OSX_ARCHITECTURES "i386")
	endif()
	option(MACOSX_BINARIES_64bit "Build x86_64 Binaries" OFF)
	if (MACOSX_BINARIES_64bit) 
		set(CMAKE_OSX_ARCHITECTURES "x86_64")
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
if (NOT TARGET_JAVA_BINDING)
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

if (APPLE)
	find_program(IFORT_EXECUTABLE ifort)
endif()

if (LINUX_32bit_BINARIES)
	find_program(IFORT_EXECUTABLE ifort /opt/intel/fc/10.1.015/bin/)
elseif (LINUX_64bit_BINARIES)
	#find_program(IFORT_EXECUTABLE ifort /opt/intel/fce/10.1.015/bin/ /cm/shared/apps/intel/fce/10.1.015/bin/)
	find_program(IFORT_EXECUTABLE ifort /opt/intel/fce/10.1.015/bin/ )
endif()

if (IFORT_EXECUTABLE MATCHES IFORT_EXECUTABLE-NOTFOUND AND NOT CYGWIN)
	message("intel fortran compiler (ifort) not found")
	set(HY3S_SOLVERS off) 
endif()
endif (NOT TARGET_JAVA_BINDING)
	
######################################
#
# set proper compiler options
#
#######################################
if (MSVC)
	add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_CRT_NONSTDC_NO_DEPRECATE)
	
	if (TARGET_SOLVERS_WITH_MESSAGING OR TARGET_SOLVERS_NO_MESSAGING OR TARGET_HYBRID_SOLVERS)
		
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
		
else(MSVC)
	
	add_definitions(-DUNIX)
	if (TARGET_JAVA_BINDING AND NOT CYGWIN)
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC")
		set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")
	endif()
	
endif(MSVC)


if (TARGET_SOLVERS_WITH_MESSAGING)
	if (APPLE OR CYGWIN)
		message(FATAL_ERROR "TARGET_SOLVERS_WITH_MESSAGING is not supported on MAC OS X and cygwin")
	endif(APPLE OR CYGWIN)
	#don't need graphics for server side solvers
	set(OPTION_USE_OPENGL OFF CACHE BOOL "")
	
	set(APR_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/apr-1.4.6/include ${CMAKE_BINARY_DIR}/apr-1.4.6/include)
	set(ACTIVEMQ_CPP_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/activemq-cpp-library-3.4.4/src/main ${CMAKE_BINARY_DIR}/activemq-cpp-library-3.4.4)
	set(JMS_INCS ${ACTIVEMQ_CPP_INCLUDE_DIR} ${APR_INCLUDE_DIR})

	set(JMS_LIBS activemq-cpp aprutil-1 apriconv-1 apr-1)
	if (MSVC)
		add_definitions(-D_SECURE_SCL=1)
		list(APPEND JMS_LIBS ws2_32 Mswsock Rpcrt4)
	elseif (UNIX)
		set(JMS_DEFS -DHAVE_CONFIG_H -DLINUX=2 -D_REENTRANT -D_GNU_SOURCE)
		include(FindOpenSSL)
		if (NOT OPENSSL_FOUND)
			message(FATAL_ERROR, "OpenSSL not found.")
		endif()
		list(APPEND JMS_LIBS ${OPENSSL_LIBRARIES} crypt pthread)
	endif()
	
	add_subdirectory(apr-1.4.6)
	add_subdirectory(apr-iconv-1.2.1)
	add_subdirectory(apr-util-1.4.1)
	add_subdirectory(activemq-cpp-library-3.4.4)
	add_definitions(-DUSE_MESSAGING)
endif()

######################################
#
# Add subdirectories
# 
#######################################
 
if (TARGET_SOLVERS_WITH_MESSAGING OR TARGET_SOLVERS_NO_MESSAGING)
	
	add_subdirectory(vcommons)
	add_subdirectory(ExpressionParser)
	
	set(HDF5_EXTERNALLY_CONFIGURED 1)
	set(HDF5_LIB_DEPENDENCIES "w2_32")
	set(H5_HAVE_GETHOSTNAME 0)
#	if (CYGWIN) 
#		cmake_policy(SET CMP0000 OLD)
#	endif (CYGWIN) 
	add_subdirectory(${HDF_VERSION})
	if (${CHOMBO_SOLVER} AND NOT MSVC) 

		add_subdirectory(VCellChombo)
	endif()
#	include_directories(${ZLIB_VERSION})
#	include_directories(${CMAKE_BINARY_DIR}/${ZLIB_VERSION})
	##add_subdirectory(${ZLIB_VERSION})

	if (${SUNDIALS_SOLVER})
		add_subdirectory(sundials)
		add_subdirectory(IDAWin)
	endif (${SUNDIALS_SOLVER})
	#add_subdirectory(sundials)
	if (NOT APPLE)
		add_subdirectory(blas)
	endif()


	if (${HY3S_SOLVERS} AND NOT MACOSX_BINARIES_ppc AND NOT CYGWIN)
		add_subdirectory(Hy3S)
		add_subdirectory(netcdf-3.6.2)
	endif()
	if (${NFSIM_EXE} AND NOT MSVC)
		add_subdirectory(NFsim_v1.11) 
	endif( )
	add_subdirectory(PCGPack) 
	add_subdirectory(qhull)
	if (MSVC)
#		add_subdirectory(glut-3.7.6)
		if (NOT ZLIB_FOUND)
			add_subdirectory(zlib)
		endif()
	endif()
	if (NEW_SMOLDYN)
		set(SMOL_DIR smoldyn-2.37)
		set(SMOL_LIB smoldyn_static)
		set(VCELL_SMOL_INC ${CMAKE_CURRENT_SOURCE_DIR}/bridgeVCellSmoldyn)
		set(VCELL_SMOL_DEP_LIBS bvcellsmoldyn hdf5_cpp hdf5 ${JMS_LIBS} zip)
		add_subdirectory(${SMOL_DIR})
		add_subdirectory(bridgeVCellSmoldyn)
	else (NEW_SMOLDYN)
		set(SMOL_LIB smoldynlib)
		add_subdirectory(smoldyn) 
	endif (NEW_SMOLDYN)
	if (${STOCHASTIC_SOLVER})
		add_subdirectory(Stochastic)
	endif (${STOCHASTIC_SOLVER})
	add_subdirectory(unzip) 
	if (${FV_SOLVER})
		add_subdirectory(VCell) 
	endif (${FV_SOLVER})
	add_subdirectory(zip)
endif() 

if (TARGET_JAVA_BINDING)
	if (CYGWIN)
		LocalJNI( )
#		set(JAVA_INCLUDE_PATH ${CMAKE_SOURCE_DIR}/JNI/Linux/include)
#		set(JAVA_AWT_INCLUDE_PATH ${JAVA_INCLUDE_PATH})
	endif (CYGWIN)
	if (LINUX)
		find_package(JNI)
	endif (LINUX)
		

	add_subdirectory(ExpressionParser)
	add_subdirectory(IDAWin)
	add_subdirectory(sundials)
	add_subdirectory(fsqp)
	add_subdirectory(tinyxml)
	add_subdirectory(JavaBinding)
	add_subdirectory(Optimization2)
endif(TARGET_JAVA_BINDING)

if (TARGET_HYBRID_SOLVERS)
	#add_subdirectory(Hy3S)
	#add_subdirectory(netcdf-3.6.2)
	set(HDF5_EXTERNALLY_CONFIGURED 1)
	#add_subdirectory(blas)
	#add_subdirectory(zip)
	#add_subdirectory(${HDF_VERSION})
	#add_subdirectory(smoldyn)
endif()

set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}")
