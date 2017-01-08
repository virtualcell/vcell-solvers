cmake_minimum_required(VERSION 2.8)

if (UNIX)
#  set(ENV{CC} "gcc")
#  set(ENV{CXX} "g++")
endif (UNIX)

project(MovingBoundarySolver)
set(SUPPORTS_MATLAB ON)
set(SUPPORTS_MOVING_BOUNDARY ON)
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
	message("gnu ${CMAKE_CXX_COMPILER_VERSION}")
	if (CMAKE_CXX_COMPILER_VERSION VERSION_EQUAL "4.8")
		set(SUPPORTS_MATLAB OFF)
	endif( )
	if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS "4.8")
		set(SUPPORTS_MOVING_BOUNDARY OFF)
	endif( )
	if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS "4.7")
		message(FATAL_ERROR "GCC 4.7 or greater required (see /share/apps/vcell2/buildsupport/gcc)") 
	endif( )
endif( )


if (SUPPORTS_MATLAB)
	option(BUILD_INCLUDE_MATLAB_DIRS "Include matlab integration directory" OFF)
endif (SUPPORTS_MATLAB)

if(DEFINED CMAKE_BUILD_TYPE)
   SET(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE} CACHE STRING "Choose the type of build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel.")
else()
   SET(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel.")
endif()

if (UNIX)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
  set(CXX_FLAGS "${CXX_FLAGS} -std=c++11")
  add_definitions("-fPIC")
endif (UNIX) 

if(CYGWIN)
    add_definitions(-U__STRICT_ANSI__)
    remove_definitions("-fPIC")
endif(CYGWIN)

include (svnversion.txt)
message(STATUS "Operating system ${CMAKE_SYSTEM_NAME}")
# per http://code.google.com/p/googletest/issues/detail?id=412 
if (MSVC) # VS2012 doesn't support correctly the tuples yet
		add_definitions( /D _VARIADIC_MAX=10 )
		add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_CRT_NONSTDC_NO_DEPRECATE)
endif(MSVC)

message(STATUS "c++ flags ${CMAKE_CXX_FLAGS}")

#OVERRIDE HDF5 default because we always need the cpp bindings
OPTION (HDF5_BUILD_CPP_LIB  "Build HDF5 C++ Library" ON)
set(HDF_VERSION "hdf5-1.8.14")
set(HDF5_BUILD_CPP_LIB ON)
set(HDF5_EXTERNALLY_CONFIGURED 1)
set(HDF5_LIB_DEPENDENCIES "w2_32")
set(H5_HAVE_GETHOSTNAME 0)
set(GTEST_VERSION "gtest-1.7.0")
add_subdirectory(vcommons)
add_subdirectory(${HDF_VERSION})
add_subdirectory(FronTierLib)
add_subdirectory(${GTEST_VERSION})
add_subdirectory(ExpressionParser)
if (SUPPORTS_MOVING_BOUNDARY)
	if (UNIX)
		find_file(SQL3 sqlite3.h)
		if (SQL3 MATCHES SQL3-NOTFOUND)
			message("Warning!!! sqlite3 not found in the system, using our own copy")
			include_directories(sqlite)
			set(SQL3 sqlite/sqlite3.h)
		endif (SQL3 MATCHES SQL3-NOTFOUND)
		set(APR_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/apr-1.4.6/include ${CMAKE_BINARY_DIR}/apr-1.4.6/include)
		set(ACTIVEMQ_CPP_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/activemq-cpp-library-3.4.4/src/main ${CMAKE_BINARY_DIR}/activemq-cpp-library-3.4.4)
		set(JMS_INCS ${ACTIVEMQ_CPP_INCLUDE_DIR} ${APR_INCLUDE_DIR})
		set(JMS_LIBS activemq-cpp aprutil-1 apriconv-1 apr-1)

		include(FindOpenSSL)
		if (NOT OPENSSL_FOUND)
			message(FATAL_ERROR, "OpenSSL not found.")
		endif()
		list(APPEND JMS_LIBS ${OPENSSL_LIBRARIES} crypt pthread)

		add_subdirectory(apr-1.4.6)
		add_subdirectory(apr-iconv-1.2.1)
		add_subdirectory(apr-util-1.4.1)
		add_subdirectory(activemq-cpp-library-3.4.4)
		add_definitions(-DUSE_MESSAGING)
	endif (UNIX)
	add_subdirectory(Solver)
#	add_subdirectory(Tests)
endif (SUPPORTS_MOVING_BOUNDARY)
if (BUILD_INCLUDE_MATLAB_DIRS)
	message(STATUS "Matlab link directories active")
	add_subdirectory(matlab)
endif (BUILD_INCLUDE_MATLAB_DIRS)
if (MSVC)
	add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_CRT_NONSTDC_NO_DEPRECATE)
		
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
		
endif (MSVC)

set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}")
