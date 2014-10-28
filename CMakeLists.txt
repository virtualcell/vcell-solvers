cmake_minimum_required(VERSION 2.8)

if(DEFINED CMAKE_BUILD_TYPE)
   SET(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE} CACHE STRING "Choose the type of build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel.")
else()
   SET(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel.")
endif()

if (UNIX)
  set(CMAKE_C_COMPILER "gcc")
  set(CMAKE_CXX_COMPILER "g++")
endif (UNIX)

project(MovingBoundarySolver)

if (UNIX)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
  set(CXX_FLAGS "${CXX_FLAGS} -std=c++11")
endif (UNIX)

include (svnversion.txt)
message("sys ${CMAKE_SYSTEM_NAME}")
# per http://code.google.com/p/googletest/issues/detail?id=412 
if (MSVC) # VS2012 doesn't support correctly the tuples yet
		add_definitions( /D _VARIADIC_MAX=10 )
		add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_CRT_NONSTDC_NO_DEPRECATE)
endif(MSVC)

message("flags ${CMAKE_CXX_FLAGS}")
OPTION (HDF5_BUILD_CPP_LIB  "Build HDF5 C++ Library" ON)
set(HDF_VERSION "hdf5-1.8.11")
set(HDF5_BUILD_CPP_LIB ON)
set(HDF5_EXTERNALLY_CONFIGURED 1)
set(HDF5_LIB_DEPENDENCIES "w2_32")
set(H5_HAVE_GETHOSTNAME 0)
set(GTEST_VERSION "gtest-1.7.0")
add_subdirectory(${HDF_VERSION})
add_subdirectory(FronTierLib)
add_subdirectory(Solver)
add_subdirectory(${GTEST_VERSION})
add_subdirectory(ExpressionParser)
add_subdirectory(Tests)
#add_subdirectory(matlabLink)
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
