#included by smoldyn CMakeLists.txt


if (NOT VCELL_BRIDGE_INCLUDED)
	message(STATUS "First include")


	option(OPTION_VCELL "Compile Smoldyn for VCell" ON)
	option(OPTION_NSV "Compile Smoldyn with NextSubvolume functionality" OFF)
	option(OPTION_USE_OPENGL "Build with OpenGL support" OFF)
	option(OPTION_USE_ZLIB "Build with Zlib support" ON)
	option(OPTION_USE_LIBTIFF "Build with LibTiff support" OFF)
	option(OPTION_USE_ICONV "Build with Libiconv support" OFF)

	SET(HAVE_ZLIB TRUE)
	SET(VCELL_BRIDGE_INCLUDED ON)
else (NOT VCELL_BRIDGE_INCLUDED)
	message(STATUS "SECOND include")

set(VCELL_SMOL_MAIN ${CMAKE_CURRENT_LIST_DIR}/vcell_smoldyn.cpp)
file (GLOB VCELL_HEADER_FILES *.h)    
file (GLOB VCELL_SMOL_SOURCES ${CMAKE_CURRENT_LIST_DIR}/*.cpp)
list(REMOVE_ITEM VCELL_SMOL_SOURCES ${VCELL_SMOL_MAIN}) 

include_directories(${VCELL_SMOL_INC})

add_definitions(-D_CRT_SECURE_NO_WARNINGS)
ADD_DEFINITIONS(-DVCELL)
ADD_DEFINITIONS(-DVCELL_HYBRID)
include_directories(../IDAWin ../ExpressionParser ../VCell/include ${CMAKE_CURRENT_BINARY_DIR})	

include_directories(${CMAKE_BINARY_DIR}/${HDF_VERSION})
include_directories(../${HDF_VERSION}/src)
include_directories(../${HDF_VERSION}/c++/src)

include_directories(${CMAKE_BINARY_DIR}/${SMOL_DIR})
include_directories(../${SMOL_DIR}/source/lib)
include_directories(../${SMOL_DIR}/source/Smoldyn)
include_directories(${JMS_INCS})
include_directories(${CMAKE_CURRENT_LIST_DIR})
set(VCELL_SMOL_DEP_LIBS vcell pcgwrapper ExpressionParser hdf5_cpp hdf5 IDAWin PCGPack qhull sundials_cvode sundials_ida sundials_nvecserial sundials ${JMS_LIBS} zip unzip ${SMOL_LIB} blas)
if (MSVC AND OPTION_USE_OPENGL)
	link_directories(${CMAKE_CURRENT_SOURCE_DIR}/../smoldyn/windows/glut-3.7.6/)
	if (ARCH_64bit)
		list(APPEND VCELL_SMOL_DEP_LIBSS glut64)
	else()
		list(APPEND VCELL_SMOL_DEP_LIBSS glut32)
	endif()
endif()
if (ARCH_64bit)
	set(SMOLDYN_EXE ${SMOLDYN_EXE}_x64)	
endif (ARCH_64bit)

if (ZLIB_FOUND)
	list(APPEND VCELL_SMOL_DEP_LIBSS ${ZLIB_LIBRARIES})
else()
	list(APPEND VCELL_SMOL_DEP_LIBSS zlib)
endif()
if (APPLE)
	list(APPEND VCELL_SMOL_DEP_LIBSS gfortran)
endif()

endif (NOT VCELL_BRIDGE_INCLUDED)
