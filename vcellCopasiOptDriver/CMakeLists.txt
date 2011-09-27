cmake_minimum_required(VERSION 2.8)
project(vcellCopasiOptDriver)

set(LINUX FALSE)
if (${CMAKE_SYSTEM_NAME} MATCHES Linux)
	set(LINUX TRUE)
endif()

set (ARCH_64bit FALSE)
if (CMAKE_SIZEOF_VOID_P EQUAL 8)
	set (ARCH_64bit TRUE)
endif()

if (LINUX)
	option(LINUX_32bit_BINARIES "Build 32bit Linux BINARIES" OFF)
	option(LINUX_64bit_BINARIES "Build 64bit Linux BINARIES" ON)

	if (LINUX_32bit_BINARIES AND LINUX_64bit_BINARIES)
		message(FATAL_ERROR "It is required to select either 32bit or 64bit Linux Binaries, not both")
	endif()
	
	if (LINUX_32bit_BINARIES) 
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -m32")
	endif()
	
	if (LINUX_64bit_BINARIES) 
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -m64")
	endif()
endif()

file (GLOB SRC_FILES ../tinyxml/*.cpp)

list (APPEND SRC_FILES
	CopasiOptDriver.cpp
	OptXMLParser.cpp
	org_vcell_optimization_CopasiOptimizationSolver.cpp
	VCellCallback.cpp
	../Optimization2/OptResultSet.cpp
	../Optimization2/OptXmlWriter2.cpp
)

set(JNIDIR ${CMAKE_CURRENT_SOURCE_DIR}/../JNI/)
set(COPASIDIR ${CMAKE_CURRENT_SOURCE_DIR}/../Copasi/copasi-34-src)
include_directories(${COPASIDIR} 
	${COPASIDIR}/copasi 
	${COPASIDIR}/copasi/commandline 
	${COPASIDIR}/copasi/compareExpressions 
	${COPASIDIR}/copasi/utilities 
	${CMAKE_CURRENT_SOURCE_DIR}/../Optimization2
	${CMAKE_CURRENT_SOURCE_DIR}/../tinyxml
)
add_definitions(-DWITH_CSPMETHOD -DXML_STATIC -DLIBSBML_STATIC -DRAPTOR_STATIC)
if (MSVC)
	add_definitions(-DLinux -DUSE_CLAPACK)
	include_directories(${JNIDIR}/Win32/include ${JNIDIR}/Win32/include/win32)
elseif (APPLE)
	set(CMAKE_OSX_ARCHITECTURES "i386;ppc;x86_64")
	add_definitions(-DDarwin)
	include_directories(${JNIDIR}/MacOSX/i386)
	link_directories(${CMAKE_CURRENT_SOURCE_DIR}/../Copasi/thirdparty_libs/mac ${COPASIDIR}/copasi/libs/mac)
elseif (LINUX)
	include_directories(${JNIDIR}/Linux/include ${JNIDIR}/Linux/include/linux)
	if (LINUX_64bit_BINARIES)
		link_directories(${COPASIDIR}/copasi/libs/linux/m64 ${CMAKE_CURRENT_SOURCE_DIR}/../Copasi/thirdparty_libs/linux/m64)
	elseif (LINUX_32bit_BINARIES)
		link_directories(${COPASIDIR}/copasi/libs/linux/m32 ${CMAKE_CURRENT_SOURCE_DIR}/../Copasi/thirdparty_libs/linux/m32)
	endif()
endif ()

add_library(vcellCopasiOptDriver SHARED ${SRC_FILES})
set (DEP_LIBS
	optimization
	copasiDM
	copasiXML
	crosssection
	elementaryFluxModes
	fitting
	function
	lyap
	moieties
	plot
	randomGenerator
	scan
	sensitivities
	steadystate
	tex
	trajectory
	tss
	tssanalysis
	odepack++
	model
	sbmlimport
	sbmlunit
	MIRIAM
	layout
	utilities
 	report
	compareExpressions
	commandline
	sbml
	raptor
	expat
	lapack
	f2c
	blas
)
if (MSVC)
elseif (APPLE)
	list(APPEND DEP_LIBS 
		lapack
		blas
		xml2
		xslt
		curl
		/System/Library/Frameworks/Carbon.framework
	)
endif()
target_link_libraries(vcellCopasiOptDriver ${DEP_LIBS})
