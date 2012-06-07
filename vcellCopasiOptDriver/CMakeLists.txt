cmake_minimum_required(VERSION 2.8)

if(DEFINED CMAKE_BUILD_TYPE)
   SET(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE} CACHE STRING "Choose the type of build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel.")
else()
   SET(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel.")
endif()

project(vcellCopasiOptDriver)

include (FindJNI)

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

set(COPASIDIR ${CMAKE_CURRENT_SOURCE_DIR}/../../Copasi/copasi-34-src)
set(COPASITHIRDLIBDIR ${CMAKE_CURRENT_SOURCE_DIR}/../../Copasi/thirdparty_libs)
include_directories(${COPASIDIR}
	${COPASIDIR}/copasi 
	${COPASIDIR}/copasi/commandline 
	${COPASIDIR}/copasi/compareExpressions 
	${COPASIDIR}/copasi/utilities
	${COPASIDIR}/../libsbml-4.3.1/include
	${COPASIDIR}/../expat-2.0.1/include
	${CMAKE_CURRENT_SOURCE_DIR}/../Optimization2
	${CMAKE_CURRENT_SOURCE_DIR}/../tinyxml
	${JAVA_INCLUDE_PATH}
	${JAVA_INCLUDE_PATH2}
	${JAVA_AWT_INCLUDE_PATH}
)
add_definitions(-DWITH_CSPMETHOD -DXML_STATIC -DLIBSBML_STATIC -DRAPTOR_STATIC)
if (MSVC)
	add_definitions(-DUSE_CLAPACK)
	link_directories(${COPASIDIR}/copasi/tmp/debug ${COPASITHIRDLIBDIR}/win)
elseif (APPLE)
	set(CMAKE_OSX_ARCHITECTURES "i386;x86_64")
	add_definitions(-DDarwin)
	link_directories(${COPASITHIRDLIBDIR}/mac ${COPASIDIR}/copasi/libs/mac)
elseif (LINUX)
	add_definitions(-DLinux -DUSE_CLAPACK)
	if (LINUX_64bit_BINARIES)
		link_directories(${COPASIDIR}/copasi/libs/linux/m64 ${COPASITHIRDLIBDIR}/linux/m64)
	elseif (LINUX_32bit_BINARIES)
		link_directories(${COPASIDIR}/copasi/libs/linux/m32 ${COPASITHIRDLIBDIR}/linux/m32)
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
)

if (APPLE)
	list(APPEND DEP_LIBS 
		lapack
		blas
		${COPASITHIRDLIBDIR}/mac/libexpat.a
		/System/Library/Frameworks/Carbon.framework
	)
elseif (MSVC)
	list(APPEND DEP_LIBS 
		lapackd
		libf2cd
		blasd
		libexpatd
	)
elseif  (LINUX)
	list(APPEND DEP_LIBS 
		lapack
		f2c
		blas
		expat
	)
endif()
target_link_libraries(vcellCopasiOptDriver ${DEP_LIBS})

set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}")
if(MSVC)
	install(TARGETS vcellCopasiOptDriver RUNTIME DESTINATION bin)
else()
	install(TARGETS vcellCopasiOptDriver LIBRARY DESTINATION bin)
endif()
