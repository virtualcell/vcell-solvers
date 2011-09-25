cmake_minimum_required(VERSION 2.8)
project(vcellCopasiOptDriver)

set (SRC_FILES 
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
add_definitions(-DDarwin -DWITH_CSPMETHOD -DXML_STATIC -DLIBSBML_STATIC -DRAPTOR_STATIC)
if (MSVC)
	include_directories(${JNIDIR}/Win32/include ${JNIDIR}/Win32/include/win32)
elseif (APPLE)
	set(CMAKE_OSX_ARCHITECTURES "i386;ppc;x86_64")
	include_directories(${JNIDIR}/MacOSX/i386)
elseif (LINUX)
	include_directories(${JNIDIR}/Linux/include ${JNIDIR}/Linux/include/linux)
endif ()
link_directories(${CMAKE_CURRENT_SOURCE_DIR}/../tinyxml ${COPASIDIR}/copasi/libs ${CMAKE_CURRENT_SOURCE_DIR}/../Copasi/thirdparty_libs/mac)

add_library(vcellCopasiOptDriver SHARED ${SRC_FILES})
set (DEP_LIBS 
	tinyxml 
	commandline
	compareExpressions
	copasiDM
	copasiXML
	crosssection
	elementaryFluxModes
	fitting
	function
	layout
	lyap
	MIRIAM
	model
	moieties
	odepack++
	optimization
	plot
	randomGenerator
	report
	sbmlimport
	sbmlunit
	scan
	sensitivities
	steadystate
	tex
	trajectory
	tss
	tssanalysis
	utilities
	sbml
	raptor
	expat
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
