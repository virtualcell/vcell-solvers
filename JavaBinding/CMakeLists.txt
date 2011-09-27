cmake_minimum_required(VERSION 2.8)
project(JavaBinding)

set (SRC_FILES 
	cbit_vcell_solvers_NativeCVODESolver.cpp
	cbit_vcell_solvers_NativeIDASolver.cpp
	org_vcell_optimization_NativeOptSolver.cpp
)

set(JNIDIR ${CMAKE_CURRENT_SOURCE_DIR}/../JNI/)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../ExpressionParser
	${CMAKE_CURRENT_SOURCE_DIR}/../IDAWin
	${CMAKE_CURRENT_SOURCE_DIR}/../Optimization2
	${CMAKE_CURRENT_SOURCE_DIR}/../sundials/include 
	${CMAKE_CURRENT_SOURCE_DIR}/../sundials/src
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

add_library(NativeSolvers SHARED ${SRC_FILES})
set (DEP_LIBS
	vcellOpt
	fsqp
	tinyxml
	IDAWin
	sundials_ida
	sundials_cvode
	sundials_nvecserial
	sundials
	ExpressionParser
)
if (MSVC)
elseif (APPLE)
endif()
target_link_libraries(NativeSolvers ${DEP_LIBS})
