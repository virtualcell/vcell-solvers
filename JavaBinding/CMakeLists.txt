cmake_minimum_required(VERSION 2.8)
project(JavaBinding)

include (FindJNI)

set (SRC_FILES 
	cbit_vcell_solvers_NativeCVODESolver.cpp
	cbit_vcell_solvers_NativeIDASolver.cpp
	org_vcell_optimization_NativeOptSolver.cpp
)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../ExpressionParser
	${CMAKE_CURRENT_SOURCE_DIR}/../IDAWin
	${CMAKE_CURRENT_SOURCE_DIR}/../Optimization2
	${CMAKE_CURRENT_SOURCE_DIR}/../sundials/include 
	${CMAKE_CURRENT_SOURCE_DIR}/../sundials/src
	${CMAKE_CURRENT_SOURCE_DIR}/../tinyxml
	${JAVA_INCLUDE_PATH}
	${JAVA_AWT_INCLUDE_PATH}
)

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
target_link_libraries(NativeSolvers ${DEP_LIBS})
