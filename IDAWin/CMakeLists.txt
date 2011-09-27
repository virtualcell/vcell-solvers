project(IDAWin)

set (SRC_FILES 
	OdeResultSet.cpp
	StoppedByUserException.cpp	
	VCellCVodeSolver.cpp
	VCellIDASolver.cpp
	VCellSundialsSolver.cpp
)

set (EXE_SRC_FILES
	SundialsSolverStandalone.cpp
)

include_directories(../ExpressionParser ../sundials/include ../sundials/src)

set(DEP_LIBS IDAWin ExpressionParser sundials sundials_nvecserial sundials_cvode sundials_ida ${SONICMQ_LIBS})
add_library(IDAWin STATIC ${SRC_FILES})
if (TARGET_SOLVERS_SERVER_SIDE OR TARGET_SOLVERS_CLIENT_SIDE)
	if (TARGET_SOLVERS_SERVER_SIDE)
		list(APPEND EXE_SRC_FILES ../VCell/src/SimulationMessaging.cpp)
	endif()
	add_executable(SundialsSolverStandalone ${EXE_SRC_FILES})	
	target_link_libraries(SundialsSolverStandalone ${DEP_LIBS})
	add_dependencies(SundialsSolverStandalone ExpressionParser IDAWin sundials sundials_nvecserial sundials_cvode sundials_ida)
endif()