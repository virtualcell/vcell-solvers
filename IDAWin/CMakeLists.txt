project(IDAWin)

set (SRC_FILES 
	OdeResultSet.cpp
	StoppedByUserException.cpp	
	VCellCVodeSolver.cpp
	VCellIDASolver.cpp
	VCellSundialsSolver.cpp
)

include_directories(../ExpressionParser ../sundials/include ../sundials/src)
set(DEP_LIBS ExpressionParser sundials sundials_nvecserial sundials_cvode sundials_ida ${SONICMQ_LIBS})
if (TARGET_SOLVERS_SERVER_SIDE OR TARGET_SOLVERS_CLIENT_SIDE)
	list(APPEND SRC_FILES SundialsSolverStandalone.cpp)
	if (TARGET_SOLVERS_SERVER_SIDE)
		list(APPEND SRC_FILES ../VCell/src/SimulationMessaging.cpp)
	endif()
	add_executable(SundialsSolverStandalone ${SRC_FILES})	
	target_link_libraries(SundialsSolverStandalone ${DEP_LIBS})
	add_dependencies(SundialsSolverStandalone ExpressionParser sundials sundials_nvecserial sundials_cvode sundials_ida)
endif()

if (TARGET_JAVA_BINDING)
	add_library(idawin STATIC ${SRC_FILES})
endif()
