project(IDAWin)

set (SRC_FILES 
	OdeResultSet.cpp
	StoppedByUserException.cpp	
	VCellCVodeSolver.cpp
	VCellIDASolver.cpp
	VCellSundialsSolver.cpp
)
set (HEADER_FILES 
	OdeResultSet.h
	StoppedByUserException.h	
	VCellCVodeSolver.h
	VCellIDASolver.h
	VCellSundialsSolver.h
)

set (EXE_SRC_FILES
	SundialsSolverStandalone.cpp
)

include_directories(../ExpressionParser ../sundials/include ../sundials/src ../VCell/include)

set(DEP_LIBS IDAWin ExpressionParser sundials_cvode sundials_ida sundials_nvecserial sundials ${JMS_LIBS})
include_directories(${JMS_INCS})
add_definitions(${JMS_DEFS})
add_library(IDAWin STATIC ${SRC_FILES} ${HEADER_FILES})
if (TARGET_SOLVERS_WITH_MESSAGING OR TARGET_SOLVERS_NO_MESSAGING)
	set(EXE_FILE SundialsSolverStandalone)
	if (ARCH_64bit) 
		set(EXE_FILE ${EXE_FILE}_x64)
	elseif (MACOSX_BINARIES_ppc)
		set(EXE_FILE ${EXE_FILE}_ppc)
	endif()

	if (TARGET_SOLVERS_WITH_MESSAGING)
		list(APPEND EXE_SRC_FILES ../VCell/src/SimulationMessaging.cpp)
	endif()
	add_executable(${EXE_FILE} ${EXE_SRC_FILES})	
	target_link_libraries(${EXE_FILE} ${DEP_LIBS})
	add_dependencies(${EXE_FILE} ExpressionParser IDAWin sundials sundials_nvecserial sundials_cvode sundials_ida)
	
	install(TARGETS ${EXE_FILE} RUNTIME DESTINATION ${OPTION_EXE_DIRECTORY})
endif()
install(TARGETS IDAWin ARCHIVE DESTINATION bin)