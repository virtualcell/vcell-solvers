project(Stochastic)		

set (SRC_FILES 
	VCellStoch/Gibson.cpp
	VCellStoch/IndexedTree.cpp
	VCellStoch/Jump.cpp
	VCellStoch/StochModel.cpp
	VCellStoch/StochVar.cpp
	VCellStoch/VCellStoch.cpp
)

if (TARGET_SOLVERS_WITH_MESSAGING OR TARGET_SOLVERS_NO_MESSAGING)

	set(EXE_FILE VCellStoch)
	if (ARCH_64bit) 
		set(EXE_FILE ${EXE_FILE}_x64)
	endif()

	include_directories(${JMS_INCS})
	add_definitions(${JMS_DEFS})
	set(DEP_LIBS ExpressionParser ${JMS_LIBS})
	include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../ExpressionParser)
	include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../vcommons/include)
	if (TARGET_SOLVERS_WITH_MESSAGING)
		include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../VCell/include)
		list(APPEND SRC_FILES ../VCell/src/SimulationMessaging.cpp)
	endif()
	add_executable(${EXE_FILE} ${SRC_FILES})	
	target_link_libraries(${EXE_FILE} ${DEP_LIBS})
	add_dependencies(${EXE_FILE} ExpressionParser)
	install(TARGETS ${EXE_FILE} RUNTIME DESTINATION ${OPTION_EXE_DIRECTORY})
endif()
