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
	set(DEP_LIBS ExpressionParser ${SONICMQ_LIBS})
	include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../ExpressionParser)
	if (TARGET_SOLVERS_WITH_MESSAGING)
		include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../VCell/include)
		list(APPEND SRC_FILES ../VCell/src/SimulationMessaging.cpp)
	endif()
	add_executable(Stochastic ${SRC_FILES})	
	target_link_libraries(Stochastic ${DEP_LIBS})
	add_dependencies(Stochastic ExpressionParser)
	install(TARGETS Stochastic
        RUNTIME DESTINATION bin)
endif()