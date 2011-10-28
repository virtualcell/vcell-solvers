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
	add_executable(VCellStoch ${SRC_FILES})	
	target_link_libraries(VCellStoch ${DEP_LIBS})
	add_dependencies(VCellStoch ExpressionParser)
	install(TARGETS VCellStoch
        RUNTIME DESTINATION bin)
endif()