project(Stochastic)		

set (SRC_FILES 
	VCellStoch/Gibson.cpp
	VCellStoch/IndexedTree.cpp
	VCellStoch/Jump.cpp
	VCellStoch/StochModel.cpp
	VCellStoch/StochVar.cpp
	VCellStoch/VCellStoch.cpp
)

set(EXE_FILE VCellStoch)
if (ARCH_64bit) 
	set(EXE_FILE ${EXE_FILE}_x64)
endif()

#include_directories(${JMS_INCS})
#add_definitions(${JMS_DEFS})
#set(DEP_LIBS ExpressionParser ${VCELLMESSAGING_LIBS})
#include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../ExpressionParser)
#include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../vcommons/include)
#include_directories(${VCELL_MESSAGING_INCLUDE_DIR})
add_executable(${EXE_FILE} ${SRC_FILES})	
target_link_libraries(${EXE_FILE} ExpressionParser vcommons vcellmessaging)
#add_dependencies(${EXE_FILE} ExpressionParser)
install(TARGETS ${EXE_FILE} RUNTIME DESTINATION ${OPTION_EXE_DIRECTORY})
