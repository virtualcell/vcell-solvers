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


#set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
#include_directories(${JMS_INCS})
#add_definitions(${JMS_DEFS})
#set(DEP_LIBS ExpressionParser ${VCELLMESSAGING_LIBS})
#include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../ExpressionParser)
#include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../vcommons/include)
#include_directories(${VCELL_MESSAGING_INCLUDE_DIR})
if (APPLE)
	link_directories("/usr/local/Cellar/hdf5/1.10.5_1/lib")
endif()
add_executable(${EXE_FILE} ${SRC_FILES})
#target_link_libraries(${EXE_FILE} ExpressionParser vcommons vcellmessaging ${CMAKE_SOURCE_DIR}/mylibs/libhdf5_serial.so.10)
target_link_libraries(${EXE_FILE} ExpressionParser vcommons vcellmessaging hdf5)
#add_dependencies(${EXE_FILE} ExpressionParser)
install(TARGETS ${EXE_FILE} RUNTIME DESTINATION ${OPTION_EXE_DIRECTORY})
