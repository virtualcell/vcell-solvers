project(bridgeVCellSmoldyn C CXX)

file (GLOB HEADER_FILES *.h)    
file (GLOB SRC_FILES *.cpp *c)  

add_definitions(-D_CRT_SECURE_NO_WARNINGS)
ADD_DEFINITIONS(-DVCELL)
include_directories(../IDAWin ../ExpressionParser ../VCell/include ${CMAKE_CURRENT_BINARY_DIR})	

include_directories(${CMAKE_BINARY_DIR}/${HDF_VERSION})
include_directories(../${HDF_VERSION}/src)
include_directories(../${HDF_VERSION}/c++/src)

include_directories(${CMAKE_BINARY_DIR}/${SMOL_DIR})
include_directories(../${SMOL_DIR}/source/lib)
include_directories(../${SMOL_DIR}/source/Smoldyn)
include_directories(${JMS_INCS})
   
add_library(bvcellsmoldyn STATIC ${SRC_FILES} ${HEADER_FILES})
install(TARGETS bvcellsmoldyn ARCHIVE DESTINATION bin)
