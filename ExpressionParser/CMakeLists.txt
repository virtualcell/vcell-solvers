project(ExpressionParser)    

file (GLOB HEADER_FILES *.h)    
file (GLOB SRC_FILES *.cpp)  

list (REMOVE_ITEM HEADER_FILES ${CMAKE_CURRENT_SOURCE_DIR}/ExpressionTest.h) 
list (REMOVE_ITEM SRC_FILES ${CMAKE_CURRENT_SOURCE_DIR}/ExpressionTest.cpp) 
   
add_library(ExpressionParser STATIC ${SRC_FILES} ${HEADER_FILES})
install(TARGETS ExpressionParser 
	ARCHIVE DESTINATION bin)