project(ExpressionParser)    

file (GLOB HEADER_FILES *.h)    
file (GLOB SRC_FILES *.cpp)    
    
add_library(ExpressionParser STATIC ${SRC_FILES} ${HEADER_FILES})
install(TARGETS ExpressionParser 
	ARCHIVE DESTINATION bin)