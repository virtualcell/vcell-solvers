project(ExpressionParser)    
    
file (GLOB SRC_FILES *.cpp)    
    
add_library(ExpressionParser STATIC ${SRC_FILES})
install(TARGETS ExpressionParser 
	ARCHIVE DESTINATION bin)