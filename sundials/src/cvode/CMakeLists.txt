project(sundials_cvode)    
    
file (GLOB SRC_FILES *.c)    
include_directories(../../include ../)

add_library(sundials_cvode STATIC ${SRC_FILES})    
install(TARGETS sundials_cvode 
	ARCHIVE DESTINATION bin)