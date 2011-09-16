project(sundials_nvecserial)    
    
file (GLOB SRC_FILES *.c)    
include_directories(../../include ../)

add_library(sundials_nvecserial STATIC ${SRC_FILES})    
