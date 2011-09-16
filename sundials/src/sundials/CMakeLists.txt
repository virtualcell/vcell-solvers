project(sundials_lib)    
    
file (GLOB SRC_FILES *.c)    
include_directories(../../include ..)
    
add_library(sundials STATIC ${SRC_FILES})    
