project(sundials_ida)    
    
file (GLOB SRC_FILES *.c)    
include_directories(../../include ..)

add_library(sundials_ida STATIC ${SRC_FILES})    
