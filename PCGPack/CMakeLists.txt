project(PCGPack Fortran)    
    
file (GLOB SRC_FILES *.f)    
    
add_library(PCGPack STATIC ${SRC_FILES})