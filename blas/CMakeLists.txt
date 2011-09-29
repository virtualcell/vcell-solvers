project(blas Fortran)    
    
file (GLOB SRC_FILES *.f)    
    
add_library(blas STATIC ${SRC_FILES})