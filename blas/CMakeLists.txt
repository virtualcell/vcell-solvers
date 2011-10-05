project(blas Fortran)    

if (MSVC)
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /names:lowercase")
endif()

file (GLOB SRC_FILES *.f)    
add_library(blas STATIC ${SRC_FILES})