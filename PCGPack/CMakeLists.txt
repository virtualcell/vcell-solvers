project(PCGPack Fortran)    
   
if (MSVC)
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /names:lowercase")
elseif (UNIX)
	set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static")
endif()
    
file (GLOB SRC_FILES *.f)   
add_library(PCGPack STATIC ${SRC_FILES})