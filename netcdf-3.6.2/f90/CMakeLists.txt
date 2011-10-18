project(netcdf_f90 Fortran)

set(SRC_FILES 
	netcdf.f90
	typeSizes.f90
)

set (CMAKE_Fortran_COMPILER ${IFORT_EXECUTABLE})
if (MSVC)
	set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /iface:mixed_str_len_arg /check:bounds")
endif()

add_library(netcdf_f90 STATIC ${SRC_FILES})

