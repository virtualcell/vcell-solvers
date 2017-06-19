project(netcdf Fortran)

include_directories(libsrc cxx)

if (MSVC) 
add_definitions(-DPowerStationFortran)
include_directories(win32)
else()
include_directories(linux)
endif()

add_subdirectory(libsrc)
#add_subdirectory(cxx)
add_subdirectory(fortran)
add_subdirectory(f90)