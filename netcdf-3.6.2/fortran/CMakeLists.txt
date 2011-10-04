project(netcdf_for)

file (GLOB SRC_FILES *.c)

include_directories(../libsrc)

if (MSVC) 
include_directories(win32)
else()
include_directories(linux)
endif()

add_library(netcdf_for STATIC ${SRC_FILES})