project(netcdf_for)

file (GLOB SRC_FILES *.c)

add_library(netcdf_for STATIC ${SRC_FILES})