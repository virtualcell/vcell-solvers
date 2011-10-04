project(netcdf)

set(SRC_FILES 
	libsrc/attr.c
	libsrc/dim.c
	libsrc/error.c
	libsrc/libvers.c
	libsrc/nc.c
	libsrc/ncx.c
	libsrc/posixio.c
	libsrc/putget.c
	libsrc/string.c
	libsrc/v1hpg.c
	libsrc/v2i.c
	libsrc/var.c
	cxx/nctst.cpp
	cxx/ncvalues.cpp
	cxx/netcdf.cpp
)

include_directories(libsrc cxx)

if (MSVC) 
include_directories(win32)
else()
include_directories(linux)
endif()
add_library(netcdf STATIC ${SRC_FILES})

add_subdirectory(fortran)
add_subdirectory(f90)