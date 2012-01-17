project(unzip)

file (GLOB SRC_FILES *.c)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
if (MSVC)
	list(APPEND SRC_FILES
		win32/nt.c
		win32/win32.c
	)
	include_directories(win32)
else (MSVC)
	list(APPEND SRC_FILES
		unix/unix.c
	)
	include_directories(unix)
	add_definitions(-DUNIX)
endif (MSVC)

set_source_files_properties( ${SRC_FILES} PROPERTIES LANGUAGE CXX )
add_library(unzip STATIC ${SRC_FILES})
install(TARGETS unzip 
	ARCHIVE DESTINATION bin)