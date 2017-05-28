project(zip)

file (GLOB SRC_FILES *.c)

add_definitions(-DUSE_ZIPMAIN -DSTATIC_LIB -DNO_ASM -fpermissive)
if (MINGW)
add_definitions(-DWIN32 -DFORCE_WIN32_OVER_UNIX -DHAVE_DIRENT_H -DHAVE_TERMIOS_H) 
endif (MINGW)
include_directories(${CMAKE_CURRENT_SOURCE_DIR})
if (MSVC OR MINGW)
	list(APPEND SRC_FILES
		win32/nt.c
		win32/win32.c
		win32/win32zip.c
	)
	include_directories(win32)
	add_definitions(-DWIN32 -DMSDOS)
else (MSVC OR MINGW)
	list(APPEND SRC_FILES
		unix/unix.c
	)
	include_directories(unix)
	add_definitions(-DUNIX -DNO_ASM -DHAVE_DIRENT_H -DHAVE_TERMIOS_H)
endif (MSVC OR MINGW)

set_source_files_properties( ${SRC_FILES} PROPERTIES LANGUAGE CXX )
add_library(zip STATIC ${SRC_FILES})
install(TARGETS zip 
	ARCHIVE DESTINATION bin)
