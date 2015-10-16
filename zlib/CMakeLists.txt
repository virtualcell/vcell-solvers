project(zlib)
set(ZLIB_FOUND TRUE)
set(ZLIB_ROOT ${CMAKE_CURRENT_SOURCE_DIR} PARENT_SCOPE)
set(ZLIB_LIBRARY "zlib" PARENT_SCOPE)

set (SRC_FILES 
	adler32.c 
	compress.c 
	crc32.c 
	deflate.c 
	gzio.c 
	inflate.c 
	infblock.c 
	inftrees.c 
	infcodes.c 
	infutil.c 
	inffast.c
	trees.c 
	uncompr.c 
	zutil.c 
)

add_library(zlib STATIC ${SRC_FILES})
install(TARGETS zlib 
	ARCHIVE DESTINATION bin)
