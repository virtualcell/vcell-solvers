project(zlib)

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
