project(apriconv)

set(HEADER_FILES 
	include/api_version.h
	include/apr_iconv.h
	lib/charset_alias.h
	lib/iconv.h
) 

set(SRC_FILES
	lib/iconv.c
	lib/iconv_ces.c
	lib/iconv_ces_euc.c
	lib/iconv_ces_iso2022.c
	lib/iconv_int.c
	lib/iconv_module.c
	lib/iconv_uc.c
)

include_directories(../apr-1.4.6/include include lib)
if (MSVC)
	add_definitions(-DAPR_DECLARE_STATIC -DAPI_DECLARE_STATIC)
	set(APR_H_DIR ${CMAKE_CURRENT_BINARY_DIR}/../apr-1.4.6/include)
	include_directories(${APR_H_DIR})
endif()
add_library(apriconv-1 STATIC ${SRC_FILES} ${HEADER_FILES})
add_dependencies(apriconv-1 apr-1)
install(TARGETS apriconv-1 ARCHIVE DESTINATION bin)
