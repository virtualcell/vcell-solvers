project(qhull)

file (GLOB SRC_FILES *.c)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../VCell/include)
set_source_files_properties( ${SRC_FILES} PROPERTIES LANGUAGE CXX )
add_library(qhull STATIC ${SRC_FILES})
