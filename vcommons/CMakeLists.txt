
project(vcommons)

file(GLOB SRC_FILES src/*cpp) 

file(GLOB HEADER_FILES include/*h include/*hpp) 

set_source_files_properties( ${SRC_FILES} PROPERTIES LANGUAGE CXX)
add_library(vcommons ${SRC_FILES} ${HEADER_FILES})

install(TARGETS vcommons ARCHIVE DESTINATION lib)
