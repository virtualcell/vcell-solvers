cmake_minimum_required(VERSION 2.8)
project(matlabLink)
include(FindMatlabGerardus.txt)
if (${CMAKE_CL_64})
	set(OPT_FILE "build64.opts")
else (${CMAKE_CL_64})
	set(OPT_FILE "build32.opts")
endif (${CMAKE_CL_64})

file(GLOB SRC_FILES *cpp) 
file(GLOB HEADER_FILES *h) 
file(GLOB ML_SOURCE *cpm)
file(GLOB ML_SCRIPT *.m)
#file(GLOB BRIDGE_HEADER_FILES include/MBridge/*h) 

#set_source_files_properties( ${SRC_FILES} PROPERTIES LANGUAGE CXX)
include_directories(include 
	${MATLAB_INCLUDE_DIR}
	../FronTierLib 
	../FronTierLib/util 
	../boost_1_53_0
	../Solver/include)

set(PDIR ${CMAKE_BINARY_DIR}/${PROJECT_NAME})
file(COPY ${ML_SCRIPT} ${ML_SOURCE} ${OPT_FILE} DESTINATION ${PDIR})
foreach(F ${ML_SOURCE})
	get_filename_component(BASE ${F} NAME_WE)
	file(RENAME ${PDIR}/${BASE}.cpm ${PDIR}/${BASE}.cpp)
endforeach( )
file(RENAME ${PDIR}/${OPT_FILE} ${PDIR}/build.opts)
add_library(matlabLink STATIC ${SRC_FILES} ${HEADER_FILES}) 
install(TARGETS matlabLink ARCHIVE DESTINATION bin)
