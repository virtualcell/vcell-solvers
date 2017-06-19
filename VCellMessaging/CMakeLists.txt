project(vcellmessaging CXX)

set(HEADER_FILES 
	include/VCELL/SimulationMessaging.h
)

set(SRC_FILES 
	src/SimulationMessaging.cpp
)

include_directories(include)

add_library(vcellmessaging ${SRC_FILES} ${HEADER_FILES})

target_include_directories(vcellmessaging INTERFACE 
	$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
	$<INSTALL_INTERFACE:include>  # <prefix>/include
)
if (${OPTION_TARGET_MESSAGING})
	message(STATUS "ACTIVEMQCPP_LIBRARIES = '${ACTIVEMQCPP_LIBRARIES}'")
	message(STATUS "ACTIVEMQCPP_LINKDEFINITIONS = '${ACTIVEMQCPP_LINKDEFINITIONS}'")
	message(STATUS "ACTIVEMQCPP_DEFINITIONS = '${ACTIVEMQCPP_DEFINITIONS}'")
	message(STATUS "ACTIVEMQCPP_INCLUDE_DIR = '${ACTIVEMQCPP_INCLUDE_DIR}'")
	
	target_link_libraries(vcellmessaging ${ACTIVEMQCPP_LIBRARIES} ${ACTIVEMQCPP_LINKDEFINITIONS})
	target_compile_definitions(vcellmessaging PUBLIC USE_MESSAGING=1 ${ACTIVEMQCPP_DEFINITIONS})
	target_include_directories(vcellmessaging PUBLIC ${ACTIVEMQCPP_INCLUDE_DIR})
endif()

install(TARGETS vcellmessaging
	ARCHIVE DESTINATION bin
	RUNTIME DESTINATION bin)
