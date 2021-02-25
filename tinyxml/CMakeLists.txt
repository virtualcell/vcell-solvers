CMAKE_MINIMUM_REQUIRED(VERSION 3.0)

project(tinyxml)    
    
file (GLOB SRC_FILES *.cpp)   
if (APPLE)
	set(CMAKE_OSX_ARCHITECTURES "i386;x86_64")
endif()
    
add_library(tinyxml STATIC ${SRC_FILES})
