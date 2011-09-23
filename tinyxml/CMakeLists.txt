PROJECT(tinyxml)    
    
FILE (GLOB SRC_FILES *.cpp)    
    
ADD_LIBRARY(tinyxml STATIC ${SRC_FILES})    
