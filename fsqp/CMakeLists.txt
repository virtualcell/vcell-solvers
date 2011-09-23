PROJECT(fsqp)    
    
set (SRC_FILES 
	cfsqp.c
	qld.c
	globals.c
)   
    
ADD_LIBRARY(fsqp STATIC ${SRC_FILES})    
