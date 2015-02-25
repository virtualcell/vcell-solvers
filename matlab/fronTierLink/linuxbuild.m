fprintf( 'Building frontTierCmd\n')
mex  -I../src -I../src/util -L../bin -L../lib -lExpressionParser -lfronTierStub -lfrontier -lmatlabUtilLib -g -D_DEBUG fronTierCmd.cpp
