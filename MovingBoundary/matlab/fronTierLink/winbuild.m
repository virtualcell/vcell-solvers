fprintf( 'Building frontTierCmd\n')
mex -f build.opts -I../src -I../src/util -DWIN32  -L..\bin -L..\lib -lExpressionParser -lfrontier -lfronTierStub -lmatlabUtilLib -lMovingBoundaryLib  -g -D_DEBUG fronTierCmd.cpp
