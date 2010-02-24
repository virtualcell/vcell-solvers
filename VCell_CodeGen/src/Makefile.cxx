#
# (C) Copyright University of Connecticut Health Center 2001.
# All rights reserved.
#
###############################################################
# Makefile file for the Vcell library on Unix True 64
#
#
# Last update: July 13, 2001
##############################################################
#
LIBRARY = libvcell.a
#
# Compiler executables
CC		= cxx
FC		= f90
# Standard C++ options
COPT		= -exceptions -DCXX -DSIZE64 -DINTEL
# Special C++ options
SCOPT		= -O5 -assume noaccuracy_sensitive -arch host -tune host -inline all #-ms -using_std
CFLAGS		= ${COPT} ${SCOPT}
CINCLUDES	= -I../include -I/opt/TWWfsw/zlib11/include/
# Standard F77 options
FOPT		=
# Special f77 options
SFOPT		=
FFLAGS		= ${FOPT} ${SFOPT}
#
# Suffixes rules
.SUFFIXES:	.o .cpp .f
.f.o :
	${FC} ${FFLAGS} -c $<
.cpp.o :
	${CC} $(CFLAGS) -c $< ${CINCLUDES}
#
# Source Files:
CCODE	=	App.o				ParticleContext.o \
		CString.o			Region.o \
		Contour.o			RegionEqnBuilder.o \
		DataSet.o			Scheduler.o \
		DomainPDEScheduler.o		SimTool.o \
		EqnBuilder.o			Simulation.o \
		EqnBuilderDiffusion.o		Solver.o \
		EqnBuilderReactionDiffusion.o	SparseMatrix.o \
		EqnBuilderReactionForward.o	TIFFImage.o \
		FastSystem.o			Timer.o \
		Feature.o			TriDiagMatrix.o \
		Geometry.o			VCellModel.o \
		MembraneEqnBuilderForward.o	VarContext.o \
		Mesh.o				Variable.o \
		MeshCorrect.o			PdeSolverDiana.o
FCODE	=	pcgwrapper.o
#
default all: ${LIBRARY}
${LIBRARY}: ${CCODE} ${FCODE}
	ar rv ${LIBRARY} *.o ./cxx_repository/*.o 
#	rm *.o
