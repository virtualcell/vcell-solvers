project(VCellCodeGen Fortran)

set(SRC_FILES 
	src/AlgebraicSystem.cpp
	src/CartesianMesh.cpp
	src/Contour.cpp
	src/ContourRegionVariable.cpp
	src/ContourSubdomain.cpp
	src/ContourVarContext.cpp
	src/ContourVariable.cpp
	src/DataProcessorVFrap.cpp
	src/DataSet.cpp
#	src/DomainPDEScheduler.cpp
	src/EllipticVolumeEqnBuilder.cpp
	src/EqnBuilder.cpp
	src/EqnBuilderReactionForward.cpp
	src/FastSystem.cpp
	src/Feature.cpp
	src/FieldData.cpp
#	src/FVSolver.cpp
	src/FVUtils.cpp
#	src/Histogram.cpp
	src/MembraneEqnBuilderDiffusion.cpp
	src/MembraneEqnBuilderForward.cpp
	src/MembraneRegion.cpp
	src/MembraneRegionEqnBuilder.cpp
	src/MembraneRegionVarContext.cpp
	src/MembraneRegionVariable.cpp
	src/MembraneVarContext.cpp
	src/MembraneVariable.cpp
	src/Mesh.cpp
	src/ODESolver.cpp
#	src/ParticleContext.cpp
#	src/PDEScheduler.cpp
	src/PDESolver.cpp
	src/RandomVariable.cpp
	src/Region.cpp
	src/Scheduler.cpp
	src/SerialScheduler.cpp
	src/SimTool.cpp
	src/Simulation.cpp
	src/SimulationExpression.cpp
	src/SimulationMessaging.cpp
	src/Solver.cpp
	src/SparseLinearSolver.cpp
#	src/SparseMatrix.cpp
	src/SparseMatrixEqnBuilder.cpp
	src/SparseMatrixPCG.cpp
	src/SparseVolumeEqnBuilder.cpp
	src/SplitScheduler.cpp
	src/StructuredPDESolver.cpp
	src/SundialsPdeScheduler.cpp
#	src/TIFFImage.cpp
	src/Timer.cpp
#	src/TriDiagMatrix.cpp
	src/VarContext.cpp
	src/Variable.cpp
	src/VCellModel.cpp
	src/VolumeRegion.cpp
	src/VolumeRegionEqnBuilder.cpp
	src/VolumeRegionVarContext.cpp
	src/VolumeRegionVariable.cpp
	src/VolumeVarContext.cpp
	src/VolumeVariable.cpp	
)

set (PCGWRAPPER_SRC_FILES
	src/pcgwrapper.f
)

include_directories(include ../ExpressionParser ${CMAKE_CURRENT_SOURCE_DIR}/../IDAWin ../netcdf-3.6.2/libsrc ../netcdf-3.6.2/cxx ../sundials/include ../sundials/src)
if (NOT ZLIB_FOUND)
	include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../zlib)
endif()

if (MSVC)
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /names:lowercase")
elseif(UNIX)
	set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static")
	if (APPLE) 
		add_definitions(-DMACOSX)
	else()
		if (LINUX_64bit_BINARIES)
			add_definitions(-DLINUX64)
			if (NOT TARGET_SOLVERS_WITH_MESSAGING)
				set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static")
			endif()
		endif()	
	endif()
endif()	
add_library(vcellCodeGen STATIC ${SRC_FILES})
add_library(pcgwrapperCodeGen STATIC ${PCGWRAPPER_SRC_FILES})