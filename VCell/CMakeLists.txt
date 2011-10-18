project(VCell Fortran)

set(SRC_FILES 
	src/AlgebraicSystem.cpp
	src/CartesianMesh.cpp
#	src/Contour.cpp
#	src/ContourRegionVariable.cpp
#	src/ContourSubdomain.cpp
#	src/ContourVarContext.cpp
#	src/ContourVariable.cpp
	src/DataProcessorRoiTimeSeries.cpp
	src/DataProcessorVFrap.cpp
	src/DataSet.cpp
#	src/DomainPDEScheduler.cpp
	src/EllipticVolumeEqnBuilder.cpp
	src/EqnBuilder.cpp
	src/EqnBuilderReactionForward.cpp
	src/FastSystem.cpp
	src/FastSystemExpression.cpp
	src/Feature.cpp
	src/FieldData.cpp
	src/FVDataSet.cpp
	src/FVSolver.cpp
	src/FVUtils.cpp
#	src/Histogram.cpp
	src/JumpCondition.cpp
	src/Membrane.cpp
	src/MembraneEqnBuilderDiffusion.cpp
	src/MembraneEqnBuilderForward.cpp
	src/MembraneRegion.cpp
	src/MembraneRegionEqnBuilder.cpp
	src/MembraneRegionVarContextExpression.cpp
	src/MembraneRegionVariable.cpp
	src/MembraneVarContextExpression.cpp
	src/MembraneVariable.cpp
	src/Mesh.cpp
	src/ODESolver.cpp
#	src/ParticleContext.cpp
#	src/PDEScheduler.cpp
	src/PDESolver.cpp
	src/RandomVariable.cpp
	src/Region.cpp
	src/RegionSizeVariable.cpp
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
	src/Structure.cpp
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
	src/VolumeRegionVarContextExpression.cpp
	src/VolumeRegionVariable.cpp
	src/VolumeVarContextExpression.cpp
	src/VolumeVariable.cpp	
)

set (PCGWRAPPER_SRC_FILES
	src/pcgwrapper.f
)
 
set (EXE_SRC_FILES
	src/FiniteVolume.cpp
)

include_directories(include ../ExpressionParser ${CMAKE_CURRENT_SOURCE_DIR}/../IDAWin ../netcdf-3.6.2/libsrc ../netcdf-3.6.2/cxx ../sundials/include ../sundials/src)
if (NOT ZLIB_FOUND)
	include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../zlib)
endif()
if (MSVC)
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /names:lowercase")
elseif(UNIX)
	set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static")
	add_definitions(-DUNIX)
	if (APPLE) 
		add_definitions(-DMACOSX)
	else()
		if (LINUX_32bit_BINARIES)
	 		add_definitions(-DLINUX)
		elseif (LINUX_64bit_BINARIES)
			set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static")
			add_definitions(-DLINUX64)
		endif()	
	endif()
endif()	
add_library(vcell STATIC ${SRC_FILES})
add_library(pcgwrapper STATIC ${PCGWRAPPER_SRC_FILES})

if (TARGET_SOLVERS_WITH_MESSAGING OR TARGET_SOLVERS_NO_MESSAGING)
	set(DEP_LIBS vcell pcgwrapper ExpressionParser IDAWin netcdf_cxx netcdf PCGPack qhull sundials_cvode sundials_ida sundials_nvecserial sundials ${SONICMQ_LIBS} zip unzip)
	if (MSVC)
		list(APPEND DEP_LIBS zlib blas)
	elseif (LINUX) 
		list(APPEND DEP_LIBS blas z)
	endif()
	if (TARGET_SOLVERS_WITH_MESSAGING)
		list(APPEND EXE_SRC_FILES ../VCell/src/SimulationMessaging.cpp)
	endif()
	add_executable(FiniteVolume ${EXE_SRC_FILES})	
	target_link_libraries(FiniteVolume ${DEP_LIBS})
	add_dependencies(FiniteVolume vcell pcgwrapper ExpressionParser IDAWin netcdf qhull sundials sundials_nvecserial sundials_cvode sundials_ida zip unzip)
	install(TARGETS FiniteVolume
            RUNTIME DESTINATION bin)
	
endif()