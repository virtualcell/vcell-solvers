project(VCell)

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

if (UNIX)
	list(APPEND SRC_FILES src/pcgwrapper.f)
endif()

set (EXE_SRC_FILES
	src/FiniteVolume.cpp
)

include_directories(include ../ExpressionParser ${CMAKE_CURRENT_SOURCE_DIR}/../IDAWin ../netcdf-3.6.2/libsrc ../netcdf-3.6.2/cxx ../sundials/include ../sundials/src)
if (MSVC)
	include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../zlib)
elseif(UNIX)
	add_definitions(-DUNIX)
	if (APPLE) 
		add_definitions(-DMACOSX)
	elseif (LINUX AND ARCH_64bit)
	 	add_definitions(-DLINUX64)
	 endif()
endif()	
add_library(vcell STATIC ${SRC_FILES})

if (TARGET_SOLVERS_SERVER_SIDE OR TARGET_SOLVERS_CLIENT_SIDE)
	set(DEP_LIBS vcell ExpressionParser IDAWin netcdf qhull sundials_cvode sundials_ida sundials_nvecserial sundials ${SONICMQ_LIBS} zip unzip)
	link_directories(${CMAKE_CURRENT_SOURCE_DIR}/src ${CMAKE_CURRENT_SOURCE_DIR}/../PCGPack)
	if (MSVC)
		list(APPEND DEP_LIBS zlib pcgwrapper pcgpack_dbl mkl_c)
		link_directories(${CMAKE_CURRENT_SOURCE_DIR}/../IFortran)
	elseif (LINUX) 
		list(APPEND DEP_LIBS PCGPack blas z)
	elseif (APPLE) 
		list(APPEND DEP_LIBS PCGPack)
	endif()
	if (TARGET_SOLVERS_SERVER_SIDE)
		list(APPEND EXE_SRC_FILES ../VCell/src/SimulationMessaging.cpp)
	endif()
	add_executable(FiniteVolume ${EXE_SRC_FILES})	
	target_link_libraries(FiniteVolume ${DEP_LIBS})
	add_dependencies(FiniteVolume vcell ExpressionParser IDAWin netcdf qhull sundials sundials_nvecserial sundials_cvode sundials_ida zip unzip)
	if (MSVC)
		SET_TARGET_PROPERTIES(FiniteVolume PROPERTIES LINK_FLAGS_DEBUG "/NODEFAULTLIB:libcmt.lib")
	endif()
endif()