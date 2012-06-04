project(VCell Fortran)

set(HEADER_FILES 
	include/VCELL/AlgebraicSystem.h
	include/VCELL/CartesianMesh.h
#	include/VCELL/Contour.h
#	include/VCELL/ContourRegionVariable.h
#	include/VCELL/ContourSubdomain.h
#	include/VCELL/ContourVarContext.h
#	include/VCELL/ContourVariable.h
	include/VCELL/DataGenerator.h
	include/VCELL/DataSet.h
#	include/VCELL/DomainPDEScheduler.h
	include/VCELL/DoubleVector3.h
	include/VCELL/Element.h
	include/VCELL/EllipticVolumeEqnBuilder.h
	include/VCELL/EqnBuilder.h
	include/VCELL/EqnBuilderReactionForward.h
	include/VCELL/FastSystem.h
	include/VCELL/FastSystemExpression.h
	include/VCELL/Feature.h
	include/VCELL/FieldData.h
	include/VCELL/FVDataSet.h
	include/VCELL/FVSolver.h
	include/VCELL/FVUtils.h
	include/VCELL/GaussianConvolutionDataGenerator.h
#	include/VCELL/Histogram.h
	include/VCELL/IncidenceMatrix.h
	include/VCELL/JumpCondition.h
	include/VCELL/Membrane.h
	include/VCELL/MembraneEqnBuilderDiffusion.h
	include/VCELL/MembraneEqnBuilderForward.h
	include/VCELL/MembraneParticleVariable.h
	include/VCELL/MembraneRegion.h
	include/VCELL/MembraneRegionEqnBuilder.h
	include/VCELL/MembraneRegionVarContextExpression.h
	include/VCELL/MembraneRegionVariable.h
	include/VCELL/MembraneVarContextExpression.h
	include/VCELL/MembraneVariable.h
	include/VCELL/Mesh.h
	include/VCELL/ODESolver.h
#	include/VCELL/ParticleContext.h
#	include/VCELL/PDEScheduler.h
	include/VCELL/PDESolver.h
	include/VCELL/PostProcessingBlock.h
	include/VCELL/PostProcessingHdf5Writer.h
	include/VCELL/ProjectionDataGenerator.h
	include/VCELL/RandomVariable.h
	include/VCELL/Region.h
	include/VCELL/RegionSizeVariable.h
	include/VCELL/Scheduler.h
	include/VCELL/SerialScheduler.h
	include/VCELL/SimTool.h
	include/VCELL/SimTypes.h
	include/VCELL/Simulation.h
	include/VCELL/SimulationExpression.h
	include/VCELL/SimulationMessaging.h
	include/VCELL/Solver.h
	include/VCELL/SparseLinearSolver.h
#	include/VCELL/SparseMatrix.h
	include/VCELL/SparseMatrixEqnBuilder.h
	include/VCELL/SparseMatrixPCG.h
	include/VCELL/SparseVolumeEqnBuilder.h
	include/VCELL/SplitScheduler.h
	include/VCELL/Structure.h
	include/VCELL/StructuredPDESolver.h
	include/VCELL/SundialsPdeScheduler.h
#	include/VCELL/TIFFImage.h
	include/VCELL/Timer.h
#	include/VCELL/TriDiagMatrix.h
	include/VCELL/VarContext.h
	include/VCELL/Variable.h
	include/VCELL/VariableStatisticsDataGenerator.h
	include/VCELL/VCellModel.h
	include/VCELL/VolumeParticleVariable.h
	include/VCELL/VolumeRegion.h
	include/VCELL/VolumeRegionEqnBuilder.h
	include/VCELL/VolumeRegionVarContextExpression.h
	include/VCELL/VolumeRegionVariable.h
	include/VCELL/VolumeVarContextExpression.h
	include/VCELL/VolumeVariable.h	
)

set(SRC_FILES 
	src/AlgebraicSystem.cpp
	src/CartesianMesh.cpp
#	src/Contour.cpp
#	src/ContourRegionVariable.cpp
#	src/ContourSubdomain.cpp
#	src/ContourVarContext.cpp
#	src/ContourVariable.cpp
	src/DataGenerator.cpp
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
	src/GaussianConvolutionDataGenerator.cpp
#	src/Histogram.cpp
	src/JumpCondition.cpp
	src/Membrane.cpp
	src/MembraneEqnBuilderDiffusion.cpp
	src/MembraneEqnBuilderForward.cpp
	src/MembraneParticleVariable.cpp
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
	src/PostProcessingBlock.cpp
	src/PostProcessingHdf5Writer.cpp
	src/ProjectionDataGenerator.cpp
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
	src/VariableStatisticsDataGenerator.cpp
	src/VCellModel.cpp
	src/VolumeParticleVariable.cpp
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
if (APPLE)
	set (CMAKE_Fortran_COMPILER gfortran)
endif()
 
set (EXE_SRC_FILES
	src/FiniteVolume.cpp
)

include_directories(include ../ExpressionParser ${HDF5_BINARY_DIR} ../hdf5-1.8.8/c++/src ../hdf5-1.8.8/src ../IDAWin ../sundials/include ../sundials/src)
if (NOT ZLIB_FOUND)
	include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../zlib)
endif()


add_definitions(-DVCELL_HYBRID)
include_directories(include ../smoldyn/source/Smoldyn ../smoldyn/source/lib)
if (MSVC)
	include_directories(../smoldyn/windows ../glut-3.7.6/ ../zlib)
elseif (APPLE)
	include_directories(../smoldyn/mac)
elseif (LINUX)
	include_directories(../smoldyn/source)
endif()


if (MSVC)
	include_directories(../hdf5-1.8.8/windows/src)
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /names:lowercase")
elseif(UNIX)
	set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -static")
	if (APPLE) 
		add_definitions(-DMACOSX)
	else()
		if (LINUX_64bit_BINARIES AND NOT TARGET_SOLVERS_WITH_MESSAGING)
			set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static")
		endif()	
	endif()
endif()	
if (NOT TARGET_CHOMBO_BUILD)
	add_library(vcell STATIC ${SRC_FILES} ${HEADER_FILES})
endif()
add_library(pcgwrapper STATIC ${PCGWRAPPER_SRC_FILES})
if (TARGET_CHOMBO_BUILD)
	install(TARGETS pcgwrapper ARCHIVE DESTINATION bin)
endif()

if (TARGET_SOLVERS_WITH_MESSAGING OR TARGET_SOLVERS_NO_MESSAGING)
	set(EXE_FILE FiniteVolume)
	if (ARCH_64bit) 
		set(EXE_FILE ${EXE_FILE}_x64)
	elseif (MACOSX_BINARIES_ppc)
		set(EXE_FILE ${EXE_FILE}_ppc)
	endif()

	set(DEP_LIBS vcell pcgwrapper ExpressionParser hdf5_cpp hdf5 IDAWin PCGPack qhull sundials_cvode sundials_ida sundials_nvecserial sundials ${SONICMQ_LIBS} zip unzip)
	list(APPEND DEP_LIBS smoldynlib)
	if (MSVC)
		link_directories(${CMAKE_CURRENT_SOURCE_DIR}/../smoldyn/windows/glut-3.7.6/)
		if (ARCH_64bit)
			list(APPEND DEP_LIBS glut64)
		else()
			list(APPEND DEP_LIBS glut32)
		endif()
	endif()
	
	list(APPEND DEP_LIBS blas)
	if (ZLIB_FOUND)
		list(APPEND DEP_LIBS z)
	else()
		list(APPEND DEP_LIBS zlib)
	endif()
	if (APPLE)
		list(APPEND DEP_LIBS gfortran)
	endif()
	add_executable(${EXE_FILE} ${EXE_SRC_FILES})	
	target_link_libraries(${EXE_FILE} ${DEP_LIBS})
	add_dependencies(${EXE_FILE} vcell pcgwrapper ExpressionParser IDAWin qhull sundials sundials_nvecserial sundials_cvode sundials_ida zip unzip)
	install(TARGETS ${EXE_FILE} 
            RUNTIME DESTINATION bin)
	
endif()