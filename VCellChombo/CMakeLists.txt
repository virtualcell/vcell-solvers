project(VCellChombo)

if (NOT DEFINED HDF_VERSION) 
	message(FATAL_ERROR "HDF_VERSION not set")
endif (NOT DEFINED HDF_VERSION) 
if (NOT DEFINED HDF_LIB) 
	message(FATAL_ERROR "HDF_LIB not set")
endif (NOT DEFINED HDF_LIB) 

#find_file(CSH csh)
#if (CSH MATCHES CSH-NOTFOUND)
#	message(FATAL_ERROR "${PROJECT_NAME} requires csh")
#endif (CSH MATCHES CSH-NOTFOUND)

set(DEP_LIBS hdf5 ${JMS_LIBS} ExpressionParser zip unzip gfortran)
if (${PARALLEL_CHOMBO_SOLVER})
	find_package(MPI REQUIRED)
	set(CHOMBO_DIR ChomboParallel)
	set(COMP "mpic++")
	set(MPITAG "MPI.")
	set(PARA_EXT "p")
	#A cmake-ism requires the arguments below to be separately enclosed in quotes
	set(PARALLEL_SUPPLEMENT "MPI=TRUE" "MPICXX=mpic++")
	INCLUDE (FindMPI)
	INCLUDE_DIRECTORIES (${MPI_C_INCLUDE_PATH})
	add_definitions(-DCH_MPI)
#	LIST(APPEND DEP_LIBS mpich)
else (${PARALLEL_CHOMBO_SOLVER})
	set(CHOMBO_DIR Chombo)
	set(COMP "g++")
	set(MPITAG "")
	set(PARALLEL_SUPPLEMENT "")
	set(PARA_EXT "")
endif (${PARALLEL_CHOMBO_SOLVER})
OPTION(CHOMBO_3D "Compile VCellChombo 3d" ON)
OPTION(CHOMBO_COMPILE_TEST "CHOMBO_COMPILE_TEST" OFF)

if (TARGET_SOLVERS_WITH_MESSAGING OR TARGET_SOLVERS_NO_MESSAGING OR TARGET_CHOMBO_CYGWIN_BUILD)
	set(HEADER_FILES 
		include/VCELL/ChomboDomainBC.h
		include/VCELL/ChomboEBBC.h
		include/VCELL/ChomboGeometry.h
		include/VCELL/ChomboGeometryShop.h
		include/VCELL/ChomboIF.h
		include/VCELL/ChomboScheduler.h
		include/VCELL/ChomboSemiImplicitScheduler.h
		include/VCELL/ChomboSpec.h
		include/VCELL/DataGenerator.h
		include/VCELL/DataSet.h
		include/VCELL/Feature.h
		include/VCELL/FVSolver.h
		include/VCELL/JumpCondition.h
		include/VCELL/Membrane.h
		include/VCELL/MembraneRegionVarContextExpression.h
		include/VCELL/MembraneRegionVariable.h
		include/VCELL/MembraneVarContextExpression.h
		include/VCELL/MembraneVariable.h
		include/VCELL/PostProcessingBlock.h
		include/VCELL/PostProcessingHdf5Writer.h
		include/VCELL/SimTool.h
		include/VCELL/SimTypes.h
		include/VCELL/SimulationExpression.h
		include/VCELL/SimulationMessaging.h
		include/VCELL/Structure.h
		include/VCELL/VarContext.h
		include/VCELL/Variable.h
		include/VCELL/VariableStatisticsDataGenerator.h
		include/VCELL/VCellModel.h
		include/VCELL/VolumeRegionVarContextExpression.h
		include/VCELL/VolumeRegionVariable.h
		include/VCELL/VolumeVarContextExpression.h
		include/VCELL/VolumeVariable.h	
	)

	set(SRC_FILES 
		src/ChomboDomainBC.cpp
		src/ChomboEBBC.cpp
		src/ChomboGeometry.cpp
		src/ChomboGeometryShop.cpp
		src/ChomboIF.cpp
		src/ChomboScheduler.cpp
		src/ChomboSemiImplicitScheduler.cpp
		src/ChomboSpec.cpp
		src/DataGenerator.cpp
		src/DataSet.cpp
		src/Feature.cpp
		src/FiniteVolume.cpp
		src/FVSolver.cpp
		src/JumpCondition.cpp
		src/Membrane.cpp
		src/MembraneRegionVarContextExpression.cpp
		src/MembraneRegionVariable.cpp
		src/MembraneVarContextExpression.cpp
		src/MembraneVariable.cpp
		src/PostProcessingBlock.cpp
		src/PostProcessingHdf5Writer.cpp
		src/SimTool.cpp
		src/SimulationExpression.cpp
		src/SimulationMessaging.cpp
		src/Structure.cpp
		src/VarContext.cpp
		src/Variable.cpp
		src/VariableStatisticsDataGenerator.cpp
		src/VCellModel.cpp
		src/VolumeRegionVarContextExpression.cpp
		src/VolumeRegionVariable.cpp
		src/VolumeVarContextExpression.cpp
		src/VolumeVariable.cpp
	)

	add_definitions(-DCH_FORT_UNDERSCORE -DCH_LANG_CC -DCH_USE_64 -DCH_USE_COMPLEX 
		-DCH_USE_DOUBLE -DCH_USE_HDF5 -DCH_USE_MEMORY_TRACKING -DCH_USE_SETVAL -DH5_USE_16_API)
	include_directories(
		${CMAKE_SOURCE_DIR}/${CHOMBO_DIR}/lib/include 
		${CMAKE_SOURCE_DIR}/ExpressionParser 
		${CMAKE_CURRENT_SOURCE_DIR}/include 
	)
	set(CHOMBO_DEFS_FILE ${CMAKE_SOURCE_DIR}/${CHOMBO_DIR}/lib/mk/Make.defs.local)
	include_directories(${CMAKE_SOURCE_DIR}/${HDF_VERSION})
	include_directories(${CMAKE_BINARY_DIR}/${HDF_VERSION})
	include_directories(${CMAKE_SOURCE_DIR}/${HDF_VERSION}/src)
	if (CYGWIN)
		add_definitions(-DCH_CYGWIN)
		set(LIB_NAME_OS CYGWIN)
		file(COPY "Make.defs.local.cygwin" DESTINATION "${CMAKE_SOURCE_DIR}/${CHOMBO_DIR}/lib/mk") 
		file(RENAME "${CMAKE_SOURCE_DIR}/${CHOMBO_DIR}/lib/mk/Make.defs.local.cygwin" ${CHOMBO_DEFS_FILE})
	elseif (LINUX OR APPLE)
		add_definitions(-DCH_Linux)
		set(LIB_NAME_OS Linux)
		include_directories(${CMAKE_BINARY_DIR}/include)
		file(COPY "Make.defs.local.linux" DESTINATION "${CMAKE_SOURCE_DIR}/${CHOMBO_DIR}/lib/mk") 
		file(RENAME "${CMAKE_SOURCE_DIR}/${CHOMBO_DIR}/lib/mk/Make.defs.local.linux" ${CHOMBO_DEFS_FILE})
	endif()

	include_directories(${JMS_INCS})
	add_definitions(${JMS_DEFS})

	link_directories(${CMAKE_SOURCE_DIR}/${CHOMBO_DIR}/lib)

####################################################################################
#
# We set full path names of Chombo libraries for use in "add_custom_command". The cmake
# generated Makefiles will find these on the system and not rebuild them if they 
# already exist.
#
####################################################################################
	if (ARCH_64bit) 
		set(OPT64 "64.")
		set(USE64 TRUE)
		set(BIT_EXT "_x64")
		#set(CHOMBO_COMPILE_FLAGS -m64 -mno-cygwin)
		set(CHOMBO_COMPILE_FLAGS -m64)
	else (ARCH_64bit) 
		set(OPT64 "")
		set(USE64 FALSE)
		set(CHOMBO_COMPILE_FLAGS "")
		set(BIT_EXT "")
	endif (ARCH_64bit) 
	set(CHOMBO_LIB_DIR ${CMAKE_SOURCE_DIR}/${CHOMBO_DIR}/lib)
	set(CHOMBO_LIBS_2D 
		${CHOMBO_LIB_DIR}/libmftools2d.${LIB_NAME_OS}.${OPT64}${COMP}.gfortran.OPT.${MPITAG}a
		${CHOMBO_LIB_DIR}/libebamrelliptic2d.${LIB_NAME_OS}.${OPT64}${COMP}.gfortran.OPT.${MPITAG}a
		${CHOMBO_LIB_DIR}/libebamrtools2d.${LIB_NAME_OS}.${OPT64}${COMP}.gfortran.OPT.${MPITAG}a
		${CHOMBO_LIB_DIR}/libebtools2d.${LIB_NAME_OS}.${OPT64}${COMP}.gfortran.OPT.${MPITAG}a
		${CHOMBO_LIB_DIR}/libamrelliptic2d.${LIB_NAME_OS}.${OPT64}${COMP}.gfortran.OPT.${MPITAG}a
		${CHOMBO_LIB_DIR}/libamrtools2d.${LIB_NAME_OS}.${OPT64}${COMP}.gfortran.OPT.${MPITAG}a
		${CHOMBO_LIB_DIR}/libworkshop2d.${LIB_NAME_OS}.${OPT64}${COMP}.gfortran.OPT.${MPITAG}a
		${CHOMBO_LIB_DIR}/libboxtools2d.${LIB_NAME_OS}.${OPT64}${COMP}.gfortran.OPT.${MPITAG}a
		${CHOMBO_LIB_DIR}/libbasetools2d.${LIB_NAME_OS}.${OPT64}${COMP}.gfortran.OPT.${MPITAG}a
		)
	set(DEP_LIBS_2D  ${CHOMBO_LIBS_2D} ${DEP_LIBS})

	set(CHOMBO_LIBS_3D 
		${CHOMBO_LIB_DIR}/libmftools3d.${LIB_NAME_OS}.${OPT64}${COMP}.gfortran.OPT.${MPITAG}a
		${CHOMBO_LIB_DIR}/libebamrelliptic3d.${LIB_NAME_OS}.${OPT64}${COMP}.gfortran.OPT.${MPITAG}a
		${CHOMBO_LIB_DIR}/libebamrtools3d.${LIB_NAME_OS}.${OPT64}${COMP}.gfortran.OPT.${MPITAG}a
		${CHOMBO_LIB_DIR}/libebtools3d.${LIB_NAME_OS}.${OPT64}${COMP}.gfortran.OPT.${MPITAG}a
		${CHOMBO_LIB_DIR}/libamrelliptic3d.${LIB_NAME_OS}.${OPT64}${COMP}.gfortran.OPT.${MPITAG}a
		${CHOMBO_LIB_DIR}/libamrtools3d.${LIB_NAME_OS}.${OPT64}${COMP}.gfortran.OPT.${MPITAG}a
		${CHOMBO_LIB_DIR}/libworkshop3d.${LIB_NAME_OS}.${OPT64}${COMP}.gfortran.OPT.${MPITAG}a
		${CHOMBO_LIB_DIR}/libboxtools3d.${LIB_NAME_OS}.${OPT64}${COMP}.gfortran.OPT.${MPITAG}a
		${CHOMBO_LIB_DIR}/libbasetools3d.${LIB_NAME_OS}.${OPT64}${COMP}.gfortran.OPT.${MPITAG}a
		)
	set(DEP_LIBS_3D  ${CHOMBO_LIBS_3D} ${DEP_LIBS})

	set(EXE_2D VCellChombo2D${BIT_EXT}${PARA_EXT})

	#use the same command for 2d and 3d ; we'll just add DIM=3 to the 3d case
	#Setting USE_MF in Chombo's Make.defs.local didn't seem to work, so set both USE_EB and USE_MF from command line

	list(APPEND CHOMBO_COMPILE_FLAGS " -I${CMAKE_BINARY_DIR}/${HDF_VERSION}")
	file(APPEND ${CHOMBO_DEFS_FILE} XTRACXXFLAGS=${CHOMBO_COMPILE_FLAGS}) 
	set(CHOMBO_BUILD_CMD $(MAKE) -j1 lib CMAKE_BINARY_DIR=${CMAKE_BINARY_DIR}  HDF_VERSION=${HDF_VERSION} HDF_LIB=${HDF_LIB} CMAKE_SOURCE_DIR=${CMAKE_SOURCE_DIR} USE_EB=TRUE USE_MF=TRUE USE_64=${USE64} ${PARALLEL_SUPPLEMENT})
	#message(${CHOMBO_BUILD_CMD})
	add_custom_command(OUTPUT ${CHOMBO_LIBS_2D}
		PRE_BUILD
		COMMAND ${CHOMBO_BUILD_CMD} 
		DEPENDS ${HDF5_LIB_TARGET} 
		WORKING_DIRECTORY ${CHOMBO_LIB_DIR}
	)
	# include the Chombo libraries as "source" file to trigger building them via "add_custom_command"
	# if they don't exist
	add_executable(${EXE_2D} ${CHOMBO_LIBS_2D} ${SRC_FILES})
	target_link_libraries(${EXE_2D} ${DEP_LIBS_2D})
	add_dependencies(${EXE_2D} hdf5 hdf5_cpp ExpressionParser zip unzip ${DEP_LIBS} ) 
	set_property(TARGET ${EXE_2D} PROPERTY COMPILE_DEFINITIONS CH_SPACEDIM=2)
	install(TARGETS ${EXE_2D} RUNTIME DESTINATION ${OPTION_EXE_DIRECTORY})

	if (CHOMBO_3D)
		set(EXE_3D VCellChombo3D${BIT_EXT}${PARA_EXT})

		add_custom_command(OUTPUT ${CHOMBO_LIBS_3D}
			PRE_BUILD
			COMMAND ${CHOMBO_BUILD_CMD} DIM=3
			WORKING_DIRECTORY ${CHOMBO_LIB_DIR}
			VERBATIM
		)
		add_executable(${EXE_3D} ${CHOMBO_LIBS_3D} ${SRC_FILES})
		target_link_libraries(${EXE_3D} ${DEP_LIBS_3D})
		add_dependencies(${EXE_3D} hdf5 hdf5_cpp ExpressionParser zip unzip ${DEP_LIBS} ${EXE_2D}) 
		set_property(TARGET ${EXE_3D} PROPERTY COMPILE_DEFINITIONS CH_SPACEDIM=3)

		install(TARGETS ${EXE_3D} RUNTIME DESTINATION ${OPTION_EXE_DIRECTORY})
	endif (CHOMBO_3D)


	if (CHOMBO_COMPILE_TEST)
		set(CCT ChomboCompileTest)
		set(TEST_SRC_FILES 
			test/CompileTest.cpp)
		add_executable(${CCT} ${CHOMBO_LIBS_2D} ${TEST_SRC_FILES})
		target_link_libraries(${CCT} ${DEP_LIBS_2D})
		set_property(TARGET ${CCT} PROPERTY COMPILE_DEFINITIONS CH_SPACEDIM=2)
		install(TARGETS ${CCT} RUNTIME DESTINATION ${OPTION_EXE_DIRECTORY})
	endif (CHOMBO_COMPILE_TEST)
	
	
	if (${PARALLEL_CHOMBO_SOLVER})
		target_link_libraries(${EXE_2D} ${MPI_CXX_LIBRARIES})
		target_link_libraries(${EXE_3D} ${MPI_CXX_LIBRARIES})
		link_directories(${MPI_CXX_LIBRARIES})
	endif (${PARALLEL_CHOMBO_SOLVER})
endif (TARGET_SOLVERS_WITH_MESSAGING OR TARGET_SOLVERS_NO_MESSAGING OR TARGET_CHOMBO_CYGWIN_BUILD)
