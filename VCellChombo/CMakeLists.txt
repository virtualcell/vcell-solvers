project(VCellChombo)

if (TARGET_SOLVERS_WITH_MESSAGING OR TARGET_SOLVERS_NO_MESSAGING OR TARGET_CHOMBO_CYGWIN_BUILD)
	set(HEADER_FILES 
		include/VCELL/ChomboDomainBC.h
		include/VCELL/ChomboGeometry.h
		include/VCELL/ChomboGeometryShop.h
		include/VCELL/ChomboIF.h
		include/VCELL/ChomboScheduler.h
		include/VCELL/ChomboSemiImplicitScheduler.h
		include/VCELL/ChomboSpec.h
		include/VCELL/DataSet.h
		include/VCELL/Feature.h
		include/VCELL/FVSolver.h
		include/VCELL/JumpCondition.h
		include/VCELL/Membrane.h
		include/VCELL/MembraneRegionVarContextExpression.h
		include/VCELL/MembraneRegionVariable.h
		include/VCELL/MembraneVarContextExpression.h
		include/VCELL/MembraneVariable.h
		include/VCELL/SimTool.h
		include/VCELL/SimTypes.h
		include/VCELL/SimulationExpression.h
		include/VCELL/SimulationMessaging.h
		include/VCELL/Structure.h
		include/VCELL/VarContext.h
		include/VCELL/Variable.h
		include/VCELL/VCellModel.h
		include/VCELL/VolumeRegionVarContextExpression.h
		include/VCELL/VolumeRegionVariable.h
		include/VCELL/VolumeVarContextExpression.h
		include/VCELL/VolumeVariable.h	
	)

	set(SRC_FILES 
		src/ChomboDomainBC.cpp
		src/ChomboGeometry.cpp
		src/ChomboGeometryShop.cpp
		src/ChomboIF.cpp
		src/ChomboScheduler.cpp
		src/ChomboSemiImplicitScheduler.cpp
		src/ChomboSpec.cpp
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
		src/SimTool.cpp
		src/SimulationExpression.cpp
		src/SimulationMessaging.cpp
		src/Structure.cpp
		src/VarContext.cpp
		src/Variable.cpp
		src/VCellModel.cpp
		src/VolumeRegionVarContextExpression.cpp
		src/VolumeRegionVariable.cpp
		src/VolumeVarContextExpression.cpp
		src/VolumeVariable.cpp
	)

	add_definitions(-DCH_FORT_UNDERSCORE -DCH_LANG_CC -DCH_USE_64 -DCH_USE_COMPLEX 
		-DCH_USE_DOUBLE -DCH_USE_HDF5 -DCH_USE_MEMORY_TRACKING -DCH_USE_SETVAL -DH5_USE_16_API)
	include_directories(
		${CMAKE_SOURCE_DIR}/../Chombo/lib/include 
		${CMAKE_SOURCE_DIR}/ExpressionParser 
		${CMAKE_CURRENT_SOURCE_DIR}/include 
	)
	if (CYGWIN)
		add_definitions(-DCH_CYGWIN)
		set(LIB_NAME_OS CYGWIN)
		include_directories(${CMAKE_SOURCE_DIR}/hdf5-1.8.8/src)
	elseif (LINUX)
		add_definitions(-DCH_Linux)
		set(LIB_NAME_OS Linux)
		include_directories(${CMAKE_BINARY_DIR}/include)
	endif()

	include_directories(${JMS_INCS})
	add_definitions(${JMS_DEFS})

	link_directories(${CMAKE_SOURCE_DIR}/../Chombo/lib)
	set(DEP_LIBS hdf5_cpp hdf5 ${JMS_LIBS} ExpressionParser zip unzip gfortran)

	set(DEP_LIBS_2D 
			mftools2d.${LIB_NAME_OS}.64.g++.gfortran.OPT
			ebamrelliptic2d.${LIB_NAME_OS}.64.g++.gfortran.OPT
			ebamrtools2d.${LIB_NAME_OS}.64.g++.gfortran.OPT
			ebtools2d.${LIB_NAME_OS}.64.g++.gfortran.OPT
			amrelliptic2d.${LIB_NAME_OS}.64.g++.gfortran.OPT
			amrtools2d.${LIB_NAME_OS}.64.g++.gfortran.OPT
			workshop2d.${LIB_NAME_OS}.64.g++.gfortran.OPT
			boxtools2d.${LIB_NAME_OS}.64.g++.gfortran.OPT
			basetools2d.${LIB_NAME_OS}.64.g++.gfortran.OPT
			${DEP_LIBS}
		)
	set(DEP_LIBS_3D 
			mftools3d.${LIB_NAME_OS}.64.g++.gfortran.OPT
			ebamrelliptic3d.${LIB_NAME_OS}.64.g++.gfortran.OPT
			ebamrtools3d.${LIB_NAME_OS}.64.g++.gfortran.OPT
			ebtools3d.${LIB_NAME_OS}.64.g++.gfortran.OPT
			amrelliptic3d.${LIB_NAME_OS}.64.g++.gfortran.OPT
			amrtools3d.${LIB_NAME_OS}.64.g++.gfortran.OPT
			workshop3d.${LIB_NAME_OS}.64.g++.gfortran.OPT
			boxtools3d.${LIB_NAME_OS}.64.g++.gfortran.OPT
			basetools3d.${LIB_NAME_OS}.64.g++.gfortran.OPT
			${DEP_LIBS}
		)

	set(EXE_2D VCellChombo2D)
	if (ARCH_64bit) 
		set(EXE_2D ${EXE_2D}_x64)
	endif()
	add_executable(${EXE_2D} ${SRC_FILES})
	target_link_libraries(${EXE_2D} ${DEP_LIBS_2D})
	add_dependencies(${EXE_2D} hdf5_cpp hdf5 ExpressionParser zip unzip)
	set_property(TARGET ${EXE_2D} PROPERTY COMPILE_DEFINITIONS CH_SPACEDIM=2)

	set(EXE_3D VCellChombo3D)
	if (ARCH_64bit) 
		set(EXE_3D ${EXE_3D}_x64)
	endif()
	add_executable(${EXE_3D} ${SRC_FILES})
	target_link_libraries(${EXE_3D} ${DEP_LIBS_3D})
	add_dependencies(${EXE_3D} hdf5_cpp hdf5 ExpressionParser zip unzip)
	set_property(TARGET ${EXE_3D} PROPERTY COMPILE_DEFINITIONS CH_SPACEDIM=3)

	install(TARGETS ${EXE_2D} RUNTIME DESTINATION bin)
	install(TARGETS ${EXE_3D} RUNTIME DESTINATION bin)
	
endif (TARGET_SOLVERS_WITH_MESSAGING OR TARGET_SOLVERS_NO_MESSAGING OR TARGET_CHOMBO_CYGWIN_BUILD)
