project(Hy3S Fortran)    
    
set (F90_SRC_FILES 
		src/f2kcli.f90
		src/globalvariables.f90
		src/ratelaws.f90
		src/dataio.f90
		src/randomgen.f90
		src/priorityqueue.f90
		src/specialevents.f90
		src/SDEintegration.f90
		src/propagators-HyJCMSS.f90
		src/mainprogram-HyJCMSS.f90
)
set_source_files_properties( ${F90_SRC_FILES} PROPERTIES LANGUAGE Fortran)
include_directories(${CMAKE_BINARY_DIR}/netcdf-3.6.2/f90)

if (TARGET_SOLVERS_WITH_MESSAGING)
	set (C_SRC_FILES
		src/msgwrapper.c 
		../VCell/src/SimulationMessaging.cpp
	)
	include_directories(../VCell/include)
	set_source_files_properties(${C_SRC_FILES} PROPERTIES LANGUAGE CXX)
	if (UNIX)
		add_definitions(-DUNIX)
		if (LINUX_32bit_BINARIES)
	 		add_definitions(-DLINUX)
		elseif (LINUX_64bit_BINARIES)
			add_definitions(-DLINUX64)
		endif()
	endif()	
endif()

set (CMAKE_Fortran_COMPILER ${IFORT_EXECUTABLE})
if (MSVC)
	set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /iface:mixed_str_len_arg /check:bounds")
endif()
set(DEP_LIBS netcdf_f90 netcdf_for netcdf ${SONICMQ_LIBS})

add_executable(Hybrid_EM ${F90_SRC_FILES} ${C_SRC_FILES})
target_link_libraries(Hybrid_EM ${DEP_LIBS})
set_target_properties(Hybrid_EM PROPERTIES LINKER_LANGUAGE Fortran)

add_executable(Hybrid_MIL ${F90_SRC_FILES} ${C_SRC_FILES})
target_link_libraries(Hybrid_MIL ${DEP_LIBS})
set_target_properties(Hybrid_MIL PROPERTIES LINKER_LANGUAGE Fortran)
set_property(TARGET Hybrid_MIL PROPERTY COMPILE_DEFINITIONS MILSTEIN)

add_executable(Hybrid_MIL_Adaptive ${F90_SRC_FILES} ${C_SRC_FILES})
target_link_libraries(Hybrid_MIL_Adaptive ${DEP_LIBS})
set_target_properties(Hybrid_MIL_Adaptive PROPERTIES LINKER_LANGUAGE Fortran)
set_property(TARGET Hybrid_MIL_Adaptive PROPERTY COMPILE_DEFINITIONS MILSTEIN ADAPTIVE)

install(TARGETS Hybrid_EM Hybrid_MIL Hybrid_MIL_Adaptive
        RUNTIME DESTINATION bin)
