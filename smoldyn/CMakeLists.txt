project(smoldyn)

if (TARGET_SOLVERS_WITH_MESSAGING)
	ADD_DEFINITIONS(-UOPENGL)
else()
	if (APPLE)
		include (FindOpenGL)
		if (NOT OPENGL_FOUND)
			message(FATAL_ERROR, "smoldyn needs OpenGL to build")
		endif()
	endif()
	if (NOT MSVC)
		include (FindGLUT)
		if (NOT GLUT_FOUND)
			message(FATAL_ERROR, "smoldyn needs glut to build")
		endif()
	endif()
endif()

set(HEADER_FILES 
	source/lib/Geometry.h
	source/lib/math2.h
	source/lib/opengl2.h
	source/lib/parse.h
	source/lib/queue.h
	source/lib/random2.h
	source/lib/Rn.h
	source/lib/RnSort.h
	source/lib/rxnparam.h
	source/lib/SimCommand.h
	source/lib/Sphere.h
	source/lib/string2.h
	source/lib/SurfaceParam.h
	source/lib/Zn.h
	source/Smoldyn/smoldyn.h
	source/Smoldyn/DataProcessorRoiTimeSeriesSmoldyn.h
	source/Smoldyn/VCellSmoldynOutput.h
)

set(SRC_FILES 
	source/lib/Geometry.c
	source/lib/math2.c
	source/lib/opengl2.c
	source/lib/parse.c
	source/lib/queue.c
	source/lib/random2.c
	source/lib/Rn.c
	source/lib/RnSort.c
	source/lib/rxnparam.c
	source/lib/SimCommand.c
	source/lib/Sphere.c
	source/lib/string2.c
	source/lib/SurfaceParam.c
	source/lib/Zn.c
	source/Smoldyn/smolboxes.c
	source/Smoldyn/smolcmd.c
	source/Smoldyn/smolcomparts.c
	source/Smoldyn/smoldyn.c
	source/Smoldyn/smolgraphics.c
	source/Smoldyn/smolmolec.c
	source/Smoldyn/smolmoleculizer.c
	source/Smoldyn/smolport.c
	source/Smoldyn/smolreact.c
	source/Smoldyn/smolsim.c
	source/Smoldyn/smolsurface.c
	source/Smoldyn/smolthread.c
	source/Smoldyn/smolwall.c
	source/Smoldyn/DataProcessorRoiTimeSeriesSmoldyn.cpp
	source/Smoldyn/VCellSmoldynOutput.cpp
)

list(APPEND SRC_FILES ../VCell/src/DataSet.cpp)
list(APPEND SRC_FILES ../VCell/src/FieldData.cpp)
list(APPEND SRC_FILES ../VCell/src/SimulationMessaging.cpp)
include_directories(source/lib source/Smoldyn ../ExpressionParser ../IDAWin ../netcdf-3.6.2/libsrc ../netcdf-3.6.2/cxx ../VCell/include)

if (MSVC)
	include_directories(windows ../glut-3.7.6 ../zlib)
elseif (APPLE)
	include_directories(mac)
elseif (LINUX)
	include_directories(source)
endif()

set_source_files_properties(${SRC_FILES} PROPERTIES LANGUAGE CXX )
set(DEP_LIBS IDAWin ExpressionParser netcdf_cxx netcdf zip ${SONICMQ_LIBS})

if (VCELL_SPATIAL_HYBRID)
	add_library(smoldynlib STATIC ${SRC_FILES} ${HEADER_FILES})
	set_property(TARGET smoldynlib PROPERTY COMPILE_DEFINITIONS VCELL_HYBRID)
endif()

if (TARGET_SOLVERS_WITH_MESSAGING OR TARGET_SOLVERS_NO_MESSAGING)
	if (MSVC)
		link_directories(${CMAKE_CURRENT_SOURCE_DIR}/../glut-3.7.6)
		list(APPEND DEP_LIBS zlib)
		if (ARCH_64bit)
			list(APPEND DEP_LIBS glut64)
		else()
			list(APPEND DEP_LIBS glut32)
		endif()
	else()
		list(APPEND DEP_LIBS z)
		if (TARGET_SOLVERS_NO_MESSAGING)
			if (APPLE)
				list(APPEND DEP_LIBS ${GLUT_glut_LIBRARY} ${OPENGL_gl_LIBRARY})
			else()
				list(APPEND DEP_LIBS glut tiff)
			endif()
		endif()
	endif()
	add_executable(smoldyn ${SRC_FILES} ${HEADER_FILES})
	target_link_libraries(smoldyn ${DEP_LIBS})
	add_dependencies(smoldyn ExpressionParser IDAWin zip zlib)
	
	install(TARGETS smoldyn
        RUNTIME DESTINATION bin)	
endif()
