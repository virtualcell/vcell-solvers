project(IDAWin)

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
set(DEP_LIBS IDAWin ExpressionParser netcdf zip ${SONICMQ_LIBS})

if (TARGET_SOLVERS_SERVER_SIDE OR TARGET_SOLVERS_CLIENT_SIDE)
	if (MSVC)
		link_directories(${CMAKE_CURRENT_SOURCE_DIR}/../glut-3.7.6)
		list(APPEND DEP_LIBS zlib glut32)
	else()
		list(APPEND DEP_LIBS glut tiff)
	endif()
	add_executable(smoldyn ${SRC_FILES})	
	target_link_libraries(smoldyn ${DEP_LIBS})
	add_dependencies(smoldyn ExpressionParser IDAWin zip zlib)
endif()
