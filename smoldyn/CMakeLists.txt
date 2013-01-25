project(smoldyn)
cmake_minimum_required(VERSION 2.6)
set(SMOLDYN_VERSION "2.27")



######### Core code information ###########

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
	source/lib/SFMT/SFMT.h
	source/Smoldyn/smoldyn.h
	source/Smoldyn/smoldynfuncs.h
#	source/vcell/SimpleMesh.h
#	source/vcell/SimpleValueProvider.h
	source/vcell/SmoldynDataGenerator.h
	source/vcell/SmoldynHdf5Writer.h
	source/vcell/SmoldynVarStatDataGenerator.h
	source/vcell/VCellSmoldynOutput.h
	source/vcell/vcellcmd.h
)

set(HYBRID_HEADER_FILES	
	source/vcell/VCellValueProvider.h
	source/vcell/VCellMesh.h
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
	source/lib/SFMT/SFMT.c
	source/Smoldyn/smolboxes.c
	source/Smoldyn/smolcmd.c
	source/Smoldyn/smolcomparts.c
	source/Smoldyn/smolfilament.c
	source/Smoldyn/smolgraphics.c
	source/Smoldyn/smoldynhybrid.c
	source/Smoldyn/smolmolec.c
	source/Smoldyn/smolmoleculizer.c
	source/Smoldyn/smolport.c
	source/Smoldyn/smolreact.c
	source/Smoldyn/smolsim.c
	source/Smoldyn/smolsurface.c
	source/Smoldyn/smolthread.c
	source/Smoldyn/smolwall.c
#	source/vcell/SimpleMesh.cpp
#	source/vcell/SimpleValueProvider.cpp
	source/vcell/SmoldynDataGenerator.cpp
	source/vcell/SmoldynHdf5Writer.cpp
	source/vcell/SmoldynVarStatDataGenerator.cpp
	source/vcell/VCellSmoldynOutput.cpp
	source/vcell/vcellcmd.c
)

list(APPEND SRC_FILES ../VCell/src/DataSet.cpp)
list(APPEND SRC_FILES ../VCell/src/FieldData.cpp)
list(APPEND SRC_FILES ../VCell/src/SimulationMessaging.cpp)

set(HYBRID_SRC_FILES	
	source/vcell/vcellhybrid.c
	source/vcell/VCellValueProvider.cpp
	source/vcell/VCellMesh.cpp
)

set(MAIN_FILES
	source/vcell/vcell_smoldyn.c
)

set_source_files_properties(${SRC_FILES} PROPERTIES LANGUAGE CXX )
set_source_files_properties(${MAIN_FILES} PROPERTIES LANGUAGE CXX )
set_source_files_properties(${HYBRID_SRC_FILES} PROPERTIES LANGUAGE CXX )

include_directories(source/lib source/Smoldyn source/vcell ${CMAKE_BINARY_DIR} ${HDF5_BINARY_DIR} ../hdf5-1.8.8/c++/src ../hdf5-1.8.8/src)

####### Option: Compile for VCell ##########

option(OPTION_VCELL "Compile Smoldyn for VCell" ON)
#if(OPTION_VCELL)
#	set(MAIN_FILES
#		source/vcell/vcell_smoldyn.c
#	)
#	set_source_files_properties(${MAIN_FILES} PROPERTIES LANGUAGE CXX )
#endif()


####### Build for debugging or release ##########

if(NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE Debug CACHE STRING
		"Choose the buid type: None, Debug, Release, RelWithDebInfo, or MinSizeRel" FORCE)
endif(NOT CMAKE_BUILD_TYPE)

if(CMAKE_COMPILER_IS_GNUCXX)
	set(CMAKE_CXX_FLAGS_DEBUG "-O0 -g -Wall")
endif(CMAKE_COMPILER_IS_GNUCXX)

# message(STATUS "CMAKE_CXX_FLAGS_DEBUG='${CMAKE_CXX_FLAGS_DEBUG}'")


####### Option: Compile with MinGW ##########

option(MINGW "Cross-compile for Windows using MinGW compiler" OFF)

if (MINGW)
	set (MINGWDIR /opt/local/i386-mingw32)
	set (CMAKE_CXX_COMPILER /opt/local/bin/i386-mingw32-g++)
endif(MINGW)


######### Platform dependent information ##########

if (MINGW)
	include_directories(${MINGWDIR}/include windows)
	link_directories(${MINGWDIR}/lib ${CMAKE_CURRENT_SOURCE_DIR}/windows/dll)
	list(APPEND DEP_LIBS opengl32 glu32 freeglut z)
#elseif (APPLE)
#	include_directories(Mac)
#	link_directories(${CMAKE_CURRENT_SOURCE_DIR}/Mac/libtiff/lib ${CMAKE_CURRENT_SOURCE_DIR}/Mac/zlib/lib)
#	list(APPEND DEP_LIBS ${GLUT_glut_LIBRARY} ${OPENGL_gl_LIBRARY} z)
#else()
#	list(APPEND DEP_LIBS glut tiff z)
endif()


####### Option: Build with OpenGL ##########

option(OPTION_USE_OPENGL "Build with OpenGL support" ON)

if (OPTION_USE_OPENGL)
	if (MSVC)
		set(HAVE_OPENGL TRUE)
		set(HAVE_GL_GLUT_H TRUE)
		set(HAVE_GL_GLU_H TRUE)
		set(HAVE_GL_GL_H TRUE)
	else()
		find_file(HAVE_GL_GL_H GL/gl.h)
		find_file(HAVE_OPENGL_GL_H OpenGL/gl.h)
		find_file(HAVE_GL_GLU_H GL/glu.h)
		find_file(HAVE_OPENGL_GLU_H OpenGL/glu.h)
		find_file(HAVE_GL_GLUT_H GL/glut.h)
		find_file(HAVE_GLUT_GLUT_H GLUT/glut.h)
	
		include(FindOpenGL)
		include(FindGLUT)
		if(NOT OPENGL_FOUND)
			message(FATAL_ERROR, "OpenGL not found.  Building without OpenGL is possible.")
		endif()
		if(NOT GLUT_FOUND)
			message(FATAL_ERROR, "Glut not found.  Building without OpenGL is possible.")
		endif()
#		set(HAVE_OPENGL TRUE)
		include_directories(${OPEN_GL_INCLUDE_DIR} ${GLUT_INCLUDE_DIR})
		list(APPEND DEP_LIBS ${GLUT_glut_LIBRARY} ${OPENGL_gl_LIBRARY})
	endif()
endif()


####### Option: Build with LibTiff ##########
set(OPTION_USE_LIBTIFF OFF)

####### Option: Build with Zlib ##########
if (MSVC) 
	set(OPTION_USE_ZLIB OFF)
	set(HAVE_ZLIB TRUE)
else()
	option(OPTION_USE_ZLIB "Build with Zlib support" ON)
endif()

if(OPTION_USE_ZLIB)
	include(FindZLIB)
	if(NOT ZLIB_FOUND)
		message(FATAL_ERROR, "Zlib not found.  Builiding without Zlib is possible.")
	endif()
	set(HAVE_ZLIB TRUE)
	include_directories(${ZLIB_INCLUDE_DIRS})
#	link_directories(${ZLIB_LINK_DIRECTORIES})
#	list(APPEND DEP_LIBS z)
	list(APPEND DEP_LIBS ${ZLIB_LIBRARY})
endif(OPTION_USE_ZLIB)


####### Option: Build with Libmoleculizer ##########

option(OPTION_USE_LIBMZR "Include Libmoleculizer module" OFF)

if(OPTION_USE_LIBMZR)
	include(FindLibXml2)
	if(NOT LIBXML2_FOUND)
		message(FATAL_ERROR "LibXml2 not found.  Building without Libmoleculizer is possible.")
	endif()
	set(HAVE_LIBXML2 TRUE)

# LibXML++ version 1.0.5 is sufficient and advised.  Higher version numbers have more dependencies.
	find_file(HAVE_LIBXML++ libxml++.h PATHS /usr/local/include/libxml++-1.0/libxml++)
	message(STATUS "HAVE_LIBXML++='${HAVE_LIBXML++}'")
	find_path(LIBXML++_INCLUDE_DIR libxml++.h PATHS /usr/local/include/libxml++-1.0/libxml++)
	message(STATUS "LIBXML++_INCLUDE_DIR='${LIBXML++_INCLUDE_DIR}'")
	find_library(LIBXML++_LIBRARIES NAMES libxml++ libxml++-1.0.a PATHS /usr/local/lib)
	message(STATUS "LIBXML++_LIBRARIES='${LIBXML++_LIBRARIES}'")

	if(HAVE_LIBXML++ AND LIBXML++_INCLUDE_DIR AND LIBXML++_LIBRARIES)
		message(STATUS "LibXML++ found!")
	else()
		message(FATAL_ERROR "LibXML++ not found.  Building without Libmoleculizer is possible.")
	endif()
	set(HAVE_LIBXML++ TRUE)


	include(FindPythonInterp)
	include(FindPythonLibs)
	if(NOT PYTHONLIBS_FOUND)
		message(FATAL_ERROR, "PythonLibs not found.")
	endif()
	set(HAVE_PYTHONLIBS TRUE)


	add_subdirectory(source/libmoleculizer-1.1.2)
	set(LIBMZR_INCLUDE_DIR libmzr_c_interface.h PATHS source/libmoleculizer-1.1.2/src)
	find_library(LIBMZR_LIBRARIES NAMES libmoleculizer.a PATHS source/libmoleculizer-1.1.2/cmake)


	include_directories(${LIBXML2_INCLUDE_DIR})
	include_directories(${LIBXML++_INCLUDE_DIR})
	include_directories(${PYTHON_INCLUDE_DIRS})
	include_directories(${LIBMZR_INCLUDE_DIR})
	list(APPEND DEP_LIBS ${LIBXML2_LIBRARIES})
	list(APPEND DEP_LIBS ${LIBXML++_LIBRARIES})
	list(APPEND DEP_LIBS ${LIBMZR_LIBRARIES})
	list(APPEND DEP_LIBS ${PYTHON_LIBRARIES})
	set(LIBMOLECULIZER TRUE)
endif(OPTION_USE_LIBMZR)

	add_definitions(-D_CRT_SECURE_NO_WARNINGS)
	ADD_DEFINITIONS(-DVCELL)
	include_directories(../IDAWin ../ExpressionParser ../VCell/include ${CMAKE_CURRENT_BINARY_DIR})	
	include_directories(${JMS_INCS})
	add_definitions(${JMS_DEFS})
	list(APPEND DEP_LIBS hdf5_cpp hdf5 ${JMS_LIBS} zip)
	if (MSVC OR LINUX)
		list(APPEND DEP_LIBS blas)
	endif()

	if (OPTION_USE_OPENGL)
		if (MSVC)
			include_directories(${CMAKE_CURRENT_SOURCE_DIR}/windows/glut-3.7.6)
			link_directories(${CMAKE_CURRENT_SOURCE_DIR}/source/vcell ${CMAKE_CURRENT_SOURCE_DIR}/windows/glut-3.7.6)
			if (ARCH_64bit)
				list(APPEND DEP_LIBS glut64)
			else()
				list(APPEND DEP_LIBS glut32)
			endif()
		endif()
	endif()
	
	if (ZLIB_FOUND)
		list(APPEND DEP_LIBS z)
	else()
		list(APPEND DEP_LIBS zlib)
	endif()
	


####### Targets ##########

option(OPTION_TARGET_SMOLDYN "Create stand-alone Smoldyn program" ON)
option(OPTION_TARGET_LIBSMOLDYN "Create LibSmoldyn library" OFF)

if(OPTION_TARGET_SMOLDYN)
	set(EXE_FILE smoldyn)
	if (ARCH_64bit) 
		set(EXE_FILE ${EXE_FILE}_x64)
	elseif (MACOSX_BINARIES_ppc)
		set(EXE_FILE ${EXE_FILE}_ppc)
	endif()

	add_executable(${EXE_FILE} ${SRC_FILES} ${MAIN_FILES} ${HEADER_FILES})
	target_link_libraries(${EXE_FILE} ${DEP_LIBS})
	if (OPTION_USE_OPENGL)
		set_property(TARGET ${EXE_FILE} PROPERTY COMPILE_FLAGS "-DHAVE_OPENGL")
	endif()
endif()

	set(OPTION_TARGET_LIBSMOLDYN ON) 
	add_library(smoldynlib STATIC ${SRC_FILES} ${HEADER_FILES} ${HYBRID_SRC_FILES} ${HYBRID_HEADER_FILES})
	set_property(TARGET smoldynlib PROPERTY COMPILE_FLAGS "-DVCELL_HYBRID")


########## install ###########

    install(TARGETS ${EXE_FILE} RUNTIME DESTINATION bin)

########## configure file #########

configure_file (
	${CMAKE_CURRENT_SOURCE_DIR}/source/smoldynconfigure.h.in
	${CMAKE_CURRENT_BINARY_DIR}/smoldynconfigure.h
)

