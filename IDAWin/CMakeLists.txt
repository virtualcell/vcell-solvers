project(IDAWin)

set (SRC_FILES 
	OdeResultSet.cpp
	StoppedByUserException.cpp	
	VCellCVodeSolver.cpp
	VCellIDASolver.cpp
	VCellSundialsSolver.cpp
)
set (HEADER_FILES 
	OdeResultSet.h
	StoppedByUserException.h	
	VCellCVodeSolver.h
	VCellIDASolver.h
	VCellSundialsSolver.h
)

set (EXE_SRC_FILES
	SundialsSolverStandalone.cpp
)

add_library(IDAWin STATIC ${SRC_FILES} ${HEADER_FILES})
target_link_libraries(IDAWin sundials ExpressionParser vcellmessaging)
set(EXE_FILE SundialsSolverStandalone)
if (ARCH_64bit) 
	set(EXE_FILE ${EXE_FILE}_x64)
endif()

add_executable(${EXE_FILE} ${EXE_SRC_FILES})	
target_link_libraries(${EXE_FILE} IDAWin)

install(TARGETS ${EXE_FILE} RUNTIME DESTINATION ${OPTION_EXE_DIRECTORY})
install(TARGETS IDAWin ARCHIVE DESTINATION bin)


enable_testing()

if (MINGW)
	set(test_sundials_exe ${CMAKE_BINARY_DIR}/bin/${EXE_FILE}.exe)
	set(python_cmd py)
else (MINGW)
	set(test_sundials_exe ${CMAKE_BINARY_DIR}/bin/${EXE_FILE})
	set(python_cmd python3)
endif (MINGW)
set(test_dir ${CMAKE_CURRENT_SOURCE_DIR}/tests/smoke)

# smoke test as a python script, for bash test example, see NFsim/tests/smoke
add_test(NAME ${EXE_FILE}_smoke COMMAND ${python_cmd} ${test_dir}/smoke.py ${test_sundials_exe} WORKING_DIRECTORY ${test_dir})


add_executable(
		hello_test
		hello_test.cpp
)
target_link_libraries(
		hello_test
		GTest::gtest_main
)

include(GoogleTest)
gtest_discover_tests(hello_test)