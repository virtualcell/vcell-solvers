project(NFsim)

set(SUB_DIRS 
    src 
    src/nauty24 
    src/NFcore 
    src/NFcore/moleculeLists 
    src/NFcore/reactionSelector 
    src/NFfunction 
    src/NFfunction/muParser
    src/NFinput 
    src/NFinput/TinyXML
    src/NFoutput 
    src/NFreactions
    src/NFreactions/mappings
    src/NFreactions/reactantLists
    src/NFreactions/reactions
    src/NFreactions/transformations
    src/NFscheduler 
    src/NFutil 
    src/NFutil/MTrand
)

aux_source_directory(src 															src_files)
aux_source_directory(src/nauty24 											nauty24_files) 
aux_source_directory(src/NFcore 											NFcore_files)
aux_source_directory(src/NFcore/moleculeLists 				NFcore_moleculeLists_files)
aux_source_directory(src/NFcore/reactionSelector			NFcore_reactionSelector_files)
aux_source_directory(src/NFfunction										NFfunction_files)
aux_source_directory(src/NFfunction/muParser					NFfunction_muParse_files)
aux_source_directory(src/NFinput											NFinput_files) 
aux_source_directory(src/NFinput/TinyXML							NFinput_TinyXML_files)
aux_source_directory(src/NFoutput											NFoutput_files) 
aux_source_directory(src/NFreactions									NFreactions_files)
aux_source_directory(src/NFreactions/mappings					NFreaction_mappings_files)
aux_source_directory(src/NFreactions/reactantLists		NFreactions_reactantLists_files)
aux_source_directory(src/NFreactions/reactions				NFreactions_reactions_files)
aux_source_directory(src/NFreactions/transformations	NFreactions_transformations_files)	
aux_source_directory(src/NFscheduler 									NFscheduler_files)
aux_source_directory(src/NFtest 											NFtest_files)
aux_source_directory(src/NFtest/agentcell							NFtest_agentcell_files)
aux_source_directory(src/NFtest/agentcell/cell				NFtest_agentcell_cell_files)
aux_source_directory(src/NFtest/simple_system					NFtest_simple_system_files)
aux_source_directory(src/NFtest/tlbr									NFtest_tlbr_files)
aux_source_directory(src/NFtest/transcription					NFtest_transcription_files)
aux_source_directory(src/NFutil 											NFutil_files)
aux_source_directory(src/NFutil/MTrand								MTrand_files)
aux_source_directory(VCell/src								VCell_glue_files)

set(SRC_FILES 
    ${src_files} 
    ${nauty24_files} 
    ${NFcore_files} 
    ${NFcore_moleculeLists_files}  
    ${NFcore_reactionSelector_files} 
    ${NFfunction_files} 
    ${NFfunction_muParse_files} 
    ${NFinput_files} 
    ${NFinput_TinyXML_files} 
    ${NFoutput_files} 
    ${NFreactions_files} 
    ${NFreaction_mappings_files} 
    ${NFreactions_reactantLists_files} 
    ${NFreactions_reactions_files} 
    ${NFreactions_transformations_files}
    ${NFscheduler_files}
    ${NFtest_files}
    ${NFtest_agentcell_files}
    ${NFtest_agentcell_cell_files}
    ${NFtest_simple_system_files}
    ${NFtest_tlbr_files}
    ${NFtest_transcription_files}
    ${NFutil_files}
    ${MTrand_files}
    ${VCell_glue_files}
)

if (MSVC)
	add_definitions(-DHAVE_UNISTD_H=0)
endif()

if (UNIX)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x -fpermissive")
  set(CXX_FLAGS "${CXX_FLAGS} -std=c++0x -fpermissive")
endif (UNIX) 

set(exe_target NFsim)
if (ARCH_64bit) 
	set(exe_target ${exe_target}_x64)
endif (ARCH_64bit)

enable_testing()

add_executable(${exe_target} ${SRC_FILES})
target_link_libraries(${exe_target} vcommons vcellmessaging)

if (MINGW)
    set(test_nfsim_exe ${CMAKE_BINARY_DIR}/bin/${exe_target}.exe)
    set(python_cmd py)
else (MINGW)
    set(test_nfsim_exe ${CMAKE_BINARY_DIR}/bin/${exe_target})
    set(python_cmd python3)
endif (MINGW)
set(test_dir ${CMAKE_CURRENT_SOURCE_DIR}/tests/smoke)

# smoke test as a python script
add_test(NAME ${exe_target}_smoke_python COMMAND ${python_cmd} ${test_dir}/smoke.py ${test_nfsim_exe} WORKING_DIRECTORY ${test_dir})

# smoke test as a bash script
add_test(NAME ${exe_target}_smoke_bash COMMAND bash -c "${test_dir}/smoke.sh ${test_nfsim_exe}" WORKING_DIRECTORY ${test_dir})

install(TARGETS ${exe_target} RUNTIME DESTINATION ${OPTION_EXE_DIRECTORY})

