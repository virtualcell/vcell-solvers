#!/bin/bash

executables=( 
  "FiniteVolume_x64" 
  "Hybrid_EM_x64" 
  "Hybrid_MIL_Adaptive_x64" 
  "Hybrid_MIL_x64" 
  "MovingBoundary" 
  "NFsim_x64" 
  "smoldyn_x64" 
  "SundialsSolverStandalone_x64" 
  "VCellChombo2D_x64" 
  "VCellChombo3D_x64" 
  "VCellStoch_x64"
)

libraries=(
  "libstdc++.6.dylib"
  "libgcc_s.1.dylib"
  "libgfortran.3.dylib"
  "libquadmath.0.dylib"
)

#
# change path to bundled dylibs in executables from absolute path (usr/local/lib/) to same directory
#
for exe in "${executables[@]}"; do
	for lib in "${libraries[@]}"; do
  		install_name_tool -change /usr/local/lib/$lib @executable_path/$lib  $exe
  	done
done

