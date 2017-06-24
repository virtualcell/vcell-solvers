#!/bin/bash

shopt -s -o nounset

executables=( 
  "FiniteVolume_x64" 
  "MovingBoundary_x64" 
  "NFsim_x64" 
  "smoldyn_x64" 
  "SundialsSolverStandalone_x64" 
  "testzip" 
  "VCellStoch_x64"
  "ziptool"
)

libraries=(
  "/usr/local/lib/libgfortran.4.dylib"
  "/usr/local/lib/libquadmath.0.dylib"
  "/usr/local/Cellar/hdf5@1.8/1.8.19/lib/libhdf5_cpp.14.dylib"
  "/usr/local/Cellar/hdf5@1.8/1.8.19/lib/libhdf5.10.dylib"
  "/usr/local/Cellar/hdf5@1.8/1.8.19/lib/libhdf5_hl_cpp.11.dylib"
  "/usr/local/Cellar/hdf5@1.8/1.8.19/lib/libhdf5_hl.10.dylib"
  "/usr/local/opt/szip/lib/libsz.2.dylib"
  "/usr/local/lib/libgcc_s.1.dylib"
  "/vagrant_numerics/build-macos-clang/bin/libzip.3.0.dylib"
  "/vagrant_numerics/build-macos-clang/bin/libzip.3.dylib"
  "/vagrant_numerics/build-macos-clang/bin/libzip.dylib"
)

#
# change path to bundled dylibs in executables from absolute path (usr/local/lib/) to same directory
#
for exe in "${executables[@]}"; do
	for lib in "${libraries[@]}"; do
		libfilename=${lib##*/}
#  		echo install_name_tool -change $lib @executable_path/$libfilename  $exe
  		     install_name_tool -change $lib @executable_path/$libfilename  $exe
#  		echo install_name_tool -add_rpath "@loader_path"  $exe
  		     install_name_tool -add_rpath "@loader_path"  $exe
  	done
done

#
# change path from each dylib to each other (dylibs can refer to other dylibs) from /usr/local/lib to @loader_path (same directory)
# if there is not dependency, then ignore the errors.
#
for lib in "${libraries[@]}"; do
	libfilename=${lib##*/}
# 	echo install_name_tool -id "@loader_path/$libfilename"  $libfilename
  	     install_name_tool -id "@loader_path/$libfilename"  $libfilename
	for dependentlib in "${libraries[@]}"; do
		dependentlibfilename=${dependentlib##*/}
#  		echo install_name_tool -change $dependentlib @loader_path/$dependentlibfilename  $libfilename
  		     install_name_tool -change $dependentlib @loader_path/$dependentlibfilename  $libfilename
  	done
done
