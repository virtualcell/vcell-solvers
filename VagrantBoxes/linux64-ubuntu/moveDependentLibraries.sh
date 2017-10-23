#!/usr/bin/env bash

shopt -s -o nounset

builddir="/vagrant_numerics/build-linux64-ubuntu/bin"

if [ ! -e  "$builddir" ]; then
	echo "skipping client build, directory $builddir missing"
else
	cd $builddir
	cp /usr/lib/x86_64-linux-gnu/libhdf5_cpp.so.7 .
	cp /usr/lib/x86_64-linux-gnu/libhdf5.so.7 .
	cp /usr/lib/x86_64-linux-gnu/libhdf5_hl_cpp.so.7 .
	cp /usr/lib/x86_64-linux-gnu/libhdf5_hl.so.7 .
	cp /usr/lib/x86_64-linux-gnu/libgfortran.so.3 .
	cp /usr/lib/x86_64-linux-gnu/libquadmath.so.0 .
#	cp /usr/lib/x86_64-linux-gnu/libz.so.1 .
#	cp /usr/lib/x86_64-linux-gnu/libdl.so.2 .
#	cp /usr/lib/x86_64-linux-gnu/libactivemq-cpp.so.19 .
#	cp /usr/lib/x86_64-linux-gnu/libapr-1.so.0 .
fi


builddir="/vagrant_numerics/build-linux64-server/bin"

if [ ! -e  "$builddir" ]; then
	echo "skipping client build, directory $builddir missing"
else
	cd $builddir
#	cp /usr/lib/x86_64-linux-gnu/libhdf5_cpp.so.8 .
#	cp /usr/lib/x86_64-linux-gnu/libhdf5.so.8 .
#	cp /usr/lib/x86_64-linux-gnu/libhdf5_hl_cpp.so.8 .
#	cp /usr/lib/x86_64-linux-gnu/libhdf5_hl.so.8 .
#	cp /usr/lib/x86_64-linux-gnu/libgfortran.so.3 .
#	cp /usr/lib/x86_64-linux-gnu/libquadmath.so.0 .
#	cp /usr/lib/x86_64-linux-gnu/libz.so.1 .
#	cp /usr/lib/x86_64-linux-gnu/libdl.so.2 .
#	cp /usr/lib/x86_64-linux-gnu/libactivemq-cpp.so.19 .
#	cp /usr/lib/x86_64-linux-gnu/libapr-1.so.0 .
fi

