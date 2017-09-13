#!/usr/bin/env bash

shopt -s -o nounset

builddir="/vagrant_numerics/build-linux64/bin"

if [ ! -e  "$builddir" ]; then
	echo "skipping client build, directory $builddir missing"
else
	cd $builddir
	cp /lib64/libhdf5_cpp.so.8 .
	cp /lib64/libhdf5.so.8 .
	cp /lib64/libhdf5_hl_cpp.so.8 .
	cp /lib64/libhdf5_hl.so.8 .
	cp /lib64/libgfortran.so.3 .
	cp /lib64/libquadmath.so.0 .
	cp /lib64/libz.so.1 .
	cp /lib64/libdl.so.2 .
	cp /lib64/libactivemq-cpp.so.19 .
	cp /lib64/libapr-1.so.0 .
fi


builddir="/vagrant_numerics/build-linux64-server/bin"

if [ ! -e  "$builddir" ]; then
	echo "skipping client build, directory $builddir missing"
else
	cd $builddir
	cp /lib64/libhdf5_cpp.so.8 .
	cp /lib64/libhdf5.so.8 .
	cp /lib64/libhdf5_hl_cpp.so.8 .
	cp /lib64/libhdf5_hl.so.8 .
	cp /lib64/libgfortran.so.3 .
	cp /lib64/libquadmath.so.0 .
	cp /lib64/libz.so.1 .
	cp /lib64/libdl.so.2 .
	cp /lib64/libactivemq-cpp.so.19 .
	cp /lib64/libapr-1.so.0 .
fi

