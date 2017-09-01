#!/usr/bin/env bash

shopt -s -o nounset

builddir="/vagrant_numerics/build-linux64/bin"

if [ ! -e  "$builddir" ]; then
	echo "skipping client build, directory $builddir missing"
else
	cd $builddir
	cp /usr/lib64/libhdf5_cpp.so.8 .
	cp /usr/lib64/libhdf5.so.8 .
	cp /usr/lib64/libactivemq-cpp.so.19 .
	cp /usr/lib64/libgfortran.so.3 .
	cp /usr/lib64/libquadmath.so.0 .
	cp /usr/lib64/libapr-1.so.0 .
fi


builddir="/vagrant_numerics/build-linux64-server/bin"

if [ ! -e  "$builddir" ]; then
	echo "skipping client build, directory $builddir missing"
else
	cd $builddir
	cp /usr/lib64/libhdf5_cpp.so.8 .
	cp /usr/lib64/libhdf5.so.8 .
	cp /usr/lib64/libactivemq-cpp.so.19 .
	cp /usr/lib64/libgfortran.so.3 .
	cp /usr/lib64/libquadmath.so.0 .
	cp /usr/lib64/libapr-1.so.0 .
fi

