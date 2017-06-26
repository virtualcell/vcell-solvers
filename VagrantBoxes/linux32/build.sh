#!/usr/bin/env bash

shopt -s -o nounset

builddir="build-linux32"

echo "cd /vagrant_numerics"
cd /vagrant_numerics
echo "making directory ${builddir}"
mkdir $builddir
echo "cd ${builddir}"
cd $builddir
echo "making directory ${builddir}/bin"
mkdir bin

/opt/cmake/bin/cmake \
	-DUNVERSIONED="True" \
	-DCMAKE_PREFIX_PATH="/usr/local/hdf5" \
	-DOPTION_TARGET_MESSAGING=OFF \
	-DOPTION_TARGET_PARALLEL=OFF \
	-DOPTION_TARGET_CHOMBO2D_SOLVER=OFF \
	-DOPTION_TARGET_CHOMBO3D_SOLVER=OFF \
	-DOPTION_TARGET_SMOLDYN_SOLVER=ON \
	-DOPTION_TARGET_FV_SOLVER=ON \
	-DOPTION_TARGET_STOCHASTIC_SOLVER=ON \
	-DOPTION_TARGET_NFSIM_SOLVER=ON \
	-DOPTION_TARGET_MOVINGBOUNDARY_SOLVER=ON \
	-DOPTION_TARGET_SUNDIALS_SOLVER=ON \
	-DOPTION_TARGET_HY3S_SOLVERS=OFF \
	..

#make

#cp /usr/local/hdf5/lib/libhdf5_cpp.so.14 bin
#cp /usr/local/hdf5/lib/libhdf5.so.10 bin
#cp /usr/local/hdf5/lib/libhdf5_hl_cpp.so.11 bin
#cp /usr/local/hdf5/lib/libhdf5_hl.so.10 bin
#cp /lib/libgfortran.so.3 bin
#cp /lib/libquadmath.so.0 bin
#cp /lib/libstdc++.so.6 bin
#cp /lib/libm.so.6 bin
#cp /lib/libgcc_s.so.1 bin
#cp /lib/libc.so.6 bin
#cp /lib/libz.so.1 bin

# cp /usr/local/opt/szip/lib/libsz.2.dylib bin
# cp /usr/local/lib/libgcc_s.1.dylib bin
