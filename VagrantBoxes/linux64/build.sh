#!/usr/bin/env bash

shopt -s -o nounset

builddir="build-linux64-server"

echo "cd /vagrant_numerics"
cd /vagrant_numerics
echo "making directory ${builddir}"
mkdir $builddir
echo "cd ${builddir}"
cd $builddir
echo "making directory ${builddir}/bin"
mkdir bin

/opt/cmake/bin/cmake \
	-DOPTION_TARGET_MESSAGING=ON \
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

make

# cp /usr/local/Cellar/hdf5@1.8/1.8.19/lib/libhdf5_cpp.14.dylib bin
# cp /usr/local/Cellar/hdf5@1.8/1.8.19/lib/libhdf5.10.dylib bin
# cp /usr/local/Cellar/hdf5@1.8/1.8.19/lib/libhdf5_hl_cpp.11.dylib bin
# cp /usr/local/Cellar/hdf5@1.8/1.8.19/lib/libhdf5_hl.10.dylib bin
# cp /usr/local/lib/libgfortran.4.dylib bin
# cp /usr/local/lib/libquadmath.0.dylib bin
# cp /usr/local/opt/szip/lib/libsz.2.dylib bin
# cp /usr/local/lib/libgcc_s.1.dylib bin
