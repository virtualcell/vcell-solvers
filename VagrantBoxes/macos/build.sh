#!/usr/bin/env bash

shopt -s -o nounset

builddir="build-macos"

echo "cd /vagrant_numerics"
cd /vagrant_numerics
echo "making directory ${builddir}"
mkdir $builddir
echo "cd ${builddir}"
cd $builddir
echo "making directory ${builddir}/bin"
mkdir bin

/usr/local/bin/cmake \
	-G "Unix Makefiles" \
	-DCMAKE_PREFIX_PATH="/usr/local/opt/hdf5@1.8" \
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

make
