#!/usr/bin/env bash

shopt -s -o nounset

builddir="build-linux64"

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
	-DLINUX_64bit_BINARIES=ON \
	-DLINUX_32bit_BINARIES=OFF \
	-DLINUX=ON \
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
