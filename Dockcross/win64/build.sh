#!/usr/bin/env bash
#
# <dockcross>
#     windows-x64
#          dockcross-win64 (dockcross executable shell script - maps to $dockcross var)
#
# <solverparent>         (runs dockcross from here - maps to /work)
#     vcell-solvers   (src dir - maps to /work/vcell-solvers)
#          Dockcross
#             win64   (Script dir)
#          build-dockcross-win64 (build dir - maps to /work/vcell-solvers/build-dockcross-win64)
#             bin
#
# vcell-solvers/Dockcross/win64
SCRIPTDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

SOLVERSDIR=$SCRIPTDIR/../..
BUILDDIR=$SOLVERSDIR/build-dockcross-win64
BINDIR=$BUILDDIR/bin

mkdir $BUILDDIR
mkdir $BINDIR

cd $SOLVERSDIR/..

#  -G "Unix Makefiles" \
#  --trace --debug --debug-output \

DOCKCROSS=/Users/schaff/build/dockcross/windows-x64/dockcross-win64

$DOCKCROSS bash -c "cmake \
	-Bvcell-solvers/build-dockcross-win64 \
	-Hvcell-solvers \
	-DCMAKE_BUILD_TYPE="Release" \
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
	-DOPTION_TARGET_HY3S_SOLVERS=OFF"

#$dockcross make

#Write-Host "install standard mingw dlls"
#copy c:\tools\msys64\mingw64\bin\*.dll bin
