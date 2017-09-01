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
#
SCRIPTDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

shopt -s -o nounset

if [ $# -eq 0 ]; then
	echo "usage: build.sh <path-to-dockcross-script>"
	echo "example: build.sh /Users/schaff/build/dockcross/windows-x64/dockcross-win64"
	exit -1
else
	DOCKCROSS=$1
	echo "using dockcross script $DOCKCROSS"
fi


if [ ! -e $DOCKCROSS ]; then
	echo "could not find dockcross script $DOCKCROSS"
	exit -1
fi


SOLVERSDIR=$SCRIPTDIR/../..
BUILDDIR=$SOLVERSDIR/build-dockcross-win64
BINDIR=$BUILDDIR/bin

mkdir $BUILDDIR
mkdir $BINDIR

cd $SOLVERSDIR

#  -G "Unix Makefiles" \
#  --trace --debug --debug-output \

$DOCKCROSS bash -c "cmake \
	-Wdev \
	--warn-uninitialized --warn-unused-vars and --check-system-vars \
	-debug-output \
	--trace \
	-DCMAKE_VERBOSE_MAKEFILE:BOOL=ON \
	-B/work/build-dockcross-win64 \
	-H/work \
	-DCMAKE_BUILD_TYPE="Release" \
	-DOPTION_TARGET_MESSAGING=OFF \
	-DOPTION_TARGET_PARALLEL=OFF \
	-DOPTION_TARGET_CHOMBO2D_SOLVER=OFF \
	-DOPTION_TARGET_CHOMBO3D_SOLVER=OFF \
	-DOPTION_TARGET_SMOLDYN_SOLVER=ON \
	-DOPTION_TARGET_FV_SOLVER=ON \
	-DOPTION_TARGET_STOCHASTIC_SOLVER=ON \
	-DOPTION_TARGET_NFSIM_SOLVER=ON \
	-DOPTION_TARGET_MOVINGBOUNDARY_SOLVER=OFF \
	-DOPTION_TARGET_SUNDIALS_SOLVER=ON \
	-DOPTION_TARGET_HY3S_SOLVERS=OFF"

cd $SOLVERSDIR
$DOCKCROSS bash -c "cd /work/build-dockcross-win64 ; make"

#Write-Host "install standard mingw dlls"
#copy c:\tools\msys64\mingw64\bin\*.dll bin
