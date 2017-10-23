#!/bin/bash
builddir="build"
mkdir -p "$builddir/bin"
cd "$builddir"

if [ "$TRAVIS_OS_NAME" = "osx" ]
then
	platform=macos
else
	platform=linux64
	
	echo "working dir is $PWD"
	
	cmake --version

	make \
		-DCMAKE_PREFIX_PATH="/usr/lib/x86_64-linux-gnu/" \
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
		-DOPTION_TARGET_HY3S_SOLVERS=OFF \
		..

	make

fi

#echo "== Building $platform binaries =="

# FIXME: These scripts assume /vagrant_numerics path prefix.
# probably want to generalize those scripts into something
# like $GIT_ROOT/build-scripts/macos/... which take the prefix
# as an argument, and use /vagrant_numerics by default maybe.
# Then the vagrant stuff can call them, and we can here as well.

# Build the code.
#../VagrantBoxes/$platform/build.sh

# Copy dependencies.
#../VagrantBoxes/$platform/moveDependentLibraries.sh

# Tweak the executables if needed.
#installScript="../VagrantBoxes/$platform/install_name_tool_macos.sh"
#test -f "$installScript" && sh "$installScript"
