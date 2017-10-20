#!/bin/bash
builddir="build"
mkdir -p "$builddir/bin"
cd "$builddir"

if [ "$TRAVIS_OS_NAME" = "osx" ]
then
	platform=macos
else
	platform=linux64
fi

echo "== Building $platform binaries =="

# FIXME: These scripts assume /vagrant_numerics path prefix.
# probably want to generalize those scripts into something
# like $GIT_ROOT/build-scripts/macos/... which take the prefix
# as an argument, and use /vagrant_numerics by default maybe.
# Then the vagrant stuff can call them, and we can here as well.

# Build the code.
../VagrantBoxes/$platform/build.sh

# Copy dependencies.
../VagrantBoxes/$platform/moveDependentLibraries.sh

# Tweak the executables if needed.
installScript="../VagrantBoxes/$platform/install_name_tool_macos.sh"
test -f "$installScript" && sh "$installScript"
