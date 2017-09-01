#!/usr/bin/env bash

shopt -s -o nounset

builddir="build-macos"
cd /vagrant_numerics/${builddir}/bin

cp /usr/local/opt/hdf5@1.8/lib/libhdf5_cpp.14.dylib .
cp /usr/local/opt/hdf5@1.8/lib/libhdf5.10.dylib .
cp /usr/local/opt/szip/lib/libsz.2.dylib .
cp /usr/local/lib/libgfortran.4.dylib .
cp /usr/local/lib/libgcc_s.1.dylib .
cp /usr/local/lib/libquadmath.0.dylib .

#cp /usr/local/Cellar/hdf5@1.8/1.8.19/lib/libhdf5_cpp.14.dylib .
#cp /usr/local/Cellar/hdf5@1.8/1.8.19/lib/libhdf5.10.dylib .
#cp /usr/local/Cellar/hdf5@1.8/1.8.19/lib/libhdf5_hl_cpp.11.dylib .
#cp /usr/local/Cellar/hdf5@1.8/1.8.19/lib/libhdf5_hl.10.dylib .
#cp /usr/local/lib/libgfortran.4.dylib .
#cp /usr/local/lib/libquadmath.0.dylib .
#cp /usr/local/opt/szip/lib/libsz.2.dylib .
#cp /usr/local/lib/libgcc_s.1.dylib .

cd /vagrant_numerics/${builddir}/bin
chmod +w *
echo "fixing mac paths"
/vagrant/install_name_tool_macos.sh
