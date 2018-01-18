#!/bin/bash
if [ "$TRAVIS_OS_NAME" = "osx" ]
then
  echo "== Installing macOS dependencies =="
  curl -fsO "https://downloads.sourceforge.net/project/hpc/hpc/g95/gfortran-7.1-bin.tar.gz?r=&ts=1508458128&use_mirror=svwh"
  gunzip gfortran-7.1-bin.tar.gz
  sudo tar -xvf gfortran-7.1-bin.tar -C / 
  brew install cmake
  brew install boost
  brew install hdf5@1.8 --c++11
else
  echo "== Installing Linux dependencies =="
  sudo apt-get -qq update
    sudo apt-get install -y -qq -o=Dpkg::Use-Pty=0  gfortran
    sudo apt-get install -y -qq -o=Dpkg::Use-Pty=0  zlib1g-dev
    sudo apt-get install -y -qq -o=Dpkg::Use-Pty=0  libhdf5-dev    
    
#    wget https://cmake.org/files/v3.8/cmake-3.8.1.tar.gz
#    sudo tar xzf cmake-3.8.1.tar.gz
#    cd cmake-3.8.1
#    sudo ./configure --prefix=/opt/cmake
#    sudo make --quiet 
#    sudo make --quiet install
fi
