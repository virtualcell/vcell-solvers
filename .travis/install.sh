#!/bin/bash
if [ "$TRAVIS_OS_NAME" = "osx" ]
then
  echo "== Installing macOS dependencies =="
  curl -fsO "https://downloads.sourceforge.net/project/hpc/hpc/g95/gfortran-7.1-bin.tar.gz?r=&ts=1508458128&use_mirror=svwh"
  gunzip gfortran-7.1-bin.tar.gz
  sudo tar -xvf gfortran-7.1-bin.tar -C / 
  brew install cmake
  brew install boost
  brew tap homebrew/science
  brew install homebrew/science/hdf5@1.8 --c++11
else
  echo "== Installing Linux dependencies =="
  sudo apt-get -q -y install \
    cmake \
    libsqlite0-dev \
    openssl-devel \
    hdf5-devel \
    libboost-dev \
    activemq-cpp-devel \
    libhdf5-dev \
    libgfortran-5-dev \
    zlib1g-dev
fi
