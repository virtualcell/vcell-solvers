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
  sudo apt-get -qq update
  sudo apt-get install -y cmake
  sudo apt-get install -y libsqlite0-dev
  sudo apt-get install -y openssl-devel
  sudo apt-get install -y hdf5-devel
  sudo apt-get install -y libboost-dev
  sudo apt-get install -y activemq-cpp-devel
  sudo apt-get install -y libhdf5-dev
  sudo apt-get install -y libgfortran-5-dev
  sudo apt-get install -y zlib1g-dev
fi
