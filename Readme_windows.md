## Contents
------------

1. [Requirements](#requirements)
2. [MinGW and dependencies](#installing-mingw-and-dependencies)
3. [Build solvers via MinGW](#installing-mingw-and-dependencies)
    1. [Building all solvers](#for-building-all-solvers-via-mingw)
    2. [Building NFSim](#for-building-nfsim-via-mingw)
4. [Build solvers via PowerShell](#for-building-solvers-manually-from-powershell)
    1. [Building all solvers](#building-all-solvers-via-powershell)
    2. [Building NFSim](#for-building-nfsim-via-powershell)

### Requirements

    1. git
    2. MinGW

### Installing MinGW and dependencies

1. Download and install the latest MinGW for windows
2. Open MinGW
    - Download and install these dependencies

```
    pacman -S git
    pacman --noconfirm -Syu
    pacman --noconfirm -Syyu python mingw-w64-x86_64-gcc
    pacman --noconfirm -S msys/git
    pacman --noconfirm -S mingw64/mingw-w64-x86_64-gcc-fortran
    pacman --noconfirm -S mingw64/mingw-w64-x86_64-cmake
    pacman --noconfirm -S mingw64/mingw-w64-x86_64-doxygen
    pacman --noconfirm -S msys/make
    pacman --noconfirm -S mingw-w64-x86_64-pkg-config
    pacman --noconfirm -S mingw-w64-x86_64-hdf5
    pacman --noconfirm -S mingw-w64-x86_64-libzip
    pacman --noconfirm -S mingw-w64-x86_64-netcdf
```

### For building solvers manually from MinGW

1. Open MinGW and redirect to vcell-solvers cloned directory
2. `mkdir -p build/bin`
3. `cd build`

#### For building all solvers via MinGW

```
    cmake -G "Unix Makefiles" \
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
    -DOPTION_TARGET_HY3S_SOLVERS=OFF \
    ..
```

`make`

:exclamation:**Note: Remove wrongly built NFSim solver and build seperately

#### For building NFSim via MinGW

```
    cmake -G "Unix Makefiles" \
    -DBUILD_SHARED_LIBS=OFF \
    -DCMAKE_FIND_LIBRARY_SUFFIXES=".a" \
    -DCMAKE_EXE_LINKER_FLAGS="-static" \
    -DOPTION_TARGET_MESSAGING=OFF \
    -DOPTION_TARGET_PARALLEL=OFF \
    -DOPTION_TARGET_CHOMBO2D_SOLVER=OFF \
    -DOPTION_TARGET_CHOMBO3D_SOLVER=OFF \
    -DOPTION_TARGET_SMOLDYN_SOLVER=OFF \
    -DOPTION_TARGET_FV_SOLVER=OFF \
    -DOPTION_TARGET_STOCHASTIC_SOLVER=OFF \
    -DOPTION_TARGET_NFSIM_SOLVER=ON \
    -DOPTION_TARGET_MOVINGBOUNDARY_SOLVER=OFF \
    -DOPTION_TARGET_SUNDIALS_SOLVER=OFF \
    -DOPTION_TARGET_HY3S_SOLVERS=OFF \
    ..
```

`make`

### For building solvers manually from Powershell

1. Open powershell and redirect to vcell-solvers cloned directory
2. Export the path `$Env:Path = "c:\msys64\mingw64\bin" + ";" + "c:\msys64\usr\bin" + ";" + $Env:Path`
3. `mkdir -p build/bin`
4. `cd build`

#### Building all solvers via Powershell

```
    cmake -G "Unix Makefiles" `
    -DCMAKE_BUILD_TYPE="Release" `
    -DOPTION_TARGET_MESSAGING=OFF `
    -DOPTION_TARGET_PARALLEL=OFF `
    -DOPTION_TARGET_CHOMBO2D_SOLVER=OFF `
    -DOPTION_TARGET_CHOMBO3D_SOLVER=OFF `
    -DOPTION_TARGET_SMOLDYN_SOLVER=ON `
    -DOPTION_TARGET_FV_SOLVER=ON `
    -DOPTION_TARGET_STOCHASTIC_SOLVER=ON `
    -DOPTION_TARGET_NFSIM_SOLVER=ON `
    -DOPTION_TARGET_MOVINGBOUNDARY_SOLVER=OFF `
    -DOPTION_TARGET_SUNDIALS_SOLVER=ON `
    -DOPTION_TARGET_HY3S_SOLVERS=OFF `
    ..
```

`make`

:exclamation:**Note: Remove wrongly built NFSim solver if you want build in same directory

#### For building NFSim via Powershell

```
    cmake -G "Unix Makefiles" `
    -DBUILD_SHARED_LIBS=OFF `
    -DCMAKE_FIND_LIBRARY_SUFFIXES=".a" `
    -DCMAKE_EXE_LINKER_FLAGS="-static" `
    -DOPTION_TARGET_MESSAGING=OFF `
    -DOPTION_TARGET_PARALLEL=OFF `
    -DOPTION_TARGET_CHOMBO2D_SOLVER=OFF `
    -DOPTION_TARGET_CHOMBO3D_SOLVER=OFF `
    -DOPTION_TARGET_SMOLDYN_SOLVER=OFF `
    -DOPTION_TARGET_FV_SOLVER=OFF `
    -DOPTION_TARGET_STOCHASTIC_SOLVER=OFF `
    -DOPTION_TARGET_NFSIM_SOLVER=ON `
    -DOPTION_TARGET_MOVINGBOUNDARY_SOLVER=OFF `
    -DOPTION_TARGET_SUNDIALS_SOLVER=OFF `
    -DOPTION_TARGET_HY3S_SOLVERS=OFF `
    ..
```

`make`
