<img align="left" width="32px" id="windows" src="https://raw.githubusercontent.com/virtualcell/vcell-solvers/readme_instructions/asserts/windows.png">
<h4>Windows</h4>
<br style="clear:both"/>


# Requirements:

    - git
    - MinGW

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

## For building solvers manually from MinGW

    - redirect to vcell-solvers directory
    - `mkdir -p build/bin`
    - For building all solvers
    ```bash
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

    - :exclamation:**Note: Remove wrongly built NFSim solver and build seperately

    For building NFSim

    - `make clean`
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

## For building solvers manually from powershell

    - Open powershell and redirect to vcell-solvers directory
    - Export the path `$Env:Path = "c:\msys64\mingw64\bin" + ";" + "c:\msys64\usr\bin" + ";" + $Env:Path`
    - `mkdir -p build/bin`
    - `cd build`
    - Building all solvers
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
    - `make`

    - :exclamation:**Note: Remove wrongly built NFSim solver if you want build in same 

    - building only NFSim

    - ```
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
    - `make`
