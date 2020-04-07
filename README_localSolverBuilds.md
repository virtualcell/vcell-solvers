# Compiling and building VCell-Solvers locally

## For all platforms
- clone the repo 

<img align="left" width="32px" id="windows" src="https://raw.githubusercontent.com/virtualcell/vcell-solvers/readme_instructions/asserts/windows.png">
<h4>Windows</h4>
<br style="clear:both"/>

### Instructions for Windows
- Requirements:
    - git
    - MinGW
1. Download and install the latest MinGW for windows
2. Open MinGW
    - Download and install these dependencies
        `pacman -S git`
        `pacman --noconfirm -Syu`
        `pacman --noconfirm -Syyu python mingw-w64-x86_64-gcc`
        `pacman --noconfirm -S msys/git`
        `pacman --noconfirm -S mingw64/mingw-w64-x86_64-gcc-fortran`
        `pacman --noconfirm -S mingw64/mingw-w64-x86_64-cmake`
        `pacman --noconfirm -S mingw64/mingw-w64-x86_64-doxygen`
        `pacman --noconfirm -S msys/make`
        `pacman --noconfirm -S mingw-w64-x86_64-pkg-config`
        `pacman --noconfirm -S mingw-w64-x86_64-hdf5`
        `pacman --noconfirm -S mingw-w64-x86_64-libzip`
        `pacman --noconfirm -S mingw-w64-x86_64-netcdf`
3. For building solvers manually from MinGW, redirect to vcell-solvers directory
    - `mkdir -p build/bin`
    For building all solvers
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

    For building NFSim
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
    :exclamation:**Note: Remove wrongly built NFSim solver and build seperately

<img align="left" width="32px" id="linux" src="https://raw.githubusercontent.com/virtualcell/vcell-solvers/readme_instructions/asserts/ubuntu.png">
<h4>Ubuntu</h4>
<br style="clear:both"/>

### Instructions for Ubuntu
- Requirements:
    - git 
    - cmake
    - CLION
1. For configuring the project 
    1. Open CLION go to `File > settings > Build, Execution, Deployement > Toolchains > +`  
        Let it detect everything automatically `CMake`, `Make`, `C Complier`, `C++ Compiler` and `Debugger`
    2. Configure CMake `File > settings > Build, Execution, Deployement > CMake > +`
        a. Build type as `Default`
        b. Give `CMake options`
            For all solvers
            ```
            -DCMAKE_PREFIX_PATH="/usr/lib/x86_64-linux-gnu/"
            -DOPTION_TARGET_MESSAGING=OFF
            -DOPTION_TARGET_PARALLEL=OFF
            -DOPTION_TARGET_CHOMBO2D_SOLVER=OFF
            -DOPTION_TARGET_CHOMBO3D_SOLVER=OFF
            -DOPTION_TARGET_SMOLDYN_SOLVER=ON
            -DOPTION_TARGET_FV_SOLVER=ON
            -DOPTION_TARGET_STOCHASTIC_SOLVER=ON
            -DOPTION_TARGET_NFSIM_SOLVER=ON
            -DOPTION_TARGET_MOVINGBOUNDARY_SOLVER=OFF
            -DOPTION_TARGET_SUNDIALS_SOLVER=ON
            -DOPTION_TARGET_HY3S_SOLVERS=OFF
            ```
        c. Generation path `build/bin`
        d. Apply all these changes builds every solver in `bin` directory.

<img align="left" width="32px" id="mac-osx" src="https://raw.githubusercontent.com/virtualcell/vcell-solvers/readme_instructions/asserts/macos.png">
<h4>Mac OS X (Legacy)</h4>
<br style="clear:both"/>

### Instructions for Mac


### Debugging
1. Program arguments

2. Environment variables

### CI/CD for all platforms

1. Appveyor for Windows

2. Travis for Mac and Linux
