$Env:Path = "c:\msys64\mingw64\bin" + ";" + "c:\msys64\usr\bin" + ";" + $Env:Path
mkdir tmp
mkdir build
cd build
mkdir bin
Write-Host "calling cmake in " + $pwd

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

make

Get-ChildItem C:\projects\vcell-solvers\build\bin\
Remove-Item C:\projects\vcell-solvers\build\bin\NFsim_x64.exe
Get-ChildItem C:\projects\vcell-solvers\build\bin\
Move-Item C:\projects\vcell-solvers\build\bin\*.exe C:\projects\vcell-solvers\tmp
Move-Item C:\projects\vcell-solvers\build\bin\*.dll C:\projects\vcell-solvers\tmp
Get-ChildItem C:\projects\vcell-solvers\tmp\
Remove-Item C:\projects\vcell-solvers\build\* -Recurse -Force
mkdir bin

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

make

Move-Item C:\projects\vcell-solvers\tmp\* C:\projects\vcell-solvers\build\bin
Remove-Item C:\projects\vcell-solvers\tmp
