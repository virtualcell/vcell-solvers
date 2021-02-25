$Env:Path = "c:\msys64\mingw64\bin" + ";" + "c:\msys64\usr\bin" + ";" + $Env:Path
mkdir tmp
mkdir build
Set-Location build
mkdir bin
Write-Host "calling cmake in " + $pwd


cmake --version

cmake -DCMAKE_BUILD_TYPE="Release" `
     -DCMAKE_C_COMPILER=C:/MinGW/bin/gcc.exe `
     -DCMAKE_CXX_COMPILER=C:/MinGW/bin/g++.exe `
     -DCMAKE_Fortran_COMPILER=C:/MinGW/bin/gfortran.exe `
     -DCMAKE_MAKE_PROGRAM=C:/MinGW/bin/mingw32-make.exe `
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
      .. -G "Unix Makefiles" `

make
<# Building NFSim solver seperately #>
# rm -rf -f !(bin)
# rm bin/NFsim_x64.exe
Write-Host "ls all files in bin directory" + $pwd
Get-ChildItem C:\projects\vcell-solvers\build\bin\
Write-Host "Removing wrong build NFsim_x64" + $pwd
Remove-Item C:\projects\vcell-solvers\build\bin\NFsim_x64.exe
Get-ChildItem C:\projects\vcell-solvers\build\bin\
Write-Host "Moving all solvers to tmp directory" + $pwd
Move-Item C:\projects\vcell-solvers\build\bin\*.exe C:\projects\vcell-solvers\tmp
Move-Item C:\projects\vcell-solvers\build\bin\*.dll C:\projects\vcell-solvers\tmp
Get-ChildItem C:\projects\vcell-solvers\tmp\
Write-Host "Removing everything from build directory" + $pwd
Remove-Item C:\projects\vcell-solvers\build\* -Recurse -Force
mkdir bin

# cmake -DBUILD_SHARED_LIBS=OFF `
#      -DCMAKE_FIND_LIBRARY_SUFFIXES=".a" `
#      -DCMAKE_EXE_LINKER_FLAGS="-static" `
#      -DOPTION_TARGET_MESSAGING=OFF `
#      -DOPTION_TARGET_PARALLEL=OFF `
#      -DOPTION_TARGET_CHOMBO2D_SOLVER=OFF `
#      -DOPTION_TARGET_CHOMBO3D_SOLVER=OFF `
#      -DOPTION_TARGET_SMOLDYN_SOLVER=OFF `
#      -DOPTION_TARGET_FV_SOLVER=OFF `
#      -DOPTION_TARGET_STOCHASTIC_SOLVER=OFF `
#      -DOPTION_TARGET_NFSIM_SOLVER=ON `
#      -DOPTION_TARGET_MOVINGBOUNDARY_SOLVER=OFF `
#      -DOPTION_TARGET_SUNDIALS_SOLVER=OFF `
#      -DOPTION_TARGET_HY3S_SOLVERS=OFF `
#       .. -G "Unix Makefiles" `

# make
# Write-Host "Moving all solvers from tmp directory to bin directory" + $pwd
# Move-Item C:\projects\vcell-solvers\tmp\*.exe C:\projects\vcell-solvers\build\bin
# Move-Item C:\projects\vcell-solvers\tmp\*.dll C:\projects\vcell-solvers\build\bin
# Remove-Item C:\projects\vcell-solvers\tmp
