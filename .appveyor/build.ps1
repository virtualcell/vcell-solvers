$Env:Path = "c:\msys64\mingw64\bin" + ";" + "c:\msys64\usr\bin" + ";" + $Env:Path

ls
Get-ChildItem

# mkdir tmp
# mkdir build
# cd build
# mkdir bin
# Write-Host "calling cmake in " + $pwd
    
# cmake -G "Unix Makefiles" `
#     -DCMAKE_BUILD_TYPE="Release" `
#     -DOPTION_TARGET_MESSAGING=OFF `
#     -DOPTION_TARGET_PARALLEL=OFF `
#     -DOPTION_TARGET_CHOMBO2D_SOLVER=OFF `
#     -DOPTION_TARGET_CHOMBO3D_SOLVER=OFF `
#     -DOPTION_TARGET_SMOLDYN_SOLVER=ON `
#     -DOPTION_TARGET_FV_SOLVER=ON `
#     -DOPTION_TARGET_STOCHASTIC_SOLVER=ON `
#     -DOPTION_TARGET_NFSIM_SOLVER=ON `
#     -DOPTION_TARGET_MOVINGBOUNDARY_SOLVER=OFF `
#     -DOPTION_TARGET_SUNDIALS_SOLVER=ON `
#     -DOPTION_TARGET_HY3S_SOLVERS=OFF `
#      ..
     
# make

# Remove-Item \projects\vcell-solvers\build\bin\NFsim_x64.exe
# Move-Item \projects\vcell-solvers\build\bin\* \projects\vcell-solvers\tmp
# Remove-Item \projects\vcell-solvers\build\*
# mkdir bin

# cmake -G "Unix Makefiles" `
#     -DBUILD_SHARED_LIBS=OFF `
#     -DCMAKE_FIND_LIBRARY_SUFFIXES=".a" `
#     -DCMAKE_EXE_LINKER_FLAGS="-static" `
#     -DOPTION_TARGET_MESSAGING=OFF `
#     -DOPTION_TARGET_PARALLEL=OFF `
#     -DOPTION_TARGET_CHOMBO2D_SOLVER=OFF `
#     -DOPTION_TARGET_CHOMBO3D_SOLVER=OFF `
#     -DOPTION_TARGET_SMOLDYN_SOLVER=OFF `
#     -DOPTION_TARGET_FV_SOLVER=OFF `
#     -DOPTION_TARGET_STOCHASTIC_SOLVER=OFF `
#     -DOPTION_TARGET_NFSIM_SOLVER=ON `
#     -DOPTION_TARGET_MOVINGBOUNDARY_SOLVER=OFF `
#     -DOPTION_TARGET_SUNDIALS_SOLVER=OFF `
#     -DOPTION_TARGET_HY3S_SOLVERS=OFF `
#      ..

# make

# Move-Item \projects\vcell-solvers\tmp\* \projects\vcell-solvers\build\bin
