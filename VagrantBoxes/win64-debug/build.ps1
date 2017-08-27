$Env:Path = "c:\tools\msys64\mingw64\bin" + ";" + "c:\tools\msys64\usr\bin" + ";" + $Env:Path
Write-Host "cd to \vagrant_numerics_parent"
cd \vagrant_numerics_parent
Write-Host "mkdir build-win64-debug"
mkdir build-win64-debug
Write-Host "cd build-win64-debug"
cd build-win64-debug
Write-Host "mkdir bin"
mkdir bin
Write-Host "cmake.exe ... "

cmake `
	-G "Eclipse CDT4 - Unix Makefiles" `
	-DCMAKE_BUILD_TYPE="Debug" `
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
	..\Numerics

#make

#Write-Host "install standard mingw dlls"
#copy c:\tools\msys64\mingw64\bin\*.dll bin
