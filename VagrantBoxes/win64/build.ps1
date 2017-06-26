$Env:Path = "c:\tools\msys64\mingw64\bin" + ";" + "c:\tools\msys64\usr\bin" + ";" + $Env:Path
Write-Host "cd to \vagrant_numerics"
cd \vagrant_numerics
Write-Host "mkdir build-win64"
mkdir build-win64
Write-Host "cd build-win64"
cd build-win64
Write-Host "mkdir bin"
mkdir bin
Write-Host "cmake.exe ... "

cmake `
	-DUNVERSIONED="True" `
	-G "Unix Makefiles" `
	-DCMAKE_BUILD_TYPE="Release" `
	-DOPTION_TARGET_MESSAGING=OFF `
	-DOPTION_TARGET_PARALLEL=OFF `
	-DOPTION_TARGET_CHOMBO2D_SOLVER=OFF `
	-DOPTION_TARGET_CHOMBO3D_SOLVER=OFF `
	-DOPTION_TARGET_SMOLDYN_SOLVER=ON `
	-DOPTION_TARGET_FV_SOLVER=ON `
	-DOPTION_TARGET_STOCHASTIC_SOLVER=ON `
	-DOPTION_TARGET_NFSIM_SOLVER=ON `
	-DOPTION_TARGET_MOVINGBOUNDARY_SOLVER=ON `
	-DOPTION_TARGET_SUNDIALS_SOLVER=ON `
	-DOPTION_TARGET_HY3S_SOLVERS=OFF `
	..

make

#Write-Host "install standard mingw dlls"
#copy c:\tools\msys64\mingw64\bin\*.dll bin
