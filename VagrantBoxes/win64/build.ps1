$Env:Path = "c:\tools\msys64\mingw64\bin" + ";" + "c:\tools\msys64\usr\bin" + ";" + $Env:Path
Write-Host "cd to \vagrant_numerics"
cd \vagrant_numerics
Write-Host "mkdir build-mingw64"
mkdir build-mingw64
Write-Host "cd build-mingw64"
cd build-mingw64
Write-Host "mkdir bin"
mkdir bin
Write-Host "cmake.exe ... "


#       --debug-output `
#	--graphvis=test.dot `
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

Write-Host "install standard mingw dlls"
copy c:\tools\msys64\mingw64\bin\*.dll bin

Write-Host "make and install SundialsSolverStandalone_x64.exe"
#make SundialsSolverStandalone_x64
copy .\IDAWin\SundialsSolverStandalone_x64.exe bin

Write-Host "make and install VCellStoch_x64.exe"
#make VCellStoch_x64
copy .\Stochastic\VCellStoch_x64.exe bin

Write-Host "make and install NFsim_x64.exe"
#make NFsim_x64
copy .\NFsim_v1.11\NFcode\NFsim_x64.exe bin

Write-Host "make and install FiniteVolume_x64.exe"
#make FiniteVolume_x64
copy .\VCell\FiniteVolume_x64.exe bin

Write-Host "make and install Smoldyn_x64.exe"
#make smoldyn_x64
copy .\bridgeVCellSmoldyn\smoldyn_x64.exe bin

Write-Host "make and install MovingBoundary_x64.exe"
#make MovingBoundary_x64
copy .\MovingBoundarySolver\MovingBoundary_x64.exe bin
