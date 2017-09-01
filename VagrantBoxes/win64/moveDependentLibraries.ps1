Write-Host "install necessary mingw dlls into \vagrant_numerics\build-win32\bin"
cd \vagrant_numerics\build-win64\bin
copy c:\tools\msys64\mingw64\bin\libhdf5-0.dll .
copy c:\tools\msys64\mingw64\bin\libhdf5_cpp-0.dll .
copy c:\tools\msys64\mingw64\bin\libgfortran-4.dll .
copy c:\tools\msys64\mingw64\bin\libstdc++-6.dll .
copy c:\tools\msys64\mingw64\bin\libszip.dll .
copy c:\tools\msys64\mingw64\bin\libgcc_s_seh-1.dll .
copy c:\tools\msys64\mingw64\bin\libwinpthread-1.dll .
copy c:\tools\msys64\mingw64\bin\libquadmath-0.dll .

