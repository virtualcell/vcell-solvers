#!/usr/bin/bash

export PATH=/mingw64/bin:/usr/bin/:$PATH
cd /c/projects/vcell-solvers/build/bin
ldd FiniteVolume_x64.exe
ldd NFsim_x64.exe
ldd smoldyn_x64.exe
ldd SundialsSolverStandalone_x64.exe
ldd testzip.exe
ldd VCellStoch_x64.exe
ldd ziptool.exe
ls *.exe | awk '{print $1}' | xargs -I '{}' ldd '{}' | grep '=> /' | grep -v build | grep -iv windows | awk '{print $3}' | xargs -I '{}' cp -vn '{}' . || true
ls *.dll | awk '{print $1}' | xargs -I '{}' ldd '{}' | grep '=> /' | grep -v build | grep -iv windows | awk '{print $3}' | xargs -I '{}' cp -vn '{}' . || true
ls *.dll | awk '{print $1}' | xargs -I '{}' ldd '{}' | grep '=> /' | grep -v build | grep -iv windows | awk '{print $3}' | xargs -I '{}' cp -vn '{}' . || true
