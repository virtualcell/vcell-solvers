### Debugging VCell C++ solvers with Eclipse CDT

assumes that there is a top-level directory (e.g. workspace/) where vcell-solvers is installed.  This configuration uses out-of-source builds where source (workspace-dir/vcell-solvers/) and build (workspace-dir/vcell-solvers-build/) are peers.  This seems to make Eclipse CDT builds easlier.

```
workspace-dir/
   vcell-solvers/
       docker/
          create.sh
          destroy.sh
          run_bash.sh
          run_eclipse.sh
       CMakeList.txt
   vcell-solvers-build/
```

### to configure for FiniteVolume_x64

create if necessary and start a new named Docker container (name is vcell-solvers-ide).  All subsequent commands will be run within this running container.  To restart the container, ./create.sh may be called again.  Note that some files may be persisted within the container.  To start with a fresh container, run ./destroy.sh first.

```bash
cd /path/to/vcell-solvers/docker

./create.sh
```

inside container shell, cd to /workspace/vcell-solvers-build and run cmake (in this case configured to build FiniteVolume_x64 with -DOPTION_TARGET_FV_SOLVER=ON).  Then exit to run eclipse.

```bash
./run_bash.sh

docker> mkdir -p /workspace/vcell-solvers-build
docker> cd /workspace/vcell-solvers-build

docker> cmake \
    -G "Eclipse CDT4 - Unix Makefiles" \
    -DCMAKE_BUILD_TYPE="Debug" \
    -DCMAKE_ECLIPSE_GENERATE_SOURCE_PROJECT=TRUE \
    -DOPTION_TARGET_MESSAGING=ON \
    -DOPTION_TARGET_PARALLEL=OFF \
    -DOPTION_TARGET_CHOMBO2D_SOLVER=OFF \
    -DOPTION_TARGET_CHOMBO3D_SOLVER=OFF \
    -DOPTION_TARGET_SMOLDYN_SOLVER=OFF \
    -DOPTION_TARGET_FV_SOLVER=ON \
    -DOPTION_TARGET_STOCHASTIC_SOLVER=OFF \
    -DOPTION_TARGET_NFSIM_SOLVER=OFF \
    -DOPTION_TARGET_MOVINGBOUNDARY_SOLVER=OFF \
    -DOPTION_TARGET_SUNDIALS_SOLVER=OFF \
    -DOPTION_TARGET_HY3S_SOLVERS=OFF \
    ../vcell-solvers

docker> exit
```

to run eclipse.  Note: to enable X11 display from a Docker container on a Mac, socat is used to bridge the communication.  Only one such process should be running at a time and is automatically started within the ./create.sh script.  This can be installed on Macos using Homebrew 'brew install socat'.

```
./run_eclipse.sh
```
