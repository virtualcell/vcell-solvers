## Requirements

    - git 
    - cmake
    - CLION

## For configuring the project

    1. Open CLION go to `File > settings > Build, Execution, Deployement > Toolchains > +`  
        Let it detect everything automatically `CMake`, `Make`, `C Complier`, `C++ Compiler` and `Debugger`
    2. Configure CMake `File > settings > Build, Execution, Deployement > CMake > +`
        - Build type as `Default`
        - Give `CMake options`
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
        - Generation path `build/bin`
        - Apply all these changes, that builds every solver in `bin` directory.
