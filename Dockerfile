FROM ubuntu:xenial as build

RUN apt-get -y update && apt-get install -y apt-utils && \
    apt-get install -y -qq -o=Dpkg::Use-Pty=0 build-essential gfortran zlib1g-dev \
    libhdf5-dev libcurl4-openssl-dev libboost-dev cmake

COPY . /vcellroot

RUN mkdir build && mkdir build/bin
WORKDIR /vcellroot/build

RUN /usr/bin/cmake \
    -DOPTION_TARGET_MESSAGING=ON \
    -DOPTION_TARGET_PARALLEL=OFF \
    -DOPTION_TARGET_CHOMBO2D_SOLVER=OFF \
    -DOPTION_TARGET_CHOMBO3D_SOLVER=OFF \
    -DOPTION_TARGET_SMOLDYN_SOLVER=ON \
    -DOPTION_TARGET_FV_SOLVER=ON \
    -DOPTION_TARGET_STOCHASTIC_SOLVER=ON \
    -DOPTION_TARGET_NFSIM_SOLVER=ON \
    -DOPTION_TARGET_MOVINGBOUNDARY_SOLVER=ON \
    -DOPTION_TARGET_SUNDIALS_SOLVER=ON \
    -DOPTION_TARGET_HY3S_SOLVERS=OFF \
    .. && \
    make

FROM ubuntu:xenial

RUN apt-get -y update && apt-get install -y apt-utils && \
    apt-get install -y -qq -o=Dpkg::Use-Pty=0 gcc gfortran zlib1g \
    libhdf5-10 libhdf5-cpp-11 libcurl4-openssl-dev 

COPY --from=build /vcellroot/build/bin /vcellbin
WORKDIR /vcellbin
ENV PATH=/vcellbin:$PATH
