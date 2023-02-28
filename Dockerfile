FROM ubuntu:20.04 as build

RUN apt-get -y update && apt-get install -y apt-utils && \
    apt-get install -y -qq -o=Dpkg::Use-Pty=0 build-essential gfortran zlib1g-dev \
    libhdf5-dev libcurl4-openssl-dev libboost-dev cmake wget python

#
# build PETSc with mpich for static linking
#
RUN mkdir /usr/local/petsc && \
    cd /usr/local/petsc && \
    wget http://ftp.mcs.anl.gov/pub/petsc/release-snapshots/petsc-3.7.7.tar.gz && \
    tar xzf petsc-3.7.7.tar.gz && \
    cd petsc-3.7.7 && \
    ./configure --with-shared-libraries=0 --download-fblaslapack=1 --with-debugging=1 --download-mpich && \
    make PETSC_DIR=/usr/local/petsc/petsc-3.7.7 PETSC_ARCH=arch-linux2-c-debug all

ENV PETSC_DIR=/usr/local/petsc/petsc-3.7.7 \
    PETSC_ARCH=arch-linux2-c-debug

COPY . /vcellroot

#
# build most solvers, and FiniteVolume without PETSc (FiniteVolume_x64)
#
RUN mkdir -p /vcellroot/build/bin
WORKDIR /vcellroot/build

RUN /usr/bin/cmake \
    -DOPTION_TARGET_MESSAGING=ON \
    -DOPTION_TARGET_PARALLEL=OFF \
    -DOPTION_TARGET_PETSC=OFF \
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
    make && \
    ctest

#
# build FiniteVolume with PETSc (FiniteVolume_PETSc_x64)
#
RUN mkdir -p /vcellroot/build_PETSc/bin
WORKDIR /vcellroot/build_PETSc

RUN /usr/bin/cmake \
    -DOPTION_TARGET_MESSAGING=ON \
    -DOPTION_TARGET_PARALLEL=OFF \
    -DOPTION_TARGET_PETSC=ON \
    -DOPTION_TARGET_CHOMBO2D_SOLVER=OFF \
    -DOPTION_TARGET_CHOMBO3D_SOLVER=OFF \
    -DOPTION_TARGET_SMOLDYN_SOLVER=OFF \
    -DOPTION_TARGET_FV_SOLVER=ON \
    -DOPTION_TARGET_STOCHASTIC_SOLVER=OFF \
    -DOPTION_TARGET_NFSIM_SOLVER=OFF \
    -DOPTION_TARGET_MOVINGBOUNDARY_SOLVER=OFF \
    -DOPTION_TARGET_SUNDIALS_SOLVER=OFF \
    -DOPTION_TARGET_HY3S_SOLVERS=OFF \
    .. && \
    make && \
    ctest


FROM eclipse-temurin:17 as jre-build

# Create a custom Java runtime
RUN $JAVA_HOME/bin/jlink \
         --add-modules java.base,java.instrument,java.logging,java.management,java.desktop,java.naming,java.xml \
         --strip-debug \
         --no-man-pages \
         --no-header-files \
         --compress=2 \
         --output /javaruntime

# Define base image and copy in jlink created minimal Java 17 environment
FROM python:3.9.7-slim
ENV JAVA_HOME=/opt/java/openjdk
ENV PATH "${JAVA_HOME}/bin:${PATH}"
COPY --from=jre-build /javaruntime $JAVA_HOME

# now we have Java 17 and Python 3.9
ENV DEBIAN_FRONTEND noninteractive
ENV LANG=en_US.UTF-8

RUN apt-get update && \
    apt-get install -y apt-utils && \
    apt-get install -q -y --no-install-recommends curl dnsutils

RUN apt-get install -qq -y -o=Dpkg::Use-Pty=0 gcc gfortran zlib1g \
    libhdf5-103 libhdf5-cpp-103 libcurl4-openssl-dev zip

COPY --from=build /vcellroot/build/bin /vcellbin
COPY --from=build /vcellroot/build_PETSc/bin/FiniteVolume_PETSc_x64 /vcellbin/
WORKDIR /vcellbin
ENV PATH=/vcellbin:$PATH

