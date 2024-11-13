FROM python:3.9.7-slim AS build

RUN apt-get -y update && apt-get install -y apt-utils && \
    apt-get install -y -qq -o=Dpkg::Use-Pty=0 build-essential gfortran zlib1g-dev \
    libhdf5-dev libcurl4-openssl-dev libboost-dev cmake wget python3 python3-pip

COPY . /vcellroot

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
    ctest -VV

FROM eclipse-temurin:17 AS jre-build

# Create a custom Java runtime
RUN $JAVA_HOME/bin/jlink \
         --add-modules ALL-MODULE-PATH \
         --strip-debug \
         --no-man-pages \
         --no-header-files \
         --compress=2 \
         --output /javaruntime

# Define base image and copy in jlink created minimal Java 17 environment
FROM python:3.9.7-slim
ENV JAVA_HOME=/opt/java/openjdk
ENV PATH="${JAVA_HOME}/bin:${PATH}"
COPY --from=jre-build /javaruntime $JAVA_HOME

# now we have Java 17 and Python 3.9
ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=en_US.UTF-8

RUN apt-get -y update && \
    apt-get install -y apt-utils && \
    apt-get install -q -y --no-install-recommends curl dnsutils

RUN apt-get install -y -qq -o=Dpkg::Use-Pty=0 gcc gfortran zlib1g \
    libhdf5-103 libhdf5-cpp-103 libcurl4-openssl-dev zip

COPY --from=build /vcellroot/build/bin /vcellbin

WORKDIR /vcellbin
ENV PATH=/vcellbin:$PATH
