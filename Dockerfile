FROM eclipse-temurin:17-jdk-jammy as build

RUN apt-get -y update && apt-get install -y apt-utils && \
    apt-get install -y -qq -o=Dpkg::Use-Pty=0 build-essential gfortran zlib1g-dev \
    libhdf5-dev libcurl4-openssl-dev libboost-dev cmake wget python3 python3-pip && \
    apt-get install -q -y --no-install-recommends curl dnsutils

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

FROM eclipse-temurin:17-jdk-jammy as runtime

RUN apt-get -y update && apt-get install -y apt-utils && \
    apt-get install -y -qq -o=Dpkg::Use-Pty=0 build-essential gfortran zlib1g-dev \
    libhdf5-dev libcurl4-openssl-dev libboost-dev cmake wget python3 python3-pip && \
    apt-get install -q -y --no-install-recommends curl dnsutils

# Create a custom Java runtime
RUN $JAVA_HOME/bin/jlink \
         --add-modules ALL-MODULE-PATH \
         --strip-debug \
         --no-man-pages \
         --no-header-files \
         --compress=2 \
         --output /javaruntime

ENV JAVA_HOME=/javaruntime
ENV PATH="${JAVA_HOME}/bin:${PATH}"

ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=en_US.UTF-8

COPY --from=build /vcellroot/build/bin /vcellbin

WORKDIR /vcellbin
ENV PATH=/vcellbin:$PATH
