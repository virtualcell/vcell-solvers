FROM ubuntu:20.04 as build

RUN apt-get -y update && apt-get install -y apt-utils && \
    apt-get install -y -qq -o=Dpkg::Use-Pty=0 build-essential gfortran zlib1g-dev \
    libhdf5-dev libcurl4-openssl-dev libboost-dev cmake wget python

COPY . /vcellroot

RUN mkdir -p /vcellroot/build/bin
WORKDIR /vcellroot/build

RUN /usr/bin/cmake \
    -DOPTION_TARGET_MESSAGING=ON \
    -DOPTION_TARGET_FV_SOLVER=ON \
    .. && \
    make && \
    ctest


#RUN apt-get update && \
#    apt-get install -y apt-utils && \
#    apt-get install -q -y --no-install-recommends curl dnsutils
#
#RUN apt-get install -qq -y -o=Dpkg::Use-Pty=0 gcc gfortran zlib1g \
#    libhdf5-103 libhdf5-cpp-103 libcurl4-openssl-dev zip
#
#COPY --from=build /vcellroot/build/bin /vcellbin
#WORKDIR /vcellbin
#ENV PATH=/vcellbin:$PATH
#
