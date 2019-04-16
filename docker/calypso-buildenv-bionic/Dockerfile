FROM ubuntu:bionic

RUN apt update && \
  DEBIAN_FRONTEND='noninteractive' \
  DEBCONF_NONINTERACTIVE_SEEN='true' \
  apt install --yes \
  build-essential \
  cmake \
  doxygen \
  fftw-dev \
  git \
  libblas-dev \
  libhdf5-openmpi-dev \
  libopenmpi-dev \
  ninja-build \
  openmpi-bin \
  openmpi-common
