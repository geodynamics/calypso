FROM ubuntu:focal

RUN apt update && \
  DEBIAN_FRONTEND='noninteractive' \
  DEBCONF_NONINTERACTIVE_SEEN='true' \
  apt install --yes \
  build-essential \
  doxygen \
  gfortran \
  git \
  zlib1g \
  zlib1g-dev \
  libblas-dev \
  libfftw3-dev \
  libhdf5-openmpi-dev \
  libopenmpi-dev \
  openmpi-bin \
  openmpi-common \
  pkg-config
