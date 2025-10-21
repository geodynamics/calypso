#
# Makefile header for Calypso Dynamo example
#
#
SHELL = /bin/sh
#
#  all:         compile programs for simulations
#
# Compiler commands
#
CC=gcc
F90=mpif90
AR=ar
RANLIB=ranlib
#
# Compiler options
#
F90OPTFLAGS = -O3 -g -fopenmp  $(F_INCLUDE)
F90CPPFLAGS =  -DZLIB_IO -DFFTW3 -DBLAS -DHDF5_IO
OPTFLAGS = -O3 -g  
#
# Libralies and includes settting
#

F90LIBS =   $(ZLIB_LIBS) $(FFTW3_LIBS) $(BLAS_LIBS) $(HDF5_LDFLAGS) $(HDF5_FLIBS)
F_INCLUDE = -I. $(HDF5_FFLAGS)
C_INCLUDE = -I. $(ZLIB_CFLAGS)  $(FFTW3_CFLAGS)

#
# zlib settings
#
ZLIB_CFLAGS = 
ZLIB_LIBS =   -lz
#
# BLAS settings
#
BLAS_LIBS =   -lblas
#
# FFTW3 settings
#
FFTW3_CFLAGS =   -I/opt/homebrew/Cellar/fftw/3.3.10_2/include
FFTW3_LIBS =     -L/opt/homebrew/Cellar/fftw/3.3.10_2/lib -lfftw3
FFTW3_OMP_LIBS = 
#
# HDF5 settings
#
HDF5_FFLAGS = -I/opt/homebrew/lib -L/opt/homebrew/lib -I/opt/homebrew/include 
HDF5_LDFLAGS = -L/opt/homebrew/lib 
HDF5_FLIBS = -L/opt/homebrew/lib -lhdf5_fortran -lhdf5
#
# -----------------------------------------------------------
# -----------------------------------------------------------
#
# Directory lists
#
SRCDIR = /Users/matsui/Calypso_test
#
MAKEDIR = /Users/matsui/Calypso_test/work
BUILDDIR = /Users/matsui/Calypso_test/bin
MHDDIR = /Users/matsui/Calypso_test/src
INSTDIR = /usr/local
EXPDIR = /Users/matsui/Calypso_test/examples
TESTDIR = /Users/matsui/Calypso_test/tests
#
