#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
SRCDIR = /Users/matsui/src_kemo
#
MHDDIR =  $(SRCDIR)/MHD
BUILDDIR= $(SRCDIR)/bin
TESTDIR=  $(SRCDIR)/tests
#
# MPI settings
#
MPICHDIR =    
MPICHLIBDIR = $(MPICHDIR)/lib
MPICHBINDIR = $(MPICHDIR)/bin
MPICHINCDIR = 
MPILIBS =      
#

all: test

test: 
	rm -f *.dat
	$(BUILDDIR)/tests/test_FFTPACK
	$(BUILDDIR)/tests/test_FFTW3_f
	$(BUILDDIR)/tests/test_single_FFTW3
	echo 'Compare section and isosurface data'
	$(BUILDDIR)/tests/compare_fft_test < compare_FFTPACK.txt
	$(BUILDDIR)/tests/compare_fft_test < compare_FFTW3.txt
	$(BUILDDIR)/tests/compare_fft_test < compare_single_FFTW3.txt

clean:
	rm -f *.dat

distclean: clean
