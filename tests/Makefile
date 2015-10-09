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
	$(MPIRUN) -np 4 $(BUILDDIR)/gen_sph_grids
	$(MPIRUN) -np 4 $(BUILDDIR)/sph_mhd
	echo 'Compare with reference mean square data'
	diff sph_pwr_volume.dat reference/sph_pwr_volume.dat

clean:
	rm -r *.dat rst_4/*  sph_lm31r48c_4/*

distclean: clean
