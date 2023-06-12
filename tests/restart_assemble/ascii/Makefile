#
#  Makefile for Dynamo toolkit Calypso
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
	rm -f *.dat restart/rst.*
	$(MPIRUN) -np 3 $(BUILDDIR)/utilities/gen_sph_grids
	$(MPIRUN) -np 3 $(BUILDDIR)/utilities/assemble_sph
	$(MPIRUN) -np 3 $(BUILDDIR)/tests/compare_sph_restart

clean:
	rm -f *.dat restart/rst.* sph_lm7t12r14c_ic_3/in.* 

distclean: clean
