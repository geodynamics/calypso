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
	rm -f *.dat
	$(MPIRUN) -np 6 $(BUILDDIR)/assemble_sph
	$(MPIRUN) -np 6 $(BUILDDIR)/tests/compare_sph_restart

clean:
	rm -f *.dat restart/rst.*

distclean: clean
