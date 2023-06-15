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
	$(MPIRUN) -np 3 $(BUILDDIR)/utilities/assemble_sph > log.txt
	$(MPIRUN) -np 3 $(BUILDDIR)/tests/compare_sph_restart >> log.txt

clean:
	rm -f *.dat *.txt rst.*

distclean: clean
