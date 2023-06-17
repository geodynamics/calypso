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

test: clean
	$(MPIRUN) -np 6 $(BUILDDIR)/sph_mhd

clean:
	rm -f *.dat *.txt
	rm -f sections/*.gz
	rm -f isosurfaces/*.gz
	rm -f */*.png
	rm -f */*.dat
	rm -f */*.dat.gz

distclean: clean
