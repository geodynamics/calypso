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
#
#  -------------------------------------------------------------------------
#
SUBDIRS = \
Dynamobench_case1 \
Dynamobench_case2 \
heterogineous_temp

#
#  -------------------------------------------------------------------------
#


all: test

test: 
	for dir in $(SUBDIRS) ; do \
	echo "# Test in $${dir} directory..."; \
		( cd $${dir}; \
		$(GMAKE) \
		GMAKE="$(GMAKE)" \
		BUILDDIR="$(BUILDDIR)" \
		TESTDIR="$(TESTDIR)" \
		MHDDIR="$(MHDDIR)" \
		MPIRUN="$(MPIRUN)" \
		test)\
	done; \
	rm -f mpif.h *.o *.mod *~ *.par *.diag *.a *.f90

clean:
	for dir in $(SUBDIRS) ; do \
	echo "# cleaning $${dir} directory..."; \
		( cd $${dir}; \
		make clean )\
	done; \
	rm -f mpif.h *.o *.mod *~ *.par *.diag *.a *.f90

distclean: clean
