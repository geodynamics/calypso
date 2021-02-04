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
no_file \
\
ascii \
binary \
gzip \
binary_gz \
\
merged \
merged_bin \
merged_gz \
merged_bin_gz

#
#  -------------------------------------------------------------------------
#


all: test

test: 
	for dir in $(SUBDIRS) ; do \
	echo "# Test spherical harmonics indexining in $${dir} directory..."; \
		( cd $${dir}; \
		$(GMAKE) \
		GMAKE="$(GMAKE)" \
		BUILDDIR="$(BUILDDIR)" \
		TESTDIR="$(TESTDIR)" \
		MHDDIR="$(MHDDIR)" \
		MPIRUN="$(MPIRUN)" \
		test)\
	done; \
	echo "# Test Communication without indexing file"; \
	$(MPIRUN) -np 6 $(BUILDDIR)/tests/check_sph_comms
	
	rm -f *.dat
	rm -f mpif.h *.o *.mod *~ *.par *.diag *.a *.f90

clean:
	for dir in $(SUBDIRS) ; do \
	echo "# cleaning $${dir} directory..."; \
		( cd $${dir}; \
		make clean )\
	done; \
	rm -f reference/in.*
	rm -f mpif.h *.o *.mod *~ *.par *.diag *.a *.f90

distclean: clean
