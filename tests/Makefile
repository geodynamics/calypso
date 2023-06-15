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
Gauss_point_test \
FFT_test \
\
Communication_check \
restart_assemble \
\
Dynamobench_case1 \
Dynamobench_case2 \
Dynamobench_case2_cont \
snapshot_test \
heterogineous_temp

#
#  -------------------------------------------------------------------------
#


all: test

test: 
	@for dir in $(SUBDIRS) ; do \
	echo "# Test in $${dir} directory..."; \
		( cd $${dir}; \
		$(GMAKE) \
		GMAKE="$(GMAKE)" \
		BUILDDIR="$(BUILDDIR)" \
		TESTDIR="$(TESTDIR)" \
		MHDDIR="$(MHDDIR)" \
		MPIRUN="$(MPIRUN)" \
		TARGET_GEN_SPH_GRID="$(TARGET_GEN_SPH_GRID)" \
		TARGET_CHECK_SPH_GRID="$(TARGET_CHECK_SPH_GRID)" \
		TARGET_TEST_SPH_GRID="$(TARGET_TEST_SPH_GRID)" \
		TARGET_NEWSPH="$(TARGET_NEWSPH)" \
		TARGET_CMP_SPH_RST="$(TARGET_CMP_SPH_RST)" \
		test)\
	done; \
	rm -f mpif.h *.o *.mod *~ *.par *.diag *.a *.f90

sym_test: 
	for dir in sym_asym_field_test ; do \
	echo "# Test in $${dir} directory..."; \
		( cd $${dir}; \
		$(GMAKE) \
		GMAKE="$(GMAKE)" \
		BUILDDIR="$(BUILDDIR)" \
		TESTDIR="$(TESTDIR)" \
		MHDDIR="$(MHDDIR)" \
		MPIRUN="$(MPIRUN)" \
		TARGET_GEN_SPH_GRID="$(TARGET_GEN_SPH_GRID)" \
		TARGET_CHECK_SPH_GRID="$(TARGET_CHECK_SPH_GRID)" \
		TARGET_TEST_SPH_GRID="$(TARGET_TEST_SPH_GRID)" \
		TARGET_NEWSPH="$(TARGET_NEWSPH)" \
		TARGET_CMP_SPH_RST="$(TARGET_CMP_SPH_RST)" \
		test)\
	done; \
	rm -f mpif.h *.o *.mod *~ *.par *.diag *.a *.f90

clean:
	for dir in $(SUBDIRS) sym_asym_field_test ; do \
	echo "# cleaning $${dir} directory..."; \
		( cd $${dir}; \
		make clean )\
	done; \
	rm -f mpif.h *.o *.mod *~ *.par *.diag *.a *.f90

distclean: clean
