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
NP = 6
MPIRUN= mpirun
MPI_GEN =   $(MPIRUN) -np $(NP) $(SRCDIR)/bin/utilities/gen_sph_grids
MPI_CHECK = $(MPIRUN) -np $(NP) $(SRCDIR)/bin/tests/check_sph_grids
MPI_TEST =  $(MPIRUN) -np $(NP) $(SRCDIR)/bin/tests/check_sph_comms
#
#
#  -------------------------------------------------------------------------
#

all: test

test: 
	@echo "# Test spherical harmonics indexining in ascii directory..."
	@cd ascii; \
		$(GMAKE) MPI_GEN="$(MPI_GEN)" MPI_CHECK="$(MPI_CHECK)" MPI_TEST="$(MPI_TEST)" \
		test
	
	@echo "# Test spherical harmonics indexining in binary directory..."
	@cd binary; \
		$(GMAKE) MPI_GEN="$(MPI_GEN)" MPI_CHECK="$(MPI_CHECK)" MPI_TEST="$(MPI_TEST)" \
		test
	
	@echo "# Test spherical harmonics indexining in gzip directory..."
	@cd gzip; \
		$(GMAKE) MPI_GEN="$(MPI_GEN)" MPI_CHECK="$(MPI_CHECK)" MPI_TEST="$(MPI_TEST)" \
		test
	
	@echo "# Test spherical harmonics indexining in binary_gz directory..."
	@cd binary_gz; \
		$(GMAKE) MPI_GEN="$(MPI_GEN)" MPI_CHECK="$(MPI_CHECK)" MPI_TEST="$(MPI_TEST)" \
		test
	
	@echo "# Test spherical harmonics indexining in merged directory..."
	@cd merged; \
		$(GMAKE) MPI_GEN="$(MPI_GEN)" MPI_CHECK="$(MPI_CHECK)" MPI_TEST="$(MPI_TEST)" \
		test
	
	@echo "# Test spherical harmonics indexining in merged_bin directory..."
	@cd merged_bin; \
		$(GMAKE) MPI_GEN="$(MPI_GEN)" MPI_CHECK="$(MPI_CHECK)" MPI_TEST="$(MPI_TEST)" \
		test
	
	@echo "# Test spherical harmonics indexining in merged_gz directory..."
	@cd merged_gz; \
		$(GMAKE) MPI_GEN="$(MPI_GEN)" MPI_CHECK="$(MPI_CHECK)" MPI_TEST="$(MPI_TEST)" \
		test
	
	@echo "# Test spherical harmonics indexining in merged_bin_gz directory..."
	@cd merged_bin_gz; \
		$(GMAKE) MPI_GEN="$(MPI_GEN)" MPI_CHECK="$(MPI_CHECK)" MPI_TEST="$(MPI_TEST)" \
		test
	
	@echo "# Test spherical harmonics indexining in no_file directory..."
	@cd no_file; \
		$(GMAKE) MPI_TEST="$(MPI_TEST)" \
		test
	
	@rm -f *.dat *.txt

clean:
	cd ascii; make clean
	cd binary; make clean
	cd gzip; make clean
	cd binary_gz; make clean
	
	cd merged; make clean
	cd merged_bin; make clean
	cd merged_gz; make clean
	cd merged_bin_gz; make clean
	
	cd no_file; make clean
	
	rm -f reference/in.*
	rm -f mpif.h *.o *.mod *~ *.par *.diag *.a *.f90

distclean: clean
