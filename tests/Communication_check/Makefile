#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
SRCDIR = /Users/matsui/src_kemo
BUILDDIR= $(SRCDIR)/bin
#
GMAKE = make

TARGET_GEN_SPH_GRID =    $(BUILDDIR)/utilities/gen_sph_grids
TARGET_CHECK_SPH_GRID =  $(BUILDDIR)/tests/check_sph_grids
TARGET_TEST_SPH_GRID =   $(BUILDDIR)/tests/check_sph_comms
#
NP = 6
MPIRUN= mpirun
MPI_GEN =   $(MPIRUN) -np $(NP) $(TARGET_GEN_SPH_GRID)
MPI_CHECK = $(MPIRUN) -np $(NP) $(TARGET_CHECK_SPH_GRID)
MPI_TEST =  $(MPIRUN) -np $(NP) $(TARGET_TEST_SPH_GRID)
#
#
#  -------------------------------------------------------------------------
#

all: test

test: 
	@echo '#'
	@echo '# -------------------------------------------------------------'
	@echo "#   Test spherical harmonics indexining in ascii directory..."
	@echo '# -------------------------------------------------------------'
	@echo '#'
	@cd ascii; \
		$(GMAKE) MPI_GEN="$(MPI_GEN)" MPI_CHECK="$(MPI_CHECK)" MPI_TEST="$(MPI_TEST)" \
		test
	
	@echo '#'
	@echo '# --------------------------------------------------------------'
	@echo "#   Test spherical harmonics indexining in binary directory..."
	@echo '# --------------------------------------------------------------'
	@echo '#'
	@cd binary; \
		$(GMAKE) MPI_GEN="$(MPI_GEN)" MPI_CHECK="$(MPI_CHECK)" MPI_TEST="$(MPI_TEST)" \
		test
	
	@echo '#'
	@echo '# -------------------------------------------------------------'
	@echo "#   Test spherical harmonics indexining in gzip directory..."
	@echo '# -------------------------------------------------------------'
	@echo '#'
	@cd gzip; \
		$(GMAKE) MPI_GEN="$(MPI_GEN)" MPI_CHECK="$(MPI_CHECK)" MPI_TEST="$(MPI_TEST)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------------'
	@echo "#   Test spherical harmonics indexining in binary_gz directory..."
	@echo '# -----------------------------------------------------------------'
	@echo '#'
	@cd binary_gz; \
		$(GMAKE) MPI_GEN="$(MPI_GEN)" MPI_CHECK="$(MPI_CHECK)" MPI_TEST="$(MPI_TEST)" \
		test
	
	@echo '#'
	@echo '# --------------------------------------------------------------'
	@echo "#   Test spherical harmonics indexining in merged directory..."
	@echo '# --------------------------------------------------------------'
	@echo '#'
	@cd merged; \
		$(GMAKE) MPI_GEN="$(MPI_GEN)" MPI_CHECK="$(MPI_CHECK)" MPI_TEST="$(MPI_TEST)" \
		test
	
	@echo '#'
	@echo '# ------------------------------------------------------------------'
	@echo "#   Test spherical harmonics indexining in merged_bin directory..."
	@echo '# ------------------------------------------------------------------'
	@echo '#'
	@cd merged_bin; \
		$(GMAKE) MPI_GEN="$(MPI_GEN)" MPI_CHECK="$(MPI_CHECK)" MPI_TEST="$(MPI_TEST)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------------'
	@echo "#   Test spherical harmonics indexining in merged_gz directory..."
	@echo '# -----------------------------------------------------------------'
	@echo '#'
	@cd merged_gz; \
		$(GMAKE) MPI_GEN="$(MPI_GEN)" MPI_CHECK="$(MPI_CHECK)" MPI_TEST="$(MPI_TEST)" \
		test
	
	@echo '#'
	@echo '# ---------------------------------------------------------------------'
	@echo "#   Test spherical harmonics indexining in merged_bin_gz directory..."
	@echo '# ---------------------------------------------------------------------'
	@echo '#'
	@cd merged_bin_gz; \
		$(GMAKE) MPI_GEN="$(MPI_GEN)" MPI_CHECK="$(MPI_CHECK)" MPI_TEST="$(MPI_TEST)" \
		test
	
	@echo '#'
	@echo '# ---------------------------------------------------------------'
	@echo "#   Test spherical harmonics indexining in no_file directory..."
	@echo '# ---------------------------------------------------------------'
	@echo '#'
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
