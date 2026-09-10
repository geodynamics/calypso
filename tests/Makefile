#
#  Makefile for Kemo's Dynamo toolkit
#    Written by H. Matsui
#
SHELL           = /bin/sh
#
#  directories of Kemo's Dynamo toolkit
#
SRCDIR = /Users/matsui/src_kemo
MAKEDIR = $(SRCDIR)/work
BUILDDIR= $(SRCDIR)/bin
#
MHDDIR =  $(SRCDIR)/MHD
BUILDDIR= $(SRCDIR)/bin
TESTDIR=  $(SRCDIR)/tests
#
GMAKE=   make
MPIRUN= mpirun

TARGET_SPH_MHD =             $(BUILDDIR)/sph_mhd
TARGET_SPH_SNAP =            $(BUILDDIR)/sph_snapshot
TARGET_PSF_2_VTK =           $(BUILDDIR)/utilities/section_to_VTK
#
TARGET_GEN_SPH_GRID =        $(BUILDDIR)/utilities/gen_sph_grids
TARGET_NEWSPH=               $(BUILDDIR)/utilities/assemble_sph
TARGET_CONTROL_MHD_CHECK =   $(BUILDDIR)/utilities/check_control_mhd

#
#  -------------------------------------------------------------------------
#


all: test

test:
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Test in Gauss_point_test directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	cd Gauss_point_test; \
		$(GMAKE) \
		SRCDIR="$(SRCDIR)" \
		MAKEDIR="$(MAKEDIR)" \
		GMAKE="$(GMAKE)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Test in FFT_test directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	cd FFT_test; \
		$(GMAKE) \
		SRCDIR="$(SRCDIR)" \
		MAKEDIR="$(MAKEDIR)" \
		GMAKE="$(GMAKE)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Tests in Communication_check directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	cd Communication_check; \
		$(GMAKE) \
		SRCDIR="$(SRCDIR)" \
		MAKEDIR="$(MAKEDIR)" \
		GMAKE="$(GMAKE)" \
		MPIRUN="$(MPIRUN)"  \
		test
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Tests in restart_assemble directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	cd restart_assemble; \
		$(GMAKE) \
		BUILDDIR="$(BUILDDIR)" \
		GMAKE="$(GMAKE)" \
		MPIRUN="$(MPIRUN)"  \
		TARGET_GEN_SPH_GRID="$(TARGET_GEN_SPH_GRID)" \
		TARGET_NEWSPH="$(TARGET_NEWSPH)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Test in Control_file_check directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	cd Control_file_check; \
		$(GMAKE) \
		BUILDDIR="$(BUILDDIR)" \
		GMAKE="$(GMAKE)" \
		TARGET_CONTROL_MHD_CHECK="$(TARGET_CONTROL_MHD_CHECK)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Tests in Dynamo_Benchmark directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	cd Dynamo_Benchmark; \
		$(GMAKE) \
		GMAKE="$(GMAKE)" \
		MPIRUN="$(MPIRUN)"  \
		SRCDIR="$(SRCDIR)" \
		BUILDDIR="$(BUILDDIR)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		TARGET_SPH_SNAP="$(TARGET_SPH_SNAP)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Tests in Models directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	cd Models; \
		$(GMAKE) \
		GMAKE="$(GMAKE)" \
		MPIRUN="$(MPIRUN)"  \
		SRCDIR="$(SRCDIR)" \
		BUILDDIR="$(BUILDDIR)" \
		MAKEDIR="$(MAKEDIR)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		TARGET_SPH_SNAP="$(TARGET_SPH_SNAP)" \
		TARGET_PSF_2_VTK="$(TARGET_PSF_2_VTK)" \
		test
	
	@echo "# --------- Calypso tests finished! -------------------------"

clean:
	cd Gauss_point_test;    make clean
	cd FFT_test;            make clean
	cd Communication_check; make clean
	cd restart_assemble;    make clean
	cd Control_file_check;  make clean
	cd Dynamo_Benchmark;    make clean
	cd Models;              make clean
	
	rm -f mpif.h *.o *.mod *~ *.par *.diag *.a *.f90

distclean: clean
