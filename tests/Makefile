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
GMAKE=   make
MPIRUN= mpirun

TARGET_SPH_MHD =             $(BUILDDIR)/sph_mhd
TARGET_SPH_SNAP =            $(BUILDDIR)/sph_snapshot
TARGET_PSF_2_VTK =           $(BUILDDIR)/utilities/section_to_VTK
TARGET_COMPARE_PSF =         $(BUILDDIR)/tests/compare_psf
TARGET_SPH_ENE_CHECK =       $(BUILDDIR)/tests/sph_ene_check
TARGET_SPH_PICK_MODE_CHECK = $(BUILDDIR)/tests/sph_picked_mode_check
TARGET_SPH_REF_CHECK =       $(BUILDDIR)/tests/sph_ref_field_check
#
TARGET_GAUSS_TEST =          $(BUILDDIR)/tests/gauss_points
TARGET_CMP_GAUSS_TEST =      $(BUILDDIR)/tests/compare_gauss_points
#
TARGET_FFTPACK_TEST =        $(BUILDDIR)/tests/test_FFTPACK
TARGET_FFW3_F_TEST =         $(BUILDDIR)/tests/test_FFTW3_f
TARGET_SGL_FFW3_TEST =       $(BUILDDIR)/tests/test_single_FFTW3
TARGET_CMPR_FFT_TEST =       $(BUILDDIR)/tests/compare_fft_test

TARGET_GEN_SPH_GRID =        $(BUILDDIR)/utilities/gen_sph_grids
TARGET_CHECK_SPH_GRID =      $(BUILDDIR)/tests/check_sph_grids
TARGET_TEST_SPH_GRID =       $(BUILDDIR)/tests/check_sph_comms

TARGET_NEWSPH=               $(BUILDDIR)/utilities/assemble_sph
TARGET_CMP_SPH_RST=          $(BUILDDIR)/tests/compare_sph_restart

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
	@cd Gauss_point_test; \
		$(GMAKE) BUILDDIR="$(BUILDDIR)" \
		TARGET_GAUSS_TEST="$(TARGET_GAUSS_TEST)" TARGET_CMP_GAUSS_TEST="$(TARGET_CMP_GAUSS_TEST)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Test in FFT_test directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd FFT_test; \
		$(GMAKE) BUILDDIR="$(BUILDDIR)" \
		GMAKE="$(GMAKE)" TARGET_FFTPACK_TEST="$(TARGET_FFTPACK_TEST)" TARGET_FFW3_F_TEST="$(TARGET_FFW3_F_TEST)" \
		TARGET_SGL_FFW3_TEST="$(TARGET_SGL_FFW3_TEST)" TARGET_CMPR_FFT_TEST="$(TARGET_CMPR_FFT_TEST)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Tests in Communication_check directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd Communication_check; \
		$(GMAKE) BUILDDIR="$(BUILDDIR)" \
		GMAKE="$(GMAKE)" MPIRUN="$(MPIRUN)"  TARGET_GEN_SPH_GRID="$(TARGET_GEN_SPH_GRID)" \
		TARGET_CHECK_SPH_GRID="$(TARGET_CHECK_SPH_GRID)" TARGET_TEST_SPH_GRID="$(TARGET_TEST_SPH_GRID)" \
		test
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Tests in restart_assemble directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd restart_assemble; \
		$(GMAKE) BUILDDIR="$(BUILDDIR)" \
		GMAKE="$(GMAKE)" MPIRUN="$(MPIRUN)"  TARGET_GEN_SPH_GRID="$(TARGET_GEN_SPH_GRID)" \
		TARGET_NEWSPH="$(TARGET_NEWSPH)" TARGET_CMP_SPH_RST="$(TARGET_CMP_SPH_RST)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Test in Control_file_check directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd Control_file_check; \
		$(GMAKE) BUILDDIR="$(BUILDDIR)" \
		GMAKE="$(GMAKE)" TARGET_CONTROL_MHD_CHECK="$(TARGET_CONTROL_MHD_CHECK)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Tests in Dynamo_Benchmark directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd Dynamo_Benchmark; \
		$(GMAKE) BUILDDIR="$(BUILDDIR)" \
		GMAKE="$(GMAKE)" MPIRUN="$(MPIRUN)"  TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		TARGET_SPH_SNAP="$(TARGET_SPH_SNAP)" TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		TARGET_COMPARE_PSF="$(TARGET_COMPARE_PSF)" TARGET_PSF_2_VTK="$(TARGET_PSF_2_VTK)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Tests in Models directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd Models; \
		$(GMAKE) SRCDIR="$(SRCDIR)" BUILDDIR="$(BUILDDIR)" \
		GMAKE="$(GMAKE)" MPIRUN="$(MPIRUN)"  TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		TARGET_SPH_SNAP="$(TARGET_SPH_SNAP)" TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		TARGET_COMPARE_PSF="$(TARGET_COMPARE_PSF)" TARGET_PSF_2_VTK="$(TARGET_PSF_2_VTK)" \
		TARGET_SPH_PICK_MODE_CHECK="$(TARGET_SPH_PICK_MODE_CHECK)" \
		TARGET_SPH_REF_CHECK="$(TARGET_SPH_REF_CHECK)" \
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
