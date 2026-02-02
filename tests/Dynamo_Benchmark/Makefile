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
MPIRUN = mpirun
TARGET_SPH_MHD =       $(BUILDDIR)/sph_mhd
TARGET_SPH_SNAP =      $(BUILDDIR)/sph_snapshot
TARGET_SPH_ENE_CHECK = $(BUILDDIR)/tests/sph_ene_check
TARGET_COMPARE_PSF =   $(BUILDDIR)/tests/compare_psf
TARGET_PSF_2_VTK =     $(BUILDDIR)/utilities/section_to_VTK
#
#  -------------------------------------------------------------------------
#

all: test

test:
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "# Test in Dynamobench_Case0 directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd Dynamobench_Case0; \
		$(GMAKE) MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		test
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "# Test in Dynamobench_Case1 directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd Dynamobench_Case1; \
		$(GMAKE) MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		test
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "# Test in Dynamobench_Case2 directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd Dynamobench_Case2; \
		$(GMAKE) MPIRUN="$(MPIRUN)" TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" TARGET_COMPARE_PSF="$(TARGET_COMPARE_PSF)" \
		test
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "# Test in Dynamobench_Case2_cont directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd Dynamobench_Case2_cont; \
		$(GMAKE) MPIRUN="$(MPIRUN)" TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" TARGET_COMPARE_PSF="$(TARGET_COMPARE_PSF)" \
		test
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "# Test in snapshot_test directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd snapshot_test; \
		$(GMAKE) MPIRUN="$(MPIRUN)" TARGET_SPH_SNAP="$(TARGET_SPH_SNAP)" \
		TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" TARGET_COMPARE_PSF="$(TARGET_COMPARE_PSF)" \
		test
	
	rm -f *.o *.mod *~

clean:
	cd Dynamobench_Case0; make clean
	cd Dynamobench_Case1; make clean
	cd Dynamobench_Case2; make clean
	cd Dynamobench_Case2_cont; make clean
	cd snapshot_test; make clean
	
	rm -f *.o *.mod *~

distclean: clean
