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
MPIRUN = mpirun
TARGET_SPH_MHD =       $(SRCDIR)/bin/sph_mhd
TARGET_SPH_SNAP =      $(SRCDIR)/bin/sph_snapshot
TARGET_SPH_ENE_CHECK = $(SRCDIR)/bin/tests/sph_ene_check
TARGET_COMPARE_PSF =   $(SRCDIR)/bin/tests/compare_psf
TARGET_PSF_2_VTK =     $(SRCDIR)/bin/utilities/section_to_VTK
#
#  -------------------------------------------------------------------------
#

all: test

test:
	@echo "# Test in heterogineous_temp_static directory..."
	@cd heterogineous_temp_static; \
		$(GMAKE) MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		test
	@echo "# Test in heterogineous_temp_evolve directory..."
	@cd heterogineous_temp_evolve; \
		$(GMAKE) MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		test
	
	rm -f *.o *.mod *~

clean:
	cd heterogineous_temp_static; make clean
	cd heterogineous_temp_evolve; make clean
	
	rm -f *.o *.mod *~

distclean: clean
