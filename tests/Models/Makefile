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
MPIRUN = mpirun
TARGET_SPH_MHD =             $(BUILDDIR)/sph_mhd
TARGET_SPH_SNAP =            $(BUILDDIR)/sph_snapshot
TARGET_PSF_2_VTK =           $(BUILDDIR)/utilities/section_to_VTK
TARGET_COMPARE_PSF =         $(BUILDDIR)/tests/compare_psf
TARGET_SPH_ENE_CHECK =       $(BUILDDIR)/tests/sph_ene_check
TARGET_SPH_PICK_MODE_CHECK = $(BUILDDIR)/tests/sph_picked_mode_check
TARGET_SPH_REF_CHECK =       $(BUILDDIR)/tests/sph_ref_field_check
#

#  -------------------------------------------------------------------------
#

all: test

test:
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Test in eq_symmetry_fields directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	@cd eq_symmetry_fields; \
		$(GMAKE) BUILDDIR="$(BUILDDIR)" \
		GMAKE="$(GMAKE)" MPIRUN="$(MPIRUN)"  TARGET_SPH_MHD="$(TARGET_SPH_MHD)" \
		TARGET_SPH_SNAP="$(TARGET_SPH_SNAP)" TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		TARGET_COMPARE_PSF="$(TARGET_COMPARE_PSF)" TARGET_PSF_2_VTK="$(TARGET_PSF_2_VTK)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Test in static_heat_source_by_rst directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	cd static_heat_source_by_rst; \
		$(GMAKE) SRCDIR="$(SRCDIR)" MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		TARGET_SPH_PICK_MODE_CHECK="$(TARGET_SPH_PICK_MODE_CHECK)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Test in static_heat_source_by_ref directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	cd static_heat_source_by_ref; \
		$(GMAKE) SRCDIR="$(SRCDIR)" MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		TARGET_SPH_PICK_MODE_CHECK="$(TARGET_SPH_PICK_MODE_CHECK)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Test in perturbation_scalars_by_ref directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	cd perturbation_scalars_by_ref; \
		$(GMAKE) SRCDIR="$(SRCDIR)" MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" TARGET_SPH_REF_CHECK="$(TARGET_SPH_REF_CHECK)" \
		TARGET_SPH_PICK_MODE_CHECK="$(TARGET_SPH_PICK_MODE_CHECK)" \
		test
		
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Test in perturbation_scalars_by_file directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	cd perturbation_scalars_by_file; \
		$(GMAKE) SRCDIR="$(SRCDIR)" MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" TARGET_SPH_REF_CHECK="$(TARGET_SPH_REF_CHECK)" \
		TARGET_SPH_PICK_MODE_CHECK="$(TARGET_SPH_PICK_MODE_CHECK)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Test in static_heat_source_inner_core directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	cd static_heat_source_inner_core; \
		$(GMAKE) SRCDIR="$(SRCDIR)" MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" TARGET_SPH_REF_CHECK="$(TARGET_SPH_REF_CHECK)" \
		TARGET_SPH_PICK_MODE_CHECK="$(TARGET_SPH_PICK_MODE_CHECK)" \
		test
	
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Test in heterogineous_temp_static directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	cd heterogineous_temp_static; \
		$(GMAKE) MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		TARGET_SPH_PICK_MODE_CHECK="$(TARGET_SPH_PICK_MODE_CHECK)" \
		test
		
	@echo '#'
	@echo '# -----------------------------------------------------------'
	@echo "#   Test in heterogineous_temp_evolve directory..."
	@echo '# -----------------------------------------------------------'
	@echo '#'
	cd heterogineous_temp_evolve; \
		$(GMAKE) MPIRUN="$(MPIRUN)" \
		TARGET_SPH_MHD="$(TARGET_SPH_MHD)" TARGET_SPH_ENE_CHECK="$(TARGET_SPH_ENE_CHECK)" \
		TARGET_SPH_PICK_MODE_CHECK="$(TARGET_SPH_PICK_MODE_CHECK)" \
		test
	
	@rm -f *.o *.mod *~
	@echo 'Tests in Model folder are finished'


clean:
	cd eq_symmetry_fields; make clean
	
	cd static_heat_source_by_rst;     make clean
	cd static_heat_source_by_ref;     make clean
	
	cd perturbation_scalars_by_ref;   make clean
	cd perturbation_scalars_by_file;  make clean
	
	cd static_heat_source_inner_core; make clean
	
	cd heterogineous_temp_static;     make clean
	cd heterogineous_temp_evolve;     make clean
	
	rm -f *.o *.mod *~

distclean: clean
