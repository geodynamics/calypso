#
#
#

SPH_SNAP_MAINDIR =   $$(DATA_UTILS_DIR)/SNAPSHOT_MHD

TARGET_SPH_VIZS_SNAP =  sph_snapshot_vizs
TARGET_SPH_PSF_SNAP =   sph_snapshot

SOURCES = $(shell ls *.f90)

MOD_SPH_SNAP_VIZS_MAIN =  \
main_sph_snapshot_w_vizs.o \
analyzer_sph_snap_w_vizs.o \
SPH_analyzer_snap_w_vizs.o

MOD_SPH_SNAP_PSF_MAIN =  \
main_sph_snapshot_w_psf.o \
analyzer_sph_snap_w_psf.o \
SPH_analyzer_snap_w_vizs.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_SNAP_MAINDIR = $(SPH_SNAP_MAINDIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_SPH_PSF_SNAP =      $$(BUILDDIR)/$(TARGET_SPH_PSF_SNAP)'>> $(MAKENAME)
	@echo 'TARGET_SPH_VIZS_SNAP = $$(BUILDDIR)/$(TARGET_SPH_VIZS_SNAP)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo sph_snapshots: \
	 '$$(TARGET_SPH_PSF_SNAP)'    \
	 '$$(TARGET_SPH_VIZS_SNAP)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_PSF_SNAP):  $$(MOD_SPH_SNAP_PSF_MAIN)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_PSF_SNAP)' '$$(MOD_SPH_SNAP_PSF_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_VIZS_SNAP):  $$(MOD_SPH_SNAP_VIZS_MAIN)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_VIZS_SNAP)' '$$(MOD_SPH_SNAP_VIZS_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_SPH_SNAP_PSF_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAP_PSF_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_SNAP_VIZS_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAP_VIZS_MAIN) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SPH_SNAP_MAINDIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
