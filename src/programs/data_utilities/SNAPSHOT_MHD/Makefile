#
#
#

SPH_SNAP_MAINDIR =   $$(DATA_UTILS_DIR)/SNAPSHOT_MHD

<<<<<<< HEAD
TARGET_SPH_VIZS_SNAP =  sph_snapshot_vizs
TARGET_SPH_PSF_SNAP =   sph_snapshot
=======
TARGET_SPH_SNAP =    sph_snapshot

>>>>>>> refs/heads/Ver.2.0_alpha

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
<<<<<<< HEAD
	@echo 'TARGET_SPH_PSF_SNAP =      $$(BUILDDIR)/$(TARGET_SPH_PSF_SNAP)'>> $(MAKENAME)
	@echo 'TARGET_SPH_VIZS_SNAP = $$(BUILDDIR)/$(TARGET_SPH_VIZS_SNAP)' >> $(MAKENAME)
=======
	@echo 'TARGET_SPH_SNAP =    $$(BUILDDIR)/$(TARGET_SPH_SNAP)'>> $(MAKENAME)
>>>>>>> refs/heads/Ver.2.0_alpha
	@echo >> $(MAKENAME)

target_task:
	@echo sph_snapshots: \
<<<<<<< HEAD
	 '$$(TARGET_SPH_PSF_SNAP)'    \
	 '$$(TARGET_SPH_VIZS_SNAP)' \
=======
	 '$$(TARGET_SPH_SNAP)'    \
>>>>>>> refs/heads/Ver.2.0_alpha
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_PSF_SNAP):  $$(MOD_SPH_SNAP_PSF_MAIN)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_PSF_SNAP)' '$$(MOD_SPH_SNAP_PSF_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
<<<<<<< HEAD
	@echo '$$(TARGET_SPH_VIZS_SNAP):  $$(MOD_SPH_SNAP_VIZS_MAIN)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_VIZS_SNAP)' '$$(MOD_SPH_SNAP_VIZS_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
=======
>>>>>>> refs/heads/Ver.2.0_alpha


lib_name:

mod_list:
<<<<<<< HEAD
	@echo  MOD_SPH_SNAP_PSF_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAP_PSF_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_SNAP_VIZS_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAP_VIZS_MAIN) >> $(MAKENAME)
=======
	@echo  MOD_SPH_SNAP_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAP_MAIN) >> $(MAKENAME)
>>>>>>> refs/heads/Ver.2.0_alpha
	@echo  '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SPH_SNAP_MAINDIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
