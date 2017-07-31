#
#
#

SPH_SNAP_MAINDIR =   $$(DATA_UTILS_DIR)/SNAPSHOT_MHD

TARGET_SPH_SNAP =    sph_snapshot
TARGET_SPH_ZM_SNAP = sph_zm_snapshot

TARGET_SPH_D_BENCH =    sph_dynamobench


SOURCES = $(shell ls *.f90)

MOD_SPH_SNAP_MAIN =  \
main_sph_snapshot_w_psf.o \
analyzer_sph_snap_w_psf.o \
SPH_analyzer_snap_w_psf.o

MOD_SPH_ZM_SNAP_MAIN =  \
main_sph_zm_snap_w_psf.o \
analyzer_sph_zm_snap_w_psf.o \
SPH_analyzer_snap_w_psf.o \
SPH_analyzer_zm_snap_w_psf.o

MOD_SPH_D_BENCH_MAIN = \
main_sph_dynamobench.o \
analyzer_sph_dynamobench.o \
SPH_analyzer_d_bench.o


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_SNAP_MAINDIR = $(SPH_SNAP_MAINDIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_SPH_D_BENCH =    $$(BUILDDIR)/$(TARGET_SPH_D_BENCH)'   >> $(MAKENAME)
	@echo 'TARGET_SPH_SNAP = $$(BUILDDIR)/$(TARGET_SPH_SNAP)'>> $(MAKENAME)
	@echo 'TARGET_SPH_ZM_SNAP = $$(BUILDDIR)/$(TARGET_SPH_ZM_SNAP)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo sph_snapshots: \
	 '$$(TARGET_SPH_D_BENCH)'    \
	 '$$(TARGET_SPH_SNAP)' \
	 '$$(TARGET_SPH_ZM_SNAP)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_SNAP):  $$(MOD_SPH_SNAP_MAIN)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_SNAP)' '$$(MOD_SPH_SNAP_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_ZM_SNAP):  $$(MOD_SPH_ZM_SNAP_MAIN)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_ZM_SNAP)' '$$(MOD_SPH_ZM_SNAP_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_D_BENCH):  $$(MOD_SPH_D_BENCH_MAIN)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_D_BENCH)' '$$(MOD_SPH_D_BENCH_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_SPH_SNAP_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAP_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_ZM_SNAP_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_ZM_SNAP_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_D_BENCH_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_D_BENCH_MAIN) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SPH_SNAP_MAINDIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
