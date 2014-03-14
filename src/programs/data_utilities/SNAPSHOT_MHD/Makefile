#
#
#

SPH_SNAP_MAINDIR =   $$(DATA_UTILS_DIR)/SNAPSHOT_MHD

TARGET_SPH_SNAP_NOVIZ =    sph_snapshot
TARGET_SPH_ZM_SNAP_NOVIZ = sph_zm_snapshot

TARGET_SPH_D_BENCH =    sph_dynamobench


SOURCES = $(shell ls *.f90)

MOD_SPH_SNAP_NOVIZ_MAIN =  \
main_sph_snapshot_noviz.o \
analyzer_noviz_sph_snap.o \
SPH_analyzer_snap.o \
FEM_analyzer_sph_MHD.o

MOD_SPH_ZM_SNAP_NOVIZ_MAIN =  \
main_sph_zm_snap_noviz.o \
analyzer_noviz_sph_zm_snap.o \
SPH_analyzer_snap.o \
SPH_analyzer_zm_snap.o \
FEM_analyzer_sph_MHD.o

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
	@echo 'TARGET_SPH_SNAP_NOVIZ = $$(BUILDDIR)/$(TARGET_SPH_SNAP_NOVIZ)'>> $(MAKENAME)
	@echo 'TARGET_SPH_ZM_SNAP_NOVIZ = $$(BUILDDIR)/$(TARGET_SPH_ZM_SNAP_NOVIZ)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo sph_snapshots: \
	 '$$(TARGET_SPH_D_BENCH)'    \
	 '$$(TARGET_SPH_SNAP_NOVIZ)' \
	 '$$(TARGET_SPH_ZM_SNAP_NOVIZ)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_SNAP_NOVIZ):  $$(MOD_SPH_SNAP_NOVIZ_MAIN)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_SNAP_NOVIZ)' '$$(MOD_SPH_SNAP_NOVIZ_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_ZM_SNAP_NOVIZ):  $$(MOD_SPH_ZM_SNAP_NOVIZ_MAIN)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_ZM_SNAP_NOVIZ)' '$$(MOD_SPH_ZM_SNAP_NOVIZ_MAIN)' \
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
	@echo  MOD_SPH_SNAP_NOVIZ_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAP_NOVIZ_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_ZM_SNAP_NOVIZ_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_ZM_SNAP_NOVIZ_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_D_BENCH_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_D_BENCH_MAIN) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SPH_SNAP_MAINDIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
