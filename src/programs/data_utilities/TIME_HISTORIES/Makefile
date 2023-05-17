#
#
#

SPH_UTILS_DIR = $$(DATA_UTILS_DIR)/TIME_HISTORIES

TARGET_TAVE_SPH_ENE_SPEC =   utilities/t_ave_monitor_data
TARGET_SPH_ENE_CHECK =       tests/sph_ene_check

SOURCES = $(shell ls *.f90 *.F90)

MOD_TAVE_SPH_ENE_SPEC = \
t_average_sph_ene_spec.o \
t_ctl_data_sph_monitor_list.o \
t_ctl_data_tave_sph_monitor.o \
t_ctl_param_sph_series_util.o \
t_tave_sph_layer_spectr.o \
t_tave_sph_layer_mean.o \
t_tave_sph_volume_spectr.o \
t_picked_sph_spectr_data_IO.o \
sph_layer_monitor_snap_IO.o \
sph_volume_monitor_snap_IO.o \
tave_sdev_sph_layer_spec.o \
tave_sdev_sph_layer_mean.o \
tave_sdev_sph_volume_spec.o \
time_ave_sph_volume_mean.o \
time_ave_picked_sph_spectr.o \
t_sph_volume_mean_series.o \
t_sph_volume_spectr_series.o \
picked_sph_spectr_data_IO.o \
gz_spl_sph_spectr_data_IO.o \
old_sph_spectr_data_IO.o \
write_snap_pick_sph_spectr.o \
count_monitor_time_series.o \
cal_tave_sph_ene_spectr.o

MOD_SPH_ENE_CHECK = \
compare_sph_mean_square.o


#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_UTILS_DIR = $(SPH_UTILS_DIR)'     >> $(MAKENAME)

target_list:
	@echo 'TARGET_TAVE_SPH_ENE_SPEC = $$(BUILDDIR)/$(TARGET_TAVE_SPH_ENE_SPEC)' \
	>> $(MAKENAME)
	@echo 'TARGET_SPH_ENE_CHECK = $$(BUILDDIR)/$(TARGET_SPH_ENE_CHECK)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo sph_data_util: \
	'$$(TARGET_TAVE_SPH_ENE_SPEC)  $$(TARGET_SPH_ENE_CHECK)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)
	@echo '$$(TARGET_TAVE_SPH_ENE_SPEC)': '$$(MOD_TAVE_SPH_ENE_SPEC)' \
	'$$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_TAVE_SPH_ENE_SPEC)' \
	'$$(MOD_TAVE_SPH_ENE_SPEC)' \
	'-L. $$(LIBS_SPH_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_ENE_CHECK)': '$$(MOD_SPH_ENE_CHECK)' \
	'$$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_ENE_CHECK)' \
	'$$(MOD_SPH_ENE_CHECK)' \
	'-L. $$(LIBS_SPH_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_TAVE_SPH_ENE_SPEC=  \\  >> $(MAKENAME)
	@echo  $(MOD_TAVE_SPH_ENE_SPEC)    >> $(MAKENAME)
	@echo  MOD_SPH_ENE_CHECK=  \\     >> $(MAKENAME)
	@echo  $(MOD_SPH_ENE_CHECK)       >> $(MAKENAME)

module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SPH_UTILS_DIR)' $(SOURCES)
