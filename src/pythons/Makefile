#
#
#

PYTHON_SRC_DIR = $$(MHDDIR)/pythons

LIB_PYTHONS = '-L$$(PYTHONDIR) -lcalypso_to_pythons'

ifdef COCOA_FLAG
  LIB_FILE_PYTHONS = '$$(PYTHONDIR)/libcalypso_to_pythons.dylib'
else
  LIB_FILE_PYTHONS = '$$(PYTHONDIR)/libcalypso_to_pythons.so'
endif

LIB_PYTHON_UTILS =       '$$(LIB_CALYPSO)'
LIB_FILES_PYTHON_UTILS = '$$(LIB_CALYPSO_FILE)'

ifdef FORTRAN_C_CONNECT
  LIB_PYTHON_UTILS+=        '$$(LIB_CALYPSO_C)'
  LIB_FILES_PYTHON_UTILS+=  '$$(LIB_CALYPSO_C_FILE)'
endif

MOD_PYTHONS = \
'$$(MOD_MHD_F2003)' \
'$$(MOD_SPH_UTIL_DLL)'

MOD_SPH_UTIL_DLL = \
t_picked_sph_spectr_data_IO.o \
t_gauss_coefs_monitor_IO.o \
t_sph_volume_mean_series.o \
t_sph_volume_spectr_series.o \
t_tave_sph_volume_spectr.o \
t_tave_sph_layer_mean.o \
t_tave_sph_layer_spectr.o \
t_time_ave_sph_volume_mean.o \
tave_sdev_sph_layer_spec.o \
tave_sdev_sph_layer_mean.o \
tave_sdev_sph_volume_spec.o \
time_ave_sdev_sph_dbench.o \
gz_spl_sph_spectr_data_IO.o \
gz_volume_spectr_monitor_IO.o \
gz_layer_mean_monitor_IO.o \
gz_layer_spectr_monitor_IO.o \
sph_layer_monitor_snap_IO.o \
sph_volume_monitor_snap_IO.o \
old_sph_spectr_data_IO.o \
picked_sph_spectr_data_IO.o \
gz_gauss_coefs_monitor_IO.o \
count_monitor_time_series.o \
cal_tave_sph_ene_spectr.o \
m_read_sph_spectra_f.o \
m_load_picked_sph_spectr_f.o \
write_gauss_coefs_4_monitor.o \
time_average_sph_ene_spectr.o \
time_average_gauss_coefs.o \
write_snap_pick_sph_spectr.o \
time_ave_picked_sph_spectr.o \
time_ave_sph_monitor_data.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'PYTHON_SRC_DIR = $(PYTHON_SRC_DIR)'     >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

target_list:

target_task:

lib_name:
	@echo  LIB_PYTHONS=  \\                >> $(MAKENAME)
	@echo  $(LIB_PYTHONS)                  >> $(MAKENAME)
	@echo  LIB_FILE_PYTHONS=  \\           >> $(MAKENAME)
	@echo  $(LIB_FILE_PYTHONS)             >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@echo  LIB_PYTHON_UTILS=  \\           >> $(MAKENAME)
	@echo  $(LIB_PYTHON_UTILS)             >> $(MAKENAME)
	@echo  LIB_FILES_PYTHON_UTILS=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_PYTHON_UTILS)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

lib_tasks:
	@echo '$$(LIB_FILE_PYTHONS)': '$$(MOD_PYTHONS)' \
	'$$(LIB_FILES_PYTHON_UTILS)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' -shared  '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' -o '$$(LIB_FILE_PYTHONS)' \
	'$$(MOD_PYTHONS)' \
	'-L. $$(LIB_PYTHON_UTILS)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_PYTHONS=  \\     >> $(MAKENAME)
	@echo  $(MOD_PYTHONS)       >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)
	@echo  MOD_SPH_UTIL_DLL=  \\     >> $(MAKENAME)
	@echo  $(MOD_SPH_UTIL_DLL)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
