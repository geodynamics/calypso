#
#
#

SPH_UTILS_DIR = $$(DATA_UTILS_DIR)/TIME_HISTORIES

TARGET_TAVE_SPH_ENE_SPEC =   t_ave_sph_mean_square
TARGET_T_AVE_PICK_SPH_SPEC = t_ave_picked_sph_coefs
TARGET_SPH_ENE_CHECK =       sph_ene_check

SOURCES = $(shell ls *.f90)

MOD_TAVE_SPH_ENE_SPEC = \
t_average_sph_ene_spec.o \
t_read_sph_spectra.o \
m_tave_sph_ene_spectr.o \
sph_mean_square_IO_select.o \
simple_sph_spectr_head_IO.o \
simple_sph_spectr_data_IO.o \
cal_tave_sph_ene_spectr.o

MOD_T_AVE_PICK_SPH = \
tave_picked_sph_spec_data.o

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
	@echo 'TARGET_T_AVE_PICK_SPH_SPEC = $$(BUILDDIR)/$(TARGET_T_AVE_PICK_SPH_SPEC)' \
	>> $(MAKENAME)
	@echo 'TARGET_SPH_ENE_CHECK = $$(BUILDDIR)/$(TARGET_SPH_ENE_CHECK)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo sph_data_util: \
	'$$(TARGET_TAVE_SPH_ENE_SPEC)  $$(TARGET_T_AVE_PICK_SPH_SPEC)' \
	'$$(TARGET_MAX_SPH_ENE)        $$(TARGET_SPH_ENE_CHECK)'    >> $(MAKENAME)
	@echo >> $(MAKENAME)
	@echo '$$(TARGET_TAVE_SPH_ENE_SPEC)': '$$(MOD_TAVE_SPH_ENE_SPEC)' \
	'$$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_TAVE_SPH_ENE_SPEC)' \
	'$$(MOD_TAVE_SPH_ENE_SPEC)' \
	'-L. $$(LIBS_SPH_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_T_AVE_PICK_SPH_SPEC)': '$$(MOD_T_AVE_PICK_SPH)' \
	'$$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_T_AVE_PICK_SPH_SPEC)' \
	'$$(MOD_T_AVE_PICK_SPH)' \
	'-L. $$(LIBS_SPH_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_ENE_CHECK)': '$$(MOD_SPH_ENE_CHECK)' \
	'$$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_ENE_CHECK)' \
	'$$(MOD_SPH_ENE_CHECK)' \
	'-L. $$(LIBS_SPH_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_TAVE_SPH_ENE_SPEC=  \\  >> $(MAKENAME)
	@echo  $(MOD_TAVE_SPH_ENE_SPEC)    >> $(MAKENAME)
	@echo  MOD_T_AVE_PICK_SPH=  \\     >> $(MAKENAME)
	@echo  $(MOD_T_AVE_PICK_SPH)       >> $(MAKENAME)
	@echo  MOD_SPH_ENE_CHECK=  \\     >> $(MAKENAME)
	@echo  $(MOD_SPH_ENE_CHECK)       >> $(MAKENAME)

module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SPH_UTILS_DIR)' $(SOURCES)
