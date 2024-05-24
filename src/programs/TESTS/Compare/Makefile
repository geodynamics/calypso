#
#
#

COMPARISON_DIR =     $$(TESTS_DIR)/Compare

TARGET_COMPARE_PSF = compare_psf

SOURCES = $(shell ls *.f90)

MOD_COMPARE_PSF = \
compare_psf_files.o \
t_ctl_param_psf_compares.o \
t_ctl_data_psf_compares.o \
t_ctl_data_psf_compare.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'COMPARISON_DIR = $(COMPARISON_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_COMPARE_PSF = $$(TEST_BUILD_DIR)/$(TARGET_COMPARE_PSF)'   \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo compare:  \
	'$$(TARGET_COMPARE_PSF)'  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_COMPARE_PSF)': '$$(MOD_COMPARE_PSF)' \
	'$$(LIB_CALYPSO_FILE)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_COMPARE_PSF)  $$(MOD_COMPARE_PSF)' \
	'-L. $$(LIB_CALYPSO) $$(LIB_CALYPSO_C) $$(F90LIBS)'  >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_COMPARE_PSF=    \\  >> $(MAKENAME)
	@echo  $(MOD_COMPARE_PSF)      >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(COMPARISON_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
