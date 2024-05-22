#
#
#

SMDT_POLY_TEST_DIR = $$(TESTS_DIR)/Legendre_test

TARGET_SMDT_POLY =    test_schmidt_Legendre

SOURCES = $(shell ls *.f90)

MOD_SMDT_POLY_TEST = \
test_schmidt_poly_med.o \
schmidt_poly_on_meridian.o \
t_spherical_harmonics.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'SMDT_POLY_TEST_DIR = $(SMDT_POLY_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_SMDT_POLY =   $$(TEST_BUILD_DIR)/$(TARGET_SMDT_POLY)'  >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo leg_tests: \
	'$$(TARGET_SMDT_POLY)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SMDT_POLY)': '$$(MOD_SMDT_POLY_TEST)' \
	'$$(LIB_FILES_SPH_MHD)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SMDT_POLY)' \
	'$$(MOD_SMDT_POLY_TEST)  -L. $$(LIBS_SPH_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_SMDT_POLY_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_SMDT_POLY_TEST) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SMDT_POLY_TEST_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
