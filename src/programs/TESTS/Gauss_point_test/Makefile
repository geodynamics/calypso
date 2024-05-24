#
#
#

GAUSS_TEST_DIR =     $$(TESTS_DIR)/Gauss_point_test

TARGET_GAUSS_TEST =     gauss_points
TARGET_CMP_GAUSS_TEST = compare_gauss_points

SOURCES = $(shell ls *.f90)

MOD_GAUSS_TEST = \
test_gauss_points.o \
gauss_points_IO.o

MOD_CMP_GAUSS_TEST = \
compare_gauss_points.o \
gauss_points_IO.o


#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'GAUSS_TEST_DIR = $(GAUSS_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_GAUSS_TEST =    $$(TEST_BUILD_DIR)/$(TARGET_GAUSS_TEST)'   \
	>> $(MAKENAME)
	@echo 'TARGET_CMP_GAUSS_TEST = $$(TEST_BUILD_DIR)/$(TARGET_CMP_GAUSS_TEST)'   \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo gauss_tests:  \
	'$$(TARGET_GAUSS_TEST)  $$(TARGET_CMP_GAUSS_TEST)'  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_GAUSS_TEST)': '$$(MOD_GAUSS_TEST)' \
	'$$(LIB_CALYPSO_FILE)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_GAUSS_TEST)  $$(MOD_GAUSS_TEST)' \
	'-L. $$(LIB_CALYPSO) $$(F90LIBS)'  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CMP_GAUSS_TEST)': '$$(MOD_CMP_GAUSS_TEST)' \
	'$$(LIB_CALYPSO_FILE)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	'-o $$(TARGET_CMP_GAUSS_TEST)  $$(MOD_CMP_GAUSS_TEST)' \
	'-L. $$(LIB_CALYPSO) $$(F90LIBS)'  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_GAUSS_TEST=    \\  >> $(MAKENAME)
	@echo  $(MOD_GAUSS_TEST)      >> $(MAKENAME)
	@echo  MOD_CMP_GAUSS_TEST=    \\  >> $(MAKENAME)
	@echo  $(MOD_CMP_GAUSS_TEST)      >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(GAUSS_TEST_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
