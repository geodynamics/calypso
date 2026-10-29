#
#
#
COMPARE_FFT_TEST_DIR = $$(FFT_TEST_DIR)/compare_FFT_test
TARGET_CMPR_FFT_TEST = $$(TEST_BUILD_DIR)/compare_FFT_test
TARGETS = 

SOURCES = $(shell ls *.f90)
MOD_CMPR_FFT_TEST = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'COMPARE_FFT_TEST_DIR = $(COMPARE_FFT_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_CMPR_FFT_TEST = $(TARGET_CMPR_FFT_TEST)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_CMPR_FFT_TEST)': \
	'$$(MOD_CMPR_FFT_TEST) $$(LIB_MESH_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_CMPR_FFT_TEST)' \
	'$$(MOD_CMPR_FFT_TEST)' \
	'-L. $$(LIB_MESH) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

lib_tasks:

mod_list:
	@echo  MOD_CMPR_FFT_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_CMPR_FFT_TEST)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(COMPARE_FFT_TEST_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

distclean: clean
