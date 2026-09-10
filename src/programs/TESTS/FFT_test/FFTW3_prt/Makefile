#
#
#
TEST_PRT_FFTW_DIR =    $$(FFT_TEST_DIR)/FFTW3_prt
TARGET_PRT_FFTW_TEST = $$(TEST_BUILD_DIR)/test_prt_FFTW
TARGETS = 

SOURCES = $(shell ls *.f90)
MOD_PRT_FFTW_TEST = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'TEST_PRT_FFTW_DIR = $(TEST_PRT_FFTW_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_PRT_FFTW_TEST = $(TARGET_PRT_FFTW_TEST)' >> $(MAKENAME)
	@echo '$(TARGETS) += $$(TARGET_PRT_FFTW_TEST)'         >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_PRT_FFTW_TEST)': \
	'$$(MOD_PRT_FFTW_TEST) $$(LIB_MESH_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_PRT_FFTW_TEST)' \
	'$$(MOD_PRT_FFTW_TEST)' \
	'-L. $$(LIB_MESH) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

lib_tasks:

mod_list:
	@echo  MOD_PRT_FFTW_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_PRT_FFTW_TEST)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(TEST_PRT_FFTW_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

distclean: clean
