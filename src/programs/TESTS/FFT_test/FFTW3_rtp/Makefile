#
#
#
TEST_RTP_FFTW_DIR =    $$(FFT_TEST_DIR)/FFTW3_rtp
TARGET_RTP_FFTW_TEST = $$(TEST_BUILD_DIR)/test_rtp_FFTW
TARGETS = 

SOURCES = $(shell ls *.f90)
MOD_RTP_FFTW_TEST = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'TEST_RTP_FFTW_DIR = $(TEST_RTP_FFTW_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_RTP_FFTW_TEST = $(TARGET_RTP_FFTW_TEST)' >> $(MAKENAME)
	@echo '$(TARGETS) += $$(TARGET_RTP_FFTW_TEST)'         >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_RTP_FFTW_TEST)': \
	'$$(MOD_RTP_FFTW_TEST) $$(LIB_MESH_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_RTP_FFTW_TEST)' \
	'$$(MOD_RTP_FFTW_TEST)' \
	'-L. $$(LIB_MESH) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

lib_tasks:

mod_list:
	@echo  MOD_RTP_FFTW_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_RTP_FFTW_TEST)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(TEST_RTP_FFTW_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

distclean: clean
