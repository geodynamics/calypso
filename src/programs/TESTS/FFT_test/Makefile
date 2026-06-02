#
#
#
#
#

FFT_TEST_DIR =       $$(TESTS_DIR)/FFT_test

TARGET_FFTPACK_TEST =  test_FFTPACK
TARGET_FFW3_F_TEST =   test_FFTW3_f
TARGET_SGL_FFW3_TEST = test_single_FFTW3

TARGET_CMPR_FFT_TEST = compare_fft_test

TARGETS_FFT_TEST = \
'$$(TARGET_FFTPACK_TEST)' \
'$$(TARGET_CMPR_FFT_TEST)'

ifdef FFTW3_LIBS
  TARGETS_FFT_TEST+= \
   '$$(TARGET_FFW3_F_TEST)' \
   '$$(TARGET_SGL_FFW3_TEST)'
endif


LIBS_FFT_TEST = \
'$$(LIB_MESH)'      \
'$$(LIB_FFTPACK)'   \
'$$(LIB_NETLIB)'

LIB_FILES_FFT_TEST = \
'$$(LIB_MESH_FILE)'      \
'$$(LIB_FFTPACK_FILE)'   \
'$$(LIB_NETLIB_FILE)'

SOURCES = $(shell ls *.f90)

MOD_FFTPACK_TEST = \
m_precision.o \
m_constants.o \
m_machine_parameter.o \
m_FFTPACK_labels.o \
m_ISPACK1_labels.o \
m_FFTW_labels.o \
m_FFT_labels.o \
m_FFT_selector.o \
\
t_multi_flag_labels.o \
skip_comment_f.o \
normalize_for_FFTPACK.o \
multi_pin_FFTPACK_smp.o \
multi_pout_FFTPACK_smp.o \
calypso_multi_fftpack.o \
t_FFTPACK5_wrapper.o \
\
m_FFT_size.o      \
t_fft_test_data.o \
test_FFTPACK5.o

MOD_FFW3_F_TEST = \
m_precision.o \
m_constants.o \
m_machine_parameter.o \
\
skip_comment_f.o \
m_fftw_parameters.o \
normalize_for_FFTW.o \
multi_pin_FFTW3_smp.o \
multi_pout_FFTW3_smp.o \
calypso_multi_FFTW3.o \
t_multi_FFTW_wrapper.o \
\
m_FFT_size.o      \
t_fft_test_data.o \
test_FFTW3.o

MOD_SGL_FFW3_TEST = \
m_precision.o \
m_constants.o \
m_machine_parameter.o \
\
skip_comment_f.o \
m_fftw_parameters.o \
t_single_FFTW_wrapper.o \
normalize_for_FFTW.o \
single_pin_FFTW3_smp.o \
single_pout_FFTW3_smp.o \
calypso_single_FFTW3.o \
\
m_FFT_size.o      \
t_fft_test_data.o \
test_single_FFTW3.o

MOD_CMPR_FFT_TEST = \
m_precision.o \
m_constants.o \
m_machine_parameter.o \
\
compare_FFT_test.o \
m_FFT_size.o      \
t_fft_test_data.o

MOD_CMPR_FFT_TEST = \
m_precision.o \
m_constants.o \
m_machine_parameter.o \
\
compare_FFT_test.o \
t_fft_test_data.o


#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'FFT_TEST_DIR =  $(FFT_TEST_DIR)'          >> $(MAKENAME)

target_list:
	@echo 'TARGET_FFTPACK_TEST =  $$(TEST_BUILD_DIR)/$(TARGET_FFTPACK_TEST)' >> $(MAKENAME)
	@echo 'TARGET_FFW3_F_TEST =   $$(TEST_BUILD_DIR)/$(TARGET_FFW3_F_TEST)' >> $(MAKENAME)
	@echo 'TARGET_SGL_FFW3_TEST = $$(TEST_BUILD_DIR)/$(TARGET_SGL_FFW3_TEST)' >> $(MAKENAME)
	@echo 'TARGET_CMPR_FFT_TEST = $$(TEST_BUILD_DIR)/$(TARGET_CMPR_FFT_TEST)' >> $(MAKENAME)

target_task:
	@echo fft_test: $(TARGETS_FFT_TEST) >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FFTPACK_TEST)': '$$(MOD_FFTPACK_TEST)' \
	'$$(LIB_FFTPACK_FILE)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FFTPACK_TEST)' \
	'$$(MOD_FFTPACK_TEST)  -L. $$(LIB_FFTPACK)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FFW3_F_TEST)': '$$(MOD_FFW3_F_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FFW3_F_TEST)' \
	'$$(MOD_FFW3_F_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SGL_FFW3_TEST)': '$$(MOD_SGL_FFW3_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SGL_FFW3_TEST)' \
	'$$(MOD_SGL_FFW3_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CMPR_FFT_TEST)': '$$(MOD_CMPR_FFT_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_CMPR_FFT_TEST)' \
	'$$(MOD_CMPR_FFT_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:
	@echo  LIBS_FFT_TEST=  \\          >> $(MAKENAME)
	@echo  $(LIBS_FFT_TEST)            >> $(MAKENAME)
	@echo  LIB_FILES_FFT_TEST=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_FFT_TEST)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_FFTPACK_TEST=      \\  >> $(MAKENAME)
	@echo  $(MOD_FFTPACK_TEST)        >> $(MAKENAME)
	@echo  MOD_FFW3_F_TEST=       \\  >> $(MAKENAME)
	@echo  $(MOD_FFW3_F_TEST)         >> $(MAKENAME)
	@echo  MOD_SGL_FFW3_TEST=      \\  >> $(MAKENAME)
	@echo  $(MOD_SGL_FFW3_TEST)        >> $(MAKENAME)
	@echo  MOD_CMPR_FFT_TEST=      \\  >> $(MAKENAME)
	@echo  $(MOD_CMPR_FFT_TEST)        >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FFT_TEST_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
