#
#
#
#
#

FFT_TEST_DIR =       $$(TESTS_DIR)/FFT_test

SUBDIRS =   \
FFTPACK_rtp \
FFTPACK_prt \
\
compare_FFT_test


ifdef FFTW3_LIBS
  SUBDIRS+=  \
   FFTW3_rtp \
   FFTW3_prt \
   single_FFTW3_rtp
endif

TARGETS_EACH_FFT_TEST =

LIBS_FFT_TEST = \
'$$(LIB_MESH)'      \
'$$(LIB_FFTPACK)'   \
'$$(LIB_NETLIB)'

LIB_FILES_FFT_TEST = \
'$$(LIB_MESH_FILE)'      \
'$$(LIB_FFTPACK_FILE)'   \
'$$(LIB_NETLIB_FILE)'

SOURCES = $(shell ls *.f90)


#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'FFT_TEST_DIR =  $(FFT_TEST_DIR)'          >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( \
		echo 'generate diectory list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		dir_list \
		) \
	done
	@echo '#' >> $(MAKENAME)


target_list:
	@echo TARGETS_EACH_FFT_TEST =  $(TARGETS_EACH_FFT_TEST)  >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		BUILDDIR='$(BUILDDIR)' \
		MAKENAME='$(MAKENAME)' \
		TARGETS=TARGETS_EACH_FFT_TEST \
		target_list \
		) \
	done
	@echo '#' >> $(MAKENAME)


target_task:
	@echo 'each_fft_test: $$(TARGETS_EACH_FFT_TEST)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		target_task \
		) \
	done


lib_name:
	@echo  LIBS_FFT_TEST=  \\          >> $(MAKENAME)
	@echo  $(LIBS_FFT_TEST)            >> $(MAKENAME)
	@echo  LIB_FILES_FFT_TEST=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_FFT_TEST)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@for dir in $(SUBDIRS); do \
		( cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKENAME='$(MAKENAME)' \
		mod_list \
		) \
	done
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FFT_TEST_DIR)' $(SOURCES)
	@for dir in $(SUBDIRS); do \
		( \
		echo 'Construct compile list for ' $$dir ; \
		cd $${dir};  \
		$(GMAKE) \
		GMAKE='$(GMAKE)' \
		MAKE_MOD_DEP='$(MAKE_MOD_DEP)' \
		MAKENAME='$(MAKENAME)' \
		depends; \
		) \
	done

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
