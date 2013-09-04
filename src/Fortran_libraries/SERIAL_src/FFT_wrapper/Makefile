#
#
#

FFT_WRAP_DIR = $$(CORE_SRCDIR)/FFT_wrapper
SRC_FFT_WRAP = $(shell ls *.f90 *.F90)
MOD_FFT_WRAP = $(addsuffix .o,$(basename $(SRC_FFT_WRAP)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FFT_WRAP_DIR = $(FFT_WRAP_DIR)'  >> $(MAKENAME)

lib_name:

libtarget:

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_FFT_WRAP)' >> $(MAKENAME)

mod_list:
	@echo MOD_FFT_WRAP=  \\ >> $(MAKENAME)
	@echo $(MOD_FFT_WRAP) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(FFT_WRAP_DIR)' $(SRC_FFT_WRAP)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
