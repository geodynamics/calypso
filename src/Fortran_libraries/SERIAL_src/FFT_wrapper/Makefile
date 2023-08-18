#
#
#

FFT_WRAP_DIR = $$(CORE_SRCDIR)/FFT_wrapper
SOURCES = $(shell ls *.f90 *.F90)
MOD_FFT_WRAP = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FFT_WRAP_DIR = $(FFT_WRAP_DIR)'  >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_FFT_WRAP)' >> $(MAKENAME)

mod_list:
	@echo MOD_FFT_WRAP=  \\ >> $(MAKENAME)
	@echo $(MOD_FFT_WRAP) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(FFT_WRAP_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
