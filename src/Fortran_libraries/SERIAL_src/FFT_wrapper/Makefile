#
#
#

FFT_WRAP_DIR = $$(CORE_SRCDIR)/FFT_wrapper
SOURCES = $(shell ls *.f90 *.F90)

MOD_FFT_WRAP = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_FFT_WRAP'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FFT_WRAP_DIR = $(FFT_WRAP_DIR)'  >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_FFT_WRAP)'      >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FFT_WRAP_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
