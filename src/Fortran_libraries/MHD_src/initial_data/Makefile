#
#
#

MHD_INI_DIR = $$(MHD_SRCDIR)/initial_data
SOURCES =     $(shell ls *.f90)
MOD_INITIAL = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_INI_DIR = $(MHD_INI_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_INITIAL)' \
	>> $(MAKENAME)

mod_list:
	@echo '$(MOD_MHD)+= $$(MOD_INITIAL)' >> $(MAKENAME)
	@echo 'MOD_INITIAL= '  \\ >> $(MAKENAME)
	@echo '$(MOD_INITIAL)'    >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(MHD_INI_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
