#
#
#

PSF_SECTION_DIR = $$(VIZ_SRCDIR)/surfacing
SOURCES = $(shell ls *.f90)

MOD_SURFACING = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_SURFACING'
UPPER_MODULES=

#
#  --------------------------------------------------------------------
#

dir_list:
	@echo 'PSF_SECTION_DIR = $(PSF_SECTION_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_SURFACING)'     >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(PSF_SECTION_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
