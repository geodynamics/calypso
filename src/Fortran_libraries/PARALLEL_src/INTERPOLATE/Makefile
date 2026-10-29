#
#
#

INTERPOLATE_DIR = $$(PARA_SRCDIR)/INTERPOLATE
SOURCES = $(shell ls *.f90)

MOD_INTERPOLATE = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_INTERPOLATE'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'INTERPOLATE_DIR = $(INTERPOLATE_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_INTERPOLATE)'    >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(INTERPOLATE_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
