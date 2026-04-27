#
#
#

BASEDIR = $$(CORE_SRCDIR)/BASE
SOURCES = $(shell ls *.f90)

MOD_BASE = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_BASE'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'BASEDIR = $(BASEDIR)'  >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)


mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_BASE)'          >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(BASEDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
