#
#
#

GAUSSDIR = $$(CORE_SRCDIR)/gauss_integration

SOURCES = $(shell ls *.f90)
MOD_GAUSS = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_GAUSS'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'GAUSSDIR = $(GAUSSDIR)'  >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_GAUSS)'         >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(GAUSSDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
