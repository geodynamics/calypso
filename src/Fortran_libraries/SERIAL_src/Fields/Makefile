#
#
#

FIELDIR = $$(CORE_SRCDIR)/Fields
SOURCES = $(shell ls *.f90)

MOD_FIELDS = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_FIELDS'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FIELDIR = $(FIELDIR)'  >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)


mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_FIELDS)'        >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FIELDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
