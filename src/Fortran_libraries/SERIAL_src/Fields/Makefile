#
#
#

FIELDIR = $$(CORE_SRCDIR)/Fields
SOURCES = $(shell ls *.f90)
MOD_FIELDS = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FIELDIR = $(FIELDIR)'  >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_FIELDS)' >> $(MAKENAME)


mod_list:
	@echo  '$(UPPER_MODULES)+= $$(MOD_FIELDS)' >> $(MAKENAME)
	@echo  'MOD_FIELDS= '  \\ >> $(MAKENAME)
	@echo  '$(MOD_FIELDS)'    >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FIELDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
