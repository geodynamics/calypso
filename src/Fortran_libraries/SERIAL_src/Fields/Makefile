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

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_FIELDS)' >> $(MAKENAME)


mod_list:
	@echo  MOD_FIELDS=  \\ >> $(MAKENAME)
	@echo  $(MOD_FIELDS) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(FIELDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
