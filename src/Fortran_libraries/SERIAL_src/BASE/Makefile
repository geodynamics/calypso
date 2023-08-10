#
#
#

BASEDIR = $$(CORE_SRCDIR)/BASE
SOURCES = $(shell ls *.f90)
MOD_BASE = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'BASEDIR = $(BASEDIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_BASE)' >> $(MAKENAME)


mod_list:
	@echo  MOD_BASE=  \\ >> $(MAKENAME)
	@echo  $(MOD_BASE) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(BASEDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
