#
#
#

GAUSSDIR = $$(CORE_SRCDIR)/gauss_integration

SOURCES = $(shell ls *.f90)
MOD_GAUSS = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'GAUSSDIR = $(GAUSSDIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_GAUSS)' >> $(MAKENAME)

mod_list:
	@echo  MOD_GAUSS=  \\ >> $(MAKENAME)
	@echo  $(MOD_GAUSS) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(GAUSSDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
