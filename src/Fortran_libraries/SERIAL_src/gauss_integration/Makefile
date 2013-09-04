#
#
#

GAUSSDIR = $$(CORE_SRCDIR)/gauss_integration

SRC_GAUSS = $(shell ls *.f90)
MOD_GAUSS = $(addsuffix .o,$(basename $(SRC_GAUSS)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'GAUSSDIR = $(GAUSSDIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_GAUSS)' >> $(MAKENAME)

mod_list:
	@echo  MOD_GAUSS=  \\ >> $(MAKENAME)
	@echo  $(MOD_GAUSS) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(GAUSSDIR)' $(SRC_GAUSS)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
