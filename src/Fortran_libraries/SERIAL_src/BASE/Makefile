#
#
#

BASEDIR = $$(CORE_SRCDIR)/BASE
SRC_BASE = $(shell ls *.f90)
MOD_BASE = $(addsuffix .o,$(basename $(SRC_BASE)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'BASEDIR = $(BASEDIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_BASE)' >> $(MAKENAME)


mod_list:
	@echo  MOD_BASE=  \\ >> $(MAKENAME)
	@echo  $(MOD_BASE) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(BASEDIR)' $(SRC_BASE)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
