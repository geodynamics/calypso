#
#
#

SPH_SPECTR_DIR = $$(CORE_SRCDIR)/SPH_SPECTR_src
SRC_SPH_SPECTR = $(shell ls *.f90)
MOD_SPH_SPECTR = $(addsuffix .o,$(basename $(SRC_SPH_SPECTR)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_SPECTR_DIR = $(SPH_SPECTR_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_SPH_SPECTR)' >> $(MAKENAME)

mod_list:
	@echo  MOD_SPH_SPECTR=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SPECTR) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SPH_SPECTR_DIR)' $(SRC_SPH_SPECTR)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
