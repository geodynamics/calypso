#
#
#

SPH_DIR = $$(CORE_SRCDIR)/spherical_harmonics
SRC_SPH = $(shell ls *.f90 *.F90)
MOD_SPH = $(addsuffix .o,$(basename $(SRC_SPH)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_DIR = $(SPH_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_SPH)' >> $(MAKENAME)

mod_list:
	@echo MOD_SPH=  \\ >> $(MAKENAME)
	@echo $(MOD_SPH) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SPH_DIR)' $(SRC_SPH)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
