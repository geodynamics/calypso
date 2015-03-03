#
#
#

MHD_SPH_DIR = $$(MHD_SRCDIR)/sph_MHD
SRC_SPH_MHD = $(shell ls *.f90 *.F90)
MOD_SPH_MHD = $(addsuffix .o,$(basename $(SRC_SPH_MHD)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_SPH_DIR = $(MHD_SPH_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_SPH_MHD)' \
	>> $(MAKENAME)

mod_list:
	@echo MOD_SPH_MHD=  \\ >> $(MAKENAME)
	@echo $(MOD_SPH_MHD) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MHD_SPH_DIR)' $(SRC_SPH_MHD)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
