#
#
#

MHD_SNAPSHOT_DIR = $$(MHD_SRCDIR)/field_data
SRC_SPH_SNAPSHOT = $(shell ls *.f90)
MOD_SPH_SNAPSHOT = $(addsuffix .o,$(basename $(SRC_SPH_SNAPSHOT)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_SNAPSHOT_DIR = $(MHD_SNAPSHOT_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_SPH_SNAPSHOT)' \
	>> $(MAKENAME)

mod_list:
	@echo  MOD_SPH_SNAPSHOT=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAPSHOT) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MHD_SNAPSHOT_DIR)' $(SRC_SPH_SNAPSHOT)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
