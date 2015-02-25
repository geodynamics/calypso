#
#
#

PSF_SECTION_DIR = $$(VIZ_SRCDIR)/surfacing
SRC_SURFACING = $(shell ls *.f90)
MOD_SURFACING = $(addsuffix .o,$(basename $(SRC_SURFACING)) )

#
#  --------------------------------------------------------------------
#

dir_list:
	@echo 'PSF_SECTION_DIR = $(PSF_SECTION_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_SURFACING)' \
	>> $(MAKENAME)


mod_list:
	@echo  MOD_SURFACING=  \\ >> $(MAKENAME)
	@echo  $(MOD_SURFACING) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(PSF_SECTION_DIR)' $(SRC_SURFACING)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
