#
#
#

PSF_SECTION_DIR = $$(VIZ_SRCDIR)/surfacing
SOURCES = $(shell ls *.f90)
MOD_SURFACING = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  --------------------------------------------------------------------
#

dir_list:
	@echo 'PSF_SECTION_DIR = $(PSF_SECTION_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_SURFACING)' \
	>> $(MAKENAME)


mod_list:
	@echo  MOD_SURFACING=  \\ >> $(MAKENAME)
	@echo  $(MOD_SURFACING) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(PSF_SECTION_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
