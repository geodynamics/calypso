#
#
#

SPH_DIR = $$(CORE_SRCDIR)/spherical_harmonics
SOURCES = $(shell ls *.f90 *.F90)
MOD_SPH = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_DIR = $(SPH_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_SPH)' >> $(MAKENAME)

mod_list:
	@echo MOD_SPH=  \\ >> $(MAKENAME)
	@echo $(MOD_SPH) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SPH_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
