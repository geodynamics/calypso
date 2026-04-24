#
#
#

SPH_SPECTR_DIR = $$(CORE_SRCDIR)/SPH_SPECTR_src
SOURCES = $(shell ls *.f90)
MOD_SPH_SPECTR = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_SPECTR_DIR = $(SPH_SPECTR_DIR)'  >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_SPH_SPECTR)' >> $(MAKENAME)

mod_list:
	@echo  MOD_SPH_SPECTR=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SPECTR) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(SPH_SPECTR_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
