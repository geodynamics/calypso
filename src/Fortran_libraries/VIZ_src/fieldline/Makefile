#
#
#

FIELDLINE_DIR = $$(VIZ_SRCDIR)/fieldline
SOURCES = $(shell ls *.f90 *.F90)

MOD_FIELDLINE = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_FIELDLINE'
UPPER_MODULES=

#
#  --------------------------------------------------------------------
#

dir_list:
	@echo 'FIELDLINE_DIR =   $(FIELDLINE_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_FIELDLINE)'     >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FIELDLINE_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
