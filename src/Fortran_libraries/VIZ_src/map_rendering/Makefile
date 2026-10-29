#
#
#

MAP_RENDERING_DIR = $$(VIZ_SRCDIR)/map_rendering
SOURCES = $(shell ls *.f90)

MOD_MAP = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_MAP'
UPPER_MODULES=

#
#  --------------------------------------------------------------------
#

dir_list:
	@echo 'MAP_RENDERING_DIR = $(MAP_RENDERING_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_MAP)'           >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(MAP_RENDERING_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
