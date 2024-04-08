#
#
#

MAP_RENDERING_DIR = $$(VIZ_SRCDIR)/map_rendering
SOURCES = $(shell ls *.f90)
MOD_MAP = $(addsuffix .o,$(basename $(SOURCES)) )

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
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_MAP)' \
	>> $(MAKENAME)


mod_list:
	@echo  MOD_MAP=  \\ >> $(MAKENAME)
	@echo  $(MOD_MAP) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MAP_RENDERING_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
