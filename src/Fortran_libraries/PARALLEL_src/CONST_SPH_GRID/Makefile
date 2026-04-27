#
#
#

SPH_GRID_SRCDIR = $$(PARA_SRCDIR)/CONST_SPH_GRID
SOURCES = $(shell ls *.f90)

MOD_SPH_GRID = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_SPH_GRID'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_GRID_SRCDIR = $(SPH_GRID_SRCDIR)'   >> $(MAKENAME)

lib_name:

lib_tasks: lib_archve

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)


mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_SPH_GRID)'    >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(SPH_GRID_SRCDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod $(EXE) $(LIB) *~

