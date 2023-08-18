#
#
#

SPH_GRID_SRCDIR = $$(PARA_SRCDIR)/CONST_SPH_GRID
SOURCES = $(shell ls *.f90)
MOD_SPH_GRID = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_GRID_SRCDIR = $(SPH_GRID_SRCDIR)'   >> $(MAKENAME)

lib_name:

lib_tasks: lib_archve

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_SPH_GRID)' \
	>> $(MAKENAME)


mod_list:
	@echo  MOD_SPH_GRID=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_GRID)   >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SPH_GRID_SRCDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod $(EXE) $(LIB) *~

