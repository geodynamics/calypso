#
#
#

SPH_GRID_SRCDIR = $$(UTILS_SRCDIR)/spherical_grid
SRC_SPH_GRID = $(shell ls *.f90)
MOD_SPH_GRID = $(addsuffix .o,$(basename $(SRC_SPH_GRID)) )

#
#  -------------------------------------------------------------------------
#

.f90.o:; $(F90) -c $(F90FLAGS) $<

dir_list:
	@echo 'SPH_GRID_SRCDIR = $(SPH_GRID_SRCDIR)'   >> $(MAKENAME)

lib_name:

lib_tasks: libtarget lib_archve

libtarget:

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_SPH_GRID)' \
	>> $(MAKENAME)


mod_list:
	@echo  MOD_SPH_GRID=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_GRID)   >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SPH_GRID_SRCDIR)' $(SRC_SPH_GRID)

clean:
	rm -f *.o *.mod $(EXE) $(LIB) *~

