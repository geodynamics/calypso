#
#
#

SOLVER_LU_DIR = $$(SOLVER_SRCDIR)/direct_solver
SOURCES = $(shell ls *.f90)

MOD_SOLVER_LU = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST= 'MOD_SOLVER_LU'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_LU_DIR =  $(SOLVER_LU_DIR)'       >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$('$(MODULE_LIST)')' >> $(MAKENAME)


mod_list:
	@echo '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo '$(MOD_SOLVER_LU)'     >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(SOLVER_LU_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
