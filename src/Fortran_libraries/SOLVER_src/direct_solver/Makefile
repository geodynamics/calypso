#
#
#

SOLVER_LU_DIR = $$(SOLVER_SRCDIR)/direct_solver
SRC_SOLVER_LU = $(shell ls *.f90)
MOD_SOLVER_LU = $(addsuffix .o,$(basename $(SRC_SOLVER_LU)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_LU_DIR =  $(SOLVER_LU_DIR)'       >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_SOLVER_LU)' >> $(MAKENAME)


mod_list:
	@echo MOD_SOLVER_LU= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVER_LU) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SOLVER_LU_DIR)' $(SRC_SOLVER_LU)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
