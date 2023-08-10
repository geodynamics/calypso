#
#
#

SOLVER_LU_DIR = $$(SOLVER_SRCDIR)/direct_solver
SOURCES = $(shell ls *.f90)
MOD_SOLVER_LU = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVER_LU_DIR =  $(SOLVER_LU_DIR)'       >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_SOLVER_LU)' >> $(MAKENAME)


mod_list:
	@echo MOD_SOLVER_LU= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVER_LU) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SOLVER_LU_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
