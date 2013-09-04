#
#
#

MESHDIR = $$(UTILS_SRCDIR)/MESH
SRC_MESH = $(shell ls *.f90)
MOD_MESH = $(addsuffix .o,$(basename $(SRC_MESH)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MESHDIR = $(MESHDIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_MESH)' >> $(MAKENAME)


mod_list:
	@echo  MOD_MESH=  \\ >> $(MAKENAME)
	@echo  $(MOD_MESH) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MESHDIR)' $(SRC_MESH)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
