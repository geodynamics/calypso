#
#
#

COMMDIR = $$(PARA_SRCDIR)/COMM_src
SRC_COMM = $(shell ls *.f90 *.F90)
MOD_COMM = $(addsuffix .o,$(basename $(SRC_COMM)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'COMMDIR = $(COMMDIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_COMM)' >> $(MAKENAME)


mod_list:
	@echo  MOD_COMM=  \\ >> $(MAKENAME)
	@echo  $(MOD_COMM) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(COMMDIR)' $(SRC_COMM)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
