#
#
#

IO_DIR = $$(CORE_SRCDIR)/IO
SRC_IO = $(shell ls *.f90 *.F90)
MOD_IO = $(addsuffix .o,$(basename $(SRC_IO)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'IO_DIR = $(IO_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_IO)' >> $(MAKENAME)

mod_list:
	@echo  MOD_IO=  \\ >> $(MAKENAME)
	@echo  $(MOD_IO)   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(IO_DIR)' $(SRC_IO)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
