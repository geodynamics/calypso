#
#
#

MPI_IO_DIR = $$(PARA_SRCDIR)/MPI_IO
SRC_MPI_IO = $(shell ls *.f90 *.F90)
MOD_MPI_IO = $(addsuffix .o,$(basename $(SRC_MPI_IO)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MPI_IO_DIR = $(MPI_IO_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_MPI_IO)' >> $(MAKENAME)


mod_list:
	@echo  MOD_MPI_IO=  \\ >> $(MAKENAME)
	@echo  $(MOD_MPI_IO) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MPI_IO_DIR)' $(SRC_MPI_IO)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
