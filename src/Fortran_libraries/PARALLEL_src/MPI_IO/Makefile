#
#
#

MPI_IO_DIR = $$(PARA_SRCDIR)/MPI_IO
SOURCES = $(shell ls *.f90 *.F90)
MOD_MPI_IO = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MPI_IO_DIR = $(MPI_IO_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_MPI_IO)' >> $(MAKENAME)


mod_list:
	@echo  MOD_MPI_IO=  \\ >> $(MAKENAME)
	@echo  $(MOD_MPI_IO) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MPI_IO_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
