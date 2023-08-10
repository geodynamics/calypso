#
#
#

MPI_ZLIB_IO_DIR = $$(PARA_SRCDIR)/MPI_ZLIB_IO
SOURCES = $(shell ls *.f90)
MOD_MPI_ZLIB_IO = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MPI_ZLIB_IO_DIR = $(MPI_ZLIB_IO_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_MPI_ZLIB_IO)' >> $(MAKENAME)


mod_list:
	@echo  MOD_MPI_ZLIB_IO=  \\ >> $(MAKENAME)
	@echo  $(MOD_MPI_ZLIB_IO) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MPI_ZLIB_IO_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
