#
#
#

MPI_ZLIB_IO_DIR = $$(PARA_SRCDIR)/MPI_ZLIB_IO
SOURCES = $(shell ls *.f90)

MOD_MPI_ZLIB_IO = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_MPI_ZLIB_IO'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MPI_ZLIB_IO_DIR = $(MPI_ZLIB_IO_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)


mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_MPI_ZLIB_IO)'    >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(MPI_ZLIB_IO_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
