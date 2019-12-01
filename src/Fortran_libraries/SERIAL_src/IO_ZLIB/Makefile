#
#
#

ZLIB_IO_DIR = $$(CORE_SRCDIR)/IO_ZLIB
SOURCES = $(shell ls *.f90 *.F90)
MOD_ZLIB_IO = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'ZLIB_IO_DIR = $(ZLIB_IO_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_ZLIB_IO)' >> $(MAKENAME)

mod_list:
	@echo  MOD_ZLIB_IO=  \\ >> $(MAKENAME)
	@echo  $(MOD_ZLIB_IO)   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(ZLIB_IO_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
