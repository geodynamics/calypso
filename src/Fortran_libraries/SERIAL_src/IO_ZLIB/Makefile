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

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_ZLIB_IO)' >> $(MAKENAME)

mod_list:
	@echo  'MOD_CORE+= $$(MOD_ZLIB_IO)' >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)
	@echo  'MOD_ZLIB_IO=  '  \\ >> $(MAKENAME)
	@echo  '$(MOD_ZLIB_IO)'   >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(ZLIB_IO_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
