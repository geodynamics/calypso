#
#
#

ZLIB_IO_DIR = $$(CORE_SRCDIR)/IO_ZLIB
SOURCES = $(shell ls *.f90 *.F90)

MOD_ZLIB_IO = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_ZLIB_IO'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'ZLIB_IO_DIR = $(ZLIB_IO_DIR)'  >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_ZLIB_IO)'       >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(ZLIB_IO_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
