#
#
#

ZLIB_IO_DIR = $$(CORE_SRCDIR)/IO_ZLIB
SRC_ZLIB_IO = $(shell ls *.f90 *.F90)
MOD_ZLIB_IO = $(addsuffix .o,$(basename $(SRC_ZLIB_IO)) )

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
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(ZLIB_IO_DIR)' $(SRC_ZLIB_IO)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
