#
#
#

IO_DIR = $$(CORE_SRCDIR)/IO
SOURCES = $(shell ls *.f90 *.F90)
MOD_IO = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'IO_DIR = $(IO_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_IO)' >> $(MAKENAME)

mod_list:
	@echo  MOD_IO=  \\ >> $(MAKENAME)
	@echo  $(MOD_IO)   >> $(MAKENAME)

module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(IO_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
