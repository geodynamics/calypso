#
#
#

F2003DIR = $$(CORE_SRCDIR)/Fortran2003
SOURCES = $(shell ls *.f90 *.F90)
MOD_F2003 = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'F2003DIR = $(F2003DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_F2003)' >> $(MAKENAME)


mod_list:
	@echo  MOD_F2003=  \\ >> $(MAKENAME)
	@echo  $(MOD_F2003) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(F2003DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
