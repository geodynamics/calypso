#
#
#

F2003DIR = $$(CORE_SRCDIR)/Fortran2003
SOURCES = $(shell ls *.f90 *.F90)

MOD_F2003 = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_F2003'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'F2003DIR = $(F2003DIR)'  >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)


mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_F2003)'         >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(F2003DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
