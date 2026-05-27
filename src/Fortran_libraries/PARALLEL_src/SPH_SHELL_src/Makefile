#
#
#

SPH_COMMDIR = $$(PARA_SRCDIR)/SPH_SHELL_src
SOURCES = $(shell ls *.f90 *.F90)

MOD_COMM_SPH = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_COMM_SPH'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_COMMDIR = $(SPH_COMMDIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)


mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_COMM_SPH)'    >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(SPH_COMMDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
