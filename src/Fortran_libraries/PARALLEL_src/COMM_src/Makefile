#
#
#

COMMDIR = $$(PARA_SRCDIR)/COMM_src
SOURCES = $(shell ls *.f90)

MOD_COMM = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_COMM'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'COMMDIR = $(COMMDIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_COMM)'    >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(COMMDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
