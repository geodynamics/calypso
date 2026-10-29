#
#
#

MESHDIR = $$(UTILS_SRCDIR)/MESH
SOURCES = $(shell ls *.f90)

MOD_MESH = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_MESH'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MESHDIR = $(MESHDIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)


mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)= '  \\ >> $(MAKENAME)
	@echo  '$(MOD_MESH)'          >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(MESHDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
