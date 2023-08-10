#
#
#

MESHDIR = $$(UTILS_SRCDIR)/MESH
SOURCES = $(shell ls *.f90)
MOD_MESH = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MESHDIR = $(MESHDIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_MESH)' >> $(MAKENAME)


mod_list:
	@echo  MOD_MESH=  \\ >> $(MAKENAME)
	@echo  $(MOD_MESH) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MESHDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
