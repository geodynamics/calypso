#
#
#

JACOBIDIR = $$(UTILS_SRCDIR)/jacobian

SOURCES = $(shell ls *.f90)
MOD_JACOBI = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_JACOBI'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'JACOBIDIR = $(JACOBIDIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$(MOD_JACOBI)' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)= ' \\ >> $(MAKENAME)
	@echo  '$(MOD_JACOBI)'       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(JACOBIDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
