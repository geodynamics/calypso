#
#
#

PVR_DIR = $$(VIZ_SRCDIR)/volume_rendering
SOURCES = $(shell ls *.f90 *.F90)

MOD_PVR = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_PVR'
UPPER_MODULES=

#
#  --------------------------------------------------------------------
#

dir_list:
	@echo 'PVR_DIR = $(PVR_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)



mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  $(MOD_PVR) >> $(MAKENAME)
	@echo  '#'>> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(PVR_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
