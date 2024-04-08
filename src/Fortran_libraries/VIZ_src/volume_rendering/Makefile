#
#
#

PVR_DIR = $$(VIZ_SRCDIR)/volume_rendering
SOURCES = $(shell ls *.f90 *.F90)
MOD_PVR = $(addsuffix .o,$(basename $(SOURCES)) )

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
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_PVR)' \
	>> $(MAKENAME)



mod_list:
	@echo  MOD_PVR=  \\ >> $(MAKENAME)
	@echo  $(MOD_PVR) >> $(MAKENAME)
	@echo  >> $(MAKENAME)

module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(PVR_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
