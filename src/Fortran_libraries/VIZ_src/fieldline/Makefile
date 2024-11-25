#
#
#

FIELDLINE_DIR = $$(VIZ_SRCDIR)/fieldline
SOURCES = $(shell ls *.f90 *.F90)
MOD_FIELDLINE = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  --------------------------------------------------------------------
#

dir_list:
	@echo 'FIELDLINE_DIR =   $(FIELDLINE_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_FIELDLINE)' \
	>> $(MAKENAME)



mod_list:
	@echo  MOD_FIELDLINE=  \\ >> $(MAKENAME)
	@echo  $(MOD_FIELDLINE) >> $(MAKENAME)
	@echo  >> $(MAKENAME)

module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(FIELDLINE_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
