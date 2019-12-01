#
#
#

MHD_SNAPSHOT_DIR = $$(MHD_SRCDIR)/field_data
SOURCES = $(shell ls *.f90)
MOD_SPH_SNAPSHOT = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_SNAPSHOT_DIR = $(MHD_SNAPSHOT_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_SPH_SNAPSHOT)' \
	>> $(MAKENAME)

mod_list:
	@echo  MOD_SPH_SNAPSHOT=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_SNAPSHOT) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MHD_SNAPSHOT_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
