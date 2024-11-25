#
#
#

MHD_FDM_DIR = $$(MHD_SRCDIR)/radial_FDM
SOURCES = $(shell ls *.f90)
MOD_SPH_MHD_FDM = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_FDM_DIR = $(MHD_FDM_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_SPH_MHD_FDM)' \
	>> $(MAKENAME)

mod_list:
	@echo MOD_SPH_MHD_FDM=  \\ >> $(MAKENAME)
	@echo $(MOD_SPH_MHD_FDM) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MHD_FDM_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
