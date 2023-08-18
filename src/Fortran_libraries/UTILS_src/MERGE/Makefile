#
#
#

MERGE_DIR = $$(UTILS_SRCDIR)/MERGE
SOURCES = $(shell ls *.f90 *.F90)
MOD_MERGE = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MERGE_DIR = $(MERGE_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: lib_archve
	@echo '	''$$(RANLIB)' '$$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_MERGE)' >> $(MAKENAME)

mod_list:
	@echo  MOD_MERGE=  \\ >> $(MAKENAME)
	@echo  $(MOD_MERGE) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MERGE_DIR)' $(SOURCES)

