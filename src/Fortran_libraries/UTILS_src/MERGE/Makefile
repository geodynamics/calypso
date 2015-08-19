#
#
#

MERGE_DIR = $$(UTILS_SRCDIR)/MERGE
SRC_MERGE = $(shell ls *.f90 *.F90)
MOD_MERGE = $(addsuffix .o,$(basename $(SRC_MERGE)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MERGE_DIR = $(MERGE_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: libtarget lib_archve
	@echo '	''$$(RANLIB)' '$$@' >> $(MAKENAME)

libtarget:

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_MERGE)' >> $(MAKENAME)

mod_list:
	@echo  MOD_MERGE=  \\ >> $(MAKENAME)
	@echo  $(MOD_MERGE) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MERGE_DIR)' $(SRC_MERGE)

