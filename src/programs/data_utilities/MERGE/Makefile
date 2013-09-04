#
#
#

MERGE_SRCDIR =     $$(DATA_UTILS_DIR)/MERGE

TARGET_NEWSPH = assemble_sph

LIBS_MERGE = \
'$$(LIB_CALYPSO)'

LIB_FILES_MERGE = \
'$$(LIB_CALYPSO_FILE)'

SOURCES = $(shell ls *.f90)

MOD_NEWSPH = \
assemble_newsph.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MERGE_SRCDIR =  $(MERGE_SRCDIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_NEWSPH = $$(BUILDDIR)/$(TARGET_NEWSPH)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo assembles: \
	'$$(TARGET_NEWSPH)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_NEWSPH)': '$$(MOD_NEWSPH)' \
	'$$(LIB_FILES_MERGE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_NEWSPH)' '$$(MOD_NEWSPH)' \
	'-L. $$(LIBS_MERGE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:
	@echo  LIBS_MERGE=  \\          >> $(MAKENAME)
	@echo  $(LIBS_MERGE)            >> $(MAKENAME)
	@echo  LIB_FILES_MERGE=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_MERGE)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_NEWSPH=  \\     >> $(MAKENAME)
	@echo  $(MOD_NEWSPH)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MERGE_SRCDIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
