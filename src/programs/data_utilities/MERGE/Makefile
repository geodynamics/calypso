#
#
#

MERGE_SRCDIR =     $$(DATA_UTILS_DIR)/MERGE

TARGET_NEWSPH = assemble_sph

SOURCES = $(shell ls *.f90)

MOD_NEWSPH = \
main_assemble_sph.o \
analyzer_assemble_sph.o

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
	'$$(LIB_CALYPSO_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_NEWSPH)' '$$(MOD_NEWSPH)' \
	'-L. $$(LIB_CALYPSO) $$(LIB_CALYPSO_C)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_NEWSPH=  \\     >> $(MAKENAME)
	@echo  $(MOD_NEWSPH)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MERGE_SRCDIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
