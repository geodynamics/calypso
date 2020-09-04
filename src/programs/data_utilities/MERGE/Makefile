#
#
#

MERGE_SRCDIR =     $$(DATA_UTILS_DIR)/MERGE

TARGET_NEWSPH =       assemble_sph
TARGET_CMP_SPH_RST =  tests/compare_sph_restart

SOURCES = $(shell ls *.f90)

MOD_NEWSPH = \
main_assemble_sph.o \
analyzer_assemble_sph.o

MOD_CMP_SPH_RST = \
main_compare_sph_rst.o \
analyzer_compare_sph_rst.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MERGE_SRCDIR =  $(MERGE_SRCDIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_NEWSPH =      $$(BUILDDIR)/$(TARGET_NEWSPH)' >> $(MAKENAME)
	@echo 'TARGET_CMP_SPH_RST = $$(BUILDDIR)/$(TARGET_CMP_SPH_RST)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo assembles: \
	'$$(TARGET_NEWSPH)  $$(TARGET_CMP_SPH_RST)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_NEWSPH)': '$$(MOD_NEWSPH)' \
	'$$(LIB_CALYPSO_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_NEWSPH)' '$$(MOD_NEWSPH)' \
	'-L. $$(LIB_CALYPSO) $$(LIB_CALYPSO_C)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CMP_SPH_RST)': '$$(MOD_CMP_SPH_RST)' \
	'$$(LIB_CALYPSO_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_CMP_SPH_RST)' '$$(MOD_CMP_SPH_RST)' \
	'-L. $$(LIB_CALYPSO) $$(LIB_CALYPSO_C)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_NEWSPH=  \\      >> $(MAKENAME)
	@echo  $(MOD_NEWSPH)        >> $(MAKENAME)
	@echo  MOD_CMP_SPH_RST=  \\ >> $(MAKENAME)
	@echo  $(MOD_CMP_SPH_RST)   >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MERGE_SRCDIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
