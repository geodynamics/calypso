#
#
#

SPH_FUJITSU_TEST =   $$(PROG_DIR)/Fujitsu_test

TARGET_FUJITSU_TEST =   test_4_fujitsu

SOURCES = $(shell ls *.f90)

MOD_FUJITSU_TEST =  \
main_fujitsu_test.o \
analyzer_fujitsu_test.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_FUJITSU_TEST = $(SPH_FUJITSU_TEST)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_FUJITSU_TEST =   $$(BUILDDIR)/$(TARGET_FUJITSU_TEST)'  >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo fujitsu: \
	 '$$(TARGET_FUJITSU_TEST)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FUJITSU_TEST): $$(MOD_FUJITSU_TEST) $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FUJITSU_TEST)' '$$(MOD_FUJITSU_TEST)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_FUJITSU_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_FUJITSU_TEST) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SPH_FUJITSU_TEST)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
