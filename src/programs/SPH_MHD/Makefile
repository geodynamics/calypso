#
#
#

SPH_MHD_MAINDIR =   $$(PROG_DIR)/SPH_MHD

TARGET_SPH_MHD_VIZS =   sph_mhd_vizs
TARGET_SPH_MHD =        sph_mhd

SOURCES = $(shell ls *.f90)

MOD_SPH_MHD_VIZS =  \
main_sph_MHD_w_vizs.o \
analyzer_sph_MHD_w_vizs.o \
SPH_analyzer_MHD.o

MOD_SPH_MHD_MAIN =  \
main_sph_MHD_w_psf.o \
analyzer_sph_MHD_w_psf.o \
SPH_analyzer_MHD.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'SPH_MHD_MAINDIR = $(SPH_MHD_MAINDIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_SPH_MHD =      $$(BUILDDIR)/$(TARGET_SPH_MHD)'  >> $(MAKENAME)
	@echo 'TARGET_SPH_MHD_VIZS = $$(BUILDDIR)/$(TARGET_SPH_MHD_VIZS)'  >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo sph_mhd: \
	 '$$(TARGET_SPH_MHD) $$(TARGET_SPH_MHD_VIZS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_MHD): $$(MOD_SPH_MHD_MAIN) $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_MHD)' '$$(MOD_SPH_MHD_MAIN)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_MHD_VIZS): $$(MOD_SPH_MHD_VIZS) $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_MHD_VIZS)' '$$(MOD_SPH_MHD_VIZS)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_SPH_MHD_MAIN=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_MHD_MAIN) >> $(MAKENAME)
	@echo  MOD_SPH_MHD_VIZS=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_MHD_VIZS) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SPH_MHD_MAINDIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
