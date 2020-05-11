#
#
#

RAYLEIGH_LINK_DIR = $$(DATA_UTILS_DIR)/Rayleigh_link

TARGET_CVT_RAYLEIGH_SPH =   convert_rayleigh_sph


SOURCES = $(shell ls *.f90)

MOD_CVT_RAYLEIGH_SPH = \
main_cvt_from_rayleigh_sph.o \
analyzer_rayleigh_cvt_sph.o \
\
t_convert_from_rayleigh.o \
t_rayleigh_restart_IO.o \
rayleigh_restart_IO.o \
rayleigh99_rst_param_IO.o \
convert_from_rayleigh_rst.o


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'RAYLEIGH_LINK_DIR = $(RAYLEIGH_LINK_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_CVT_RAYLEIGH_SPH = $$(BUILDDIR)/$(TARGET_CVT_RAYLEIGH_SPH)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo rayleigh_link: \
	'$$(TARGET_CVT_RAYLEIGH_SPH)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CVT_RAYLEIGH_SPH)': '$$(MOD_CVT_RAYLEIGH_SPH)' \
	'$$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_CVT_RAYLEIGH_SPH)' '$$(MOD_CVT_RAYLEIGH_SPH)' \
	'-L. $$(LIBS_SPH_MHD)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)



lib_name:

mod_list:
	@echo  MOD_CVT_RAYLEIGH_SPH=  \\ >> $(MAKENAME)
	@echo  $(MOD_CVT_RAYLEIGH_SPH)   >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(RAYLEIGH_LINK_DIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
