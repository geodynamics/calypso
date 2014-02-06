#
#
#

INIT_SPECTR_DIR =   $$(DATA_UTILS_DIR)/INITIAL_FIELD

TARGET_SPH_INITIAL =   sph_initial_field
TARGET_SPH_ADD_INIT =  sph_add_initial_field

SOURCES = $(shell ls *.f90)

MOD_SPH_INITIAL = \
main_sph_initial_fld.o \
SPH_analyzer_const_initial.o \
const_sph_initial_spectr.o

MOD_SPH_ADD_INIT = \
main_sph_add_initial_fld.o \
SPH_analyzer_add_initial.o \
const_sph_initial_spectr.o


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'INIT_SPECTR_DIR = $(INIT_SPECTR_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_SPH_INITIAL = $$(BUILDDIR)/$(TARGET_SPH_INITIAL)'>> $(MAKENAME)
	@echo 'TARGET_SPH_ADD_INIT =   $$(BUILDDIR)/$(TARGET_SPH_ADD_INIT)'  >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo sph_initial: \
	 '$$(TARGET_SPH_INITIAL)' \
	 '$$(TARGET_SPH_ADD_INIT)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_INITIAL):  $$(MOD_SPH_INITIAL)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_INITIAL)' '$$(MOD_SPH_INITIAL)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_SPH_ADD_INIT):  $$(MOD_SPH_ADD_INIT)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_ADD_INIT)' '$$(MOD_SPH_ADD_INIT)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_SPH_INITIAL=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_INITIAL) >> $(MAKENAME)
	@echo  MOD_SPH_ADD_INIT=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_ADD_INIT) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(INIT_SPECTR_DIR)' $(SOURCES)

clean:
	rm -f *.o *~ *.mod *.diag *.L
