#
#
#

VIZ_PLUG_DIR = $$(DATA_UTILS_DIR)/VIZ_only

TARGET_PSF =         utilities/sectioning
TARGET_VTK_CONVERT = utilities/field_to_VTK
TARGET_PSF_2_VTK =   utilities/section_to_vtk

SOURCES = $(shell ls *.f90)

MOD_PSF_PLUG = \
main_surface_rendering.o \
analyzer_psf.o \
FEM_analyzer_viz_surf.o \
input_control_section_only.o \
\
t_control_data_section_only.o \
t_field_list_for_vizs.o \
t_FEM_mesh_field_4_viz.o

MOD_VTK_CONVERT_PLUG = \
main_VTK_convert.o \
analyzer_VTK_convert.o \
FEM_analyzer_viz_surf.o \
input_control_section_only.o \
t_viz_VTK_convert.o \
\
t_control_data_section_only.o \
t_field_list_for_vizs.o

MOD_PSF_2_VTK = \
psf_file_to_VTK.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'VIZ_PLUG_DIR =  $(VIZ_PLUG_DIR)'         >> $(MAKENAME)

target_list:
	@echo 'TARGET_PSF =         $$(BUILDDIR)/$(TARGET_PSF)'         >> $(MAKENAME)
	@echo 'TARGET_VTK_CONVERT = $$(BUILDDIR)/$(TARGET_VTK_CONVERT)' >> $(MAKENAME)
	@echo 'TARGET_PSF_2_VTK =    $$(BUILDDIR)/$(TARGET_PSF_2_VTK)'  >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo visualizer: \
	'$$(TARGET_PSF)'         \
	'$$(TARGET_VTK_CONVERT)' \
	 '$$(TARGET_PSF_2_VTK)'  \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PSF): $$(MOD_PSF_PLUG)' \
	'$$(LIB_FILES_SPH_MHD)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_PSF)' '$$(MOD_PSF_PLUG)' \
	'-L. $$(LIBS_SPH_MHD) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_VTK_CONVERT):  $$(MOD_VTK_CONVERT_PLUG)' \
	'$$(LIB_FILES_SPH_MHD)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_VTK_CONVERT)' '$$(MOD_VTK_CONVERT_PLUG)' \
	'-L. $$(LIBS_SPH_MHD) $$(F90LIBS) $$(PNG_LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_PSF_2_VTK):  $$(MOD_PSF_2_VTK)  $$(LIB_FILES_SPH_MHD)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_PSF_2_VTK)' '$$(MOD_PSF_2_VTK)' \
	'-L. $$(LIBS_SPH_MHD)  $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:

mod_list:
	@echo  MOD_PSF_PLUG=  \\         >> $(MAKENAME)
	@echo  $(MOD_PSF_PLUG)           >> $(MAKENAME)
	@echo  MOD_VTK_CONVERT_PLUG=  \\ >> $(MAKENAME)
	@echo  $(MOD_VTK_CONVERT_PLUG)   >> $(MAKENAME)
	@echo  MOD_PSF_2_VTK=  \\        >> $(MAKENAME)
	@echo  $(MOD_PSF_2_VTK)          >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(VIZ_PLUG_DIR)' $(SOURCES)
