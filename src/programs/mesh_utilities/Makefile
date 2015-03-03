#
#
#

GEN_SPH_GRIDDIR = $$(PROG_DIR)/mesh_utilities

TARGET_GEN_SPH =       gen_sph_grids
TARGET_TEST_SPH_GRID = check_sph_grids


SOURCES = $(shell ls *.f90)

MOD_GEN_SPH_GRID = \
main_generate_sph_grids.o \
analyzer_gen_sph_grids.o

MOD_TEST_SPH_GRID = \
m_read_ctl_gen_sph_shell.o \
cmp_trans_sph_indices.o \
cmp_trans_sph_tests.o \
\
analyzer_test_sph.o \
main_test_sph.o
#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'GEN_SPH_GRIDDIR = $(GEN_SPH_GRIDDIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_GEN_SPH =      $$(BUILDDIR)/$(TARGET_GEN_SPH)'   \
	>> $(MAKENAME)
	@echo 'TARGET_TEST_SPH_GRID = $$(BUILDDIR)/$(TARGET_TEST_SPH_GRID)'     \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo mesh_utils:   \
	'$$(TARGET_TEST_SPH_GRID)' \
	'$$(TARGET_GEN_SPH)'   >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_GEN_SPH)': '$$(MOD_GEN_SPH_GRID)' \
	'$$(LIB_CALYPSO_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_GEN_SPH)' \
	'$$(MOD_GEN_SPH_GRID)' \
	'-L. $$(LIB_CALYPSO)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_TEST_SPH_GRID):' \
	'$$(MOD_TEST_SPH_GRID) $$(LIB_CALYPSO_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_TEST_SPH_GRID)' \
	'$$(MOD_TEST_SPH_GRID)' \
	'-L. $$(LIB_CALYPSO)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_GEN_SPH_GRID=  \\ >> $(MAKENAME)
	@echo  $(MOD_GEN_SPH_GRID) >> $(MAKENAME)
	@echo  MOD_TEST_SPH_GRID=  \\ >> $(MAKENAME)
	@echo  $(MOD_TEST_SPH_GRID) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(GEN_SPH_GRIDDIR)' $(SOURCES)
