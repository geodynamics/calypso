#
#
#

GEN_SPH_GRIDDIR = $$(PROG_DIR)/mesh_utilities

TARGET_GEN_SPH_GRID =    utilities/gen_sph_grids
TARGET_CHECK_SPH_GRID =  tests/check_sph_grids
TARGET_TEST_SPH_GRID =   tests/check_sph_comms


SOURCES = $(shell ls *.f90)

MOD_GEN_SPH_GRID = \
input_control_const_shell.o \
main_generate_sph_grids.o \
analyzer_gen_sph_grids.o

MOD_CHECK_SPH_GRID = \
input_control_const_shell.o \
main_check_sph_grids.o \
analyzer_check_sph_grids.o

MOD_TEST_SPH_GRID = \
cmp_trans_sph_indices.o \
cmp_trans_sph_tests.o \
input_control_const_shell.o \
\
analyzer_test_sph.o \
main_test_sph.o
#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'GEN_SPH_GRIDDIR = $(GEN_SPH_GRIDDIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_GEN_SPH_GRID =      $$(BUILDDIR)/$(TARGET_GEN_SPH_GRID)'   \
	>> $(MAKENAME)
	@echo 'TARGET_CHECK_SPH_GRID =   $$(BUILDDIR)/$(TARGET_CHECK_SPH_GRID)'   \
	>> $(MAKENAME)
	@echo 'TARGET_TEST_SPH_GRID = $$(BUILDDIR)/$(TARGET_TEST_SPH_GRID)'     \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo mesh_utils:   \
	'$$(TARGET_TEST_SPH_GRID) $$(TARGET_CHECK_SPH_GRID)' \
	'$$(TARGET_GEN_SPH_GRID)'   >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_GEN_SPH_GRID)': '$$(MOD_GEN_SPH_GRID)' \
	'$$(LIB_CALYPSO_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_GEN_SPH_GRID)' \
	'$$(MOD_GEN_SPH_GRID)' \
	'-L. $$(LIB_CALYPSO)  $$(LIB_CALYPSO_C)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CHECK_SPH_GRID)': '$$(MOD_CHECK_SPH_GRID)' \
	'$$(LIB_CALYPSO_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_CHECK_SPH_GRID)' \
	'$$(MOD_CHECK_SPH_GRID)' \
	'-L. $$(LIB_CALYPSO)  $$(LIB_CALYPSO_C)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_TEST_SPH_GRID):' \
	'$$(MOD_TEST_SPH_GRID) $$(LIB_CALYPSO_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_TEST_SPH_GRID)' \
	'$$(MOD_TEST_SPH_GRID)' \
	'-L. $$(LIB_CALYPSO)  $$(LIB_CALYPSO_C)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_GEN_SPH_GRID=  \\ >> $(MAKENAME)
	@echo  $(MOD_GEN_SPH_GRID) >> $(MAKENAME)
	@echo  MOD_CHECK_SPH_GRID=  \\ >> $(MAKENAME)
	@echo  $(MOD_CHECK_SPH_GRID) >> $(MAKENAME)
	@echo  MOD_TEST_SPH_GRID=  \\ >> $(MAKENAME)
	@echo  $(MOD_TEST_SPH_GRID) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(GEN_SPH_GRIDDIR)' $(SOURCES)
