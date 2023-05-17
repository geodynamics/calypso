SET(MOD_GEN_SPH_GRID input_control_const_shell.f90 main_generate_sph_grids.f90 analyzer_gen_sph_grids.f90)
ADD_EXECUTABLE(gen_sph_grids ${MOD_GEN_SPH_GRID})
TARGET_LINK_LIBRARIES(gen_sph_grids calypso calypso_c)

SET(MOD_TEST_SPH_GRID cmp_trans_sph_indices.f90 cmp_trans_sph_tests.f90 input_control_const_shell.f90 analyzer_test_sph.f90 main_test_sph.f90)
ADD_EXECUTABLE(check_sph_grids ${MOD_TEST_SPH_GRID})
TARGET_LINK_LIBRARIES(check_sph_grids calypso calypso_c)

