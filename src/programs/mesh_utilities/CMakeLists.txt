ADD_EXECUTABLE(gen_sph_grids generate_sph_grids_nosf.f90)
TARGET_LINK_LIBRARIES(gen_sph_grids calypso)

SET(MOD_TEST_SPH_GRID cmp_trans_sph_indices.f90 cmp_trans_sph_tests.f90 analyzer_test_sph.f90 main_test_sph.f90)
ADD_EXECUTABLE(check_sph_grids ${MOD_TEST_SPH_GRID})
TARGET_LINK_LIBRARIES(check_sph_grids calypso)

