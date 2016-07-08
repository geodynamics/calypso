FILE(GLOB ASSEMBLE_SPH_SRC_FILES MERGE/*.[fF]90)
ADD_EXECUTABLE(assemble_sph ${ASSEMBLE_SPH_SRC_FILES})
TARGET_LINK_LIBRARIES(assemble_sph calypso)

SET(MOD_SPH_SNAP_MAIN_SRC_FILES SNAPSHOT_MHD/main_sph_snapshot_w_psf.f90 SNAPSHOT_MHD/SPH_analyzer_snap.f90 SNAPSHOT_MHD/analyzer_sph_snap_w_psf.f90)
ADD_EXECUTABLE(sph_snapshot ${MOD_SPH_SNAP_MAIN_SRC_FILES})
TARGET_LINK_LIBRARIES(sph_snapshot calypso fftpack.5d)

SET(MOD_SPH_ZM_SNAP_MAIN_SRC_FILES SNAPSHOT_MHD/main_sph_zm_snap.f90 SNAPSHOT_MHD/analyzer_sph_zm_snap.f90 SNAPSHOT_MHD/SPH_analyzer_snap.f90 SNAPSHOT_MHD/SPH_analyzer_zm_snap.f90)
ADD_EXECUTABLE(sph_zm_snapshot ${MOD_SPH_ZM_SNAP_MAIN_SRC_FILES})
TARGET_LINK_LIBRARIES(sph_zm_snapshot calypso fftpack.5d)

SET(MOD_SPH_D_BENCH_MAIN_SRC_FILES SNAPSHOT_MHD/main_sph_dynamobench.f90 SNAPSHOT_MHD/analyzer_sph_dynamobench.f90 SNAPSHOT_MHD/SPH_analyzer_d_bench.f90)
ADD_EXECUTABLE(sph_dynamobench ${MOD_SPH_D_BENCH_MAIN_SRC_FILES})
TARGET_LINK_LIBRARIES(sph_dynamobench calypso fftpack.5d)

SET(MOD_SPH_INITIAL INITIAL_FIELD/main_sph_initial_fld.f90 INITIAL_FIELD/SPH_analyzer_add_initial.f90 INITIAL_FIELD/SPH_analyzer_const_initial.f90 INITIAL_FIELD/const_sph_initial_spectr.f90)
ADD_EXECUTABLE(sph_initial_field ${MOD_SPH_INITIAL})
TARGET_LINK_LIBRARIES(sph_initial_field calypso fftpack.5d)

SET(SPH_ENE_CHECK_SRC_FILES TIME_HISTORIES/compare_sph_mean_square.f90)
ADD_EXECUTABLE(sph_ene_check ${SPH_ENE_CHECK_SRC_FILES})
TARGET_LINK_LIBRARIES(sph_ene_check calypso fftpack.5d)
