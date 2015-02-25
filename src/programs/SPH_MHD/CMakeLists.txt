ADD_EXECUTABLE(sph_mhd main_sph_MHD_noviz.f90 analyzer_sph_MHD_noviz.f90 SPH_analyzer_MHD.f90)
TARGET_LINK_LIBRARIES(sph_mhd calypso fftpack.5d)

