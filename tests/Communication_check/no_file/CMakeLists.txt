GET_MPI_COUNT("control_MHD")
GET_OPENMP_COUNT("control_MHD")

configure_file(control_MHD ${CMAKE_BINARY_DIR}/tests/Dynamobench_case1/control_MHD COPYONLY)
configure_file(control_sph_shell ${CMAKE_BINARY_DIR}/tests/Dynamobench_case1/control_sph_shell COPYONLY)

file(COPY field DESTINATION ${CMAKE_BINARY_DIR}/tests/Dynamobench_case1)
file(COPY rst_4 DESTINATION ${CMAKE_BINARY_DIR}/tests/Dynamobench_case1)
file(COPY sph_lm31r48c_4 DESTINATION ${CMAKE_BINARY_DIR}/tests/Dynamobench_case1)
file(COPY reference DESTINATION ${CMAKE_BINARY_DIR}/tests/Dynamobench_case1)

add_test (NAME "Dynamobench_case1_gen_sph_grids"
          WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/tests/Dynamobench_case1
          COMMAND mpirun -np ${_mpi_count} ${CMAKE_BINARY_DIR}/bin/gen_sph_grids)

add_test (NAME "Dynamobench_case1_sph_mhd"
          WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/tests/Dynamobench_case1
          COMMAND mpirun -np ${_mpi_count} ${CMAKE_BINARY_DIR}/bin/sph_mhd)

add_test (NAME "Dynamobench_case1_sph_ene_check"
          WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/tests/Dynamobench_case1
          COMMAND ${CMAKE_BINARY_DIR}/bin/tests/sph_ene_check)
