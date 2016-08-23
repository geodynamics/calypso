GET_MPI_COUNT("control_MHD")
GET_OPENMP_COUNT("control_MHD")

configure_file(control_MHD ${CMAKE_BINARY_DIR}/tests/Dynamobench_case2/control_MHD COPYONLY)
configure_file(control_sph_shell ${CMAKE_BINARY_DIR}/tests/Dynamobench_case2/control_sph_shell COPYONLY)

file(COPY field DESTINATION ${CMAKE_BINARY_DIR}/tests/Dynamobench_case2)
file(COPY rst_4 DESTINATION ${CMAKE_BINARY_DIR}/tests/Dynamobench_case2)
file(COPY sph_lm31r48c_ic_4 DESTINATION ${CMAKE_BINARY_DIR}/tests/Dynamobench_case2)
file(COPY reference DESTINATION ${CMAKE_BINARY_DIR}/tests/Dynamobench_case2)

add_test (NAME "Dynamobench_case2_gen_sph_grids" 
          WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/tests/Dynamobench_case2
          COMMAND mpirun -np ${_mpi_count} ${CMAKE_BINARY_DIR}/bin/gen_sph_grids)

add_test (NAME "Dynamobench_case2_sph_mhd" 
          WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/tests/Dynamobench_case2  
          COMMAND mpirun -np ${_mpi_count} ${CMAKE_BINARY_DIR}/bin/sph_mhd)

add_test (NAME "Dynamobench_case2_sph_ene_check"
          WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/tests/Dynamobench_case2  
          COMMAND ${CMAKE_BINARY_DIR}/bin/sph_ene_check)
