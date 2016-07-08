GET_MPI_COUNT("control_MHD")
GET_OPENMP_COUNT("control_MHD")

configure_file(control_MHD ${CMAKE_BINARY_DIR}/tests/heterogineous_temp/control_MHD COPYONLY)
configure_file(control_sph_shell ${CMAKE_BINARY_DIR}/tests/heterogineous_temp/control_sph_shell COPYONLY)
configure_file(bc_spectr.btx ${CMAKE_BINARY_DIR}/tests/heterogineous_temp/bc_spectr.btx COPYONLY)

file(COPY field DESTINATION ${CMAKE_BINARY_DIR}/tests/heterogineous_temp)
file(COPY rst_2 DESTINATION ${CMAKE_BINARY_DIR}/tests/heterogineous_temp)
file(COPY sph_lm4r64c_2 DESTINATION ${CMAKE_BINARY_DIR}/tests/heterogineous_temp)
file(COPY reference DESTINATION ${CMAKE_BINARY_DIR}/tests/heterogineous_temp)

add_test (NAME "heterogineous_temp_gen_sph_grids" 
          WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/tests/heterogineous_temp  
          COMMAND mpirun -np ${_mpi_count} ${CMAKE_BINARY_DIR}/bin/gen_sph_grids)

add_test (NAME "heterogineous_temp_sph_mhd" 
          WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/tests/heterogineous_temp  
          COMMAND mpirun -np ${_mpi_count} ${CMAKE_BINARY_DIR}/bin/sph_mhd)

add_test (NAME "heterogineous_temp_sph_ene_check"
          WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/tests/heterogineous_temp
          COMMAND ${CMAKE_BINARY_DIR}/bin/sph_ene_check)
