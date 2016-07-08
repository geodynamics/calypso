# A function that extracts from file control_MHD
# the number of MPI processes this test is to be invoked as.
# This is encoded in .prm files through lines of the form
#    'num_subdomain_ctl            4'
# The result is returned in a variable _mpi_count in the
# caller's scope.
FUNCTION(get_mpi_count _filename)
  FILE(STRINGS ${_filename} _input_lines
       REGEX "^ * num_subdomain_ctl")
#  MESSAGE( STATUS "Read line is: " ${_input_lines} )
  IF("${_input_lines}" STREQUAL "")
    SET(_mpi_count 1 PARENT_SCOPE)
  ELSE()
    # go over the (possibly multiple) lines with MPI markers and choose the last
    FOREACH(_input_line ${_input_lines})
     SET(_last_line ${_input_line})
    ENDFOREACH()
    STRING(REGEX REPLACE "^ *num_subdomain_ctl *([0-9]+) *$" "\\1"
           _mpi_count ${_last_line})
#    MESSAGE( STATUS "MPI_count: " ${_mpi_count} )
    SET(_mpi_count "${_mpi_count}" PARENT_SCOPE)
  ENDIF()
ENDFUNCTION()
# A function that extracts from file control_MHD
# the number of openMP threads this test is to be invoked with.
# This is encoded in control_MHD files through lines of the form
#    'num_smp_ctl    2'
# The result is returned in a variable _mpi_count in the
# caller's scope.
FUNCTION(get_openmp_count _filename)
  FILE(STRINGS ${_filename} _input_lines
       REGEX "^ * num_smp_ctl")
#  MESSAGE( STATUS "Read line is: " ${_input_lines} )
  IF("${_input_lines}" STREQUAL "")
    SET(_mpi_count 1 PARENT_SCOPE)
  ELSE()
    # go over the (possibly multiple) lines with MPI markers and choose the last
    FOREACH(_input_line ${_input_lines})
     SET(_last_line ${_input_line})
    ENDFOREACH()
    STRING(REGEX REPLACE "^ *num_smp_ctl *([0-9]+) *$" "\\1"
           _openmp_count ${_last_line})
#    MESSAGE( STATUS "MPI_count: " ${_openmp_count} )
    SET(_mpi_count "${_mpi_count}" PARENT_SCOPE)
  ENDIF()
ENDFUNCTION()

ADD_SUBDIRECTORY(Dynamobench_case1)
ADD_SUBDIRECTORY(Dynamobench_case2)
ADD_SUBDIRECTORY(heterogineous_temp)
