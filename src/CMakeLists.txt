# Recurse into the subdirectories
IF(CUDA_FOUND)
  ADD_SUBDIRECTORY (Cuda_libraries)
ENDIF(CUDA_FOUND)
#So that it is a global variable
FILE(GLOB FORTRAN_CUDA_INTERFACE Cuda_libraries/*.f90)
SET(CUDA_FORTRAN_SOURCE_CODE ${FORTRAN_CUDA_INTERFACE})

ADD_SUBDIRECTORY (External_libs)
ADD_SUBDIRECTORY (Fortran_libraries)
ADD_SUBDIRECTORY (programs)

