# The name of our project is "CALYPSO". CMakeLists files in this project can 
# refer to the root source directory of the project as ${CALYPSO_SOURCE_DIR} and 
# to the root binary directory of the project as ${CALYPSO_BINARY_DIR}. 
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(CALYPSO Fortran)
SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/src/cmake/Modules/")
ENABLE_LANGUAGE(Fortran)
ENABLE_LANGUAGE(CXX)

SET(CMAKE_Fortran_MODULE_DIRECTORY "${CALYPSO_BINARY_DIR}/include")
SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CALYPSO_BINARY_DIR}/bin")
SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CALYPSO_BINARY_DIR}/lib")

# Use the preprocessor to handle #ifdefs
ADD_DEFINITIONS(-cpp)

# Look for MPI and add the required flags if found
FIND_PACKAGE(MPI)
ADD_DEFINITIONS(${MPI_Fortran_COMPILE_FLAGS})
INCLUDE_DIRECTORIES(${MPI_Fortran_INCLUDE_PATH})

# Look for OpenMP and use it if available
FIND_PACKAGE(OpenMP_Fortran)
SET (CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS})

# Look for FFTW3 and use it if available
FIND_PACKAGE(FFTW)
IF (FFTW_FOUND)
    ADD_DEFINITIONS(-DFFTW3)
    INCLUDE_DIRECTORIES(${FFTW_INCLUDES})
ENDIF(FFTW_FOUND)

# Find HDF5 - if it's not parallel then don't use it at all
FIND_PACKAGE(HDF5 COMPONENTS Fortran)
IF (HDF5_FOUND AND HDF5_IS_PARALLEL)
    ADD_DEFINITIONS(-DHDF5_IO)
    ADD_DEFINITIONS(${HDF5_DEFINITIONS})
    INCLUDE_DIRECTORIES(${HDF5_INCLUDE_DIRS})
ENDIF(HDF5_FOUND AND HDF5_IS_PARALLEL)

# Searching for Cuda
FIND_PACKAGE(CUDA)
IF (CUDA_FOUND)
  #Adding flags to fortran compiler
  #Debugging Purposes
  ADD_DEFINITIONS(-DCUDA)
  ADD_DEFINITIONS(-DCUDA_DEBUG)
  INCLUDE_DIRECTORIES(${CUDA_INCLUDE_DIRS})
  #These environment variables are specific to the supercomputers accessed
  # through xsede.
  #SET (CUDA_LIBRARY_FLAGS "-I${CUDA_INCLUDE_DIRS};-L${CUDA_TOOLKIT_ROOT_DIR}/lib64;-lcudart;-lcublas;-lcuda;-lcublas_device;-lcudadevrt")
  SET (CUDA_LIBRARY_FLAGS "-L${CUDA_TOOLKIT_ROOT_DIR}/lib64;-lcudart;-lcuda;-lcudadevrt;-lcublas;-lcublas_device;-lcurand")
ENDIF(CUDA_FOUND)

#Need to set this s.t when user specifies Degug mode, these flags are set
set (CMAKE_Fortran_FLAGS_DEBUG "-O0 -g")

# Recurse into the "src" subdirectory
ADD_SUBDIRECTORY (src) 

