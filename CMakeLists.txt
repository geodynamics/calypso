# The name of our project is "CALYPSO". CMakeLists files in this project can 
# refer to the root source directory of the project as ${CALYPSO_SOURCE_DIR} and 
# to the root binary directory of the project as ${CALYPSO_BINARY_DIR}. 
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(CALYPSO Fortran)
# Set the Rayleigh version
SET(VERSION 2.0a)

SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/src/cmake/Modules/")

ENABLE_LANGUAGE(Fortran C)
ENABLE_LANGUAGE(c)

SET(CMAKE_Fortran_MODULE_DIRECTORY "${CALYPSO_BINARY_DIR}/include")
SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CALYPSO_BINARY_DIR}/bin")
SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CALYPSO_BINARY_DIR}/lib")
# Use the preprocessor to handle #ifdefs
ADD_DEFINITIONS(-cpp)

IF(EXISTS ${CMAKE_SOURCE_DIR}/tests/CMakeLists.txt)
  ENABLE_TESTING()
  ADD_SUBDIRECTORY(tests)
ENDIF()

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
    message(STATUS "FFTW is found")
    ADD_DEFINITIONS(-DFFTW3)
    INCLUDE_DIRECTORIES(${FFTW_INCLUDES})
ENDIF(FFTW_FOUND)

# Find HDF5 - if it's not parallel then don't use it at all
FIND_PACKAGE(HDF5 COMPONENTS Fortran)
IF (HDF5_FOUND AND HDF5_IS_PARALLEL)
    message(STATUS "HDF5 Ver. ${HDF5_VERSION} is found")
    ADD_DEFINITIONS(-DHDF5_IO)
    ADD_DEFINITIONS(${HDF5_DEFINITIONS})
    INCLUDE_DIRECTORIES(${HDF5_INCLUDE_DIRS})
ENDIF(HDF5_FOUND AND HDF5_IS_PARALLEL)

# Find BLAS
FIND_PACKAGE(BLAS)
IF (BLAS_FOUND)
    message(STATUS "BLAS is found as ${BLA_VENDOR}")
    ADD_DEFINITIONS(-DBLAS)
ENDIF(BLAS_FOUND)

# Find zlib
FIND_PACKAGE(ZLIB)
IF (ZLIB_FOUND)
    message(STATUS "zlib Ver. ${ZLIB_VERSION_STRING} is found")
    ADD_DEFINITIONS(-DZLIB_IO)
    INCLUDE_DIRECTORIES(${ZLIB_INCLUDE_DIRS})
ENDIF(ZLIB_FOUND)

# Recurse into the "src" subdirectory
ADD_SUBDIRECTORY (src) 

