FILE(GLOB CUDA_SRC_FILES *.cu)
#FILE(GLOB FORTRAN_CUDA_INTERFACE *.f90)

set(GENCODE "-gencode=arch=compute_35,code=sm_35")

# The option dc enables dynamic parallelism in cuda

# debug options
set(CUDA_NVCC_FLAGS_DEBUG "-G;-g;-O0;-DCUDA_DEBUG")
set(CUDA_NVCC_FLAGS_RELEASE "-O3;-DCUDA_OPTIMIZED")


#The following command links the compiled code. 
#This is necessary when using dynamic parallelism for one. 
#Dynamic Parallelism essentially allows you to nest kernel calls. 

set(CUDA_SEPARABLE_COMPILATION OFF)
set(CUDA_VERBOSE_BUILD ON)
set(CUDA_HOST_COMPILER ${CMAKE_CXX_COMPILER})

#cuda_add_library( cudaLib ${CUDA_SRC_FILES} STATIC OPTIONS ${GENCODE})
#cuda_add_library( cudaLib ${CUDA_SRC_FILES} STATIC OPTIONS ${GENCODE} ${CUDA_LIBRARY_FLAGS} )
cuda_add_library( cudaLib ${CUDA_SRC_FILES} OPTIONS ${GENCODE} ${CUDA_LIBRARY_FLAGS} )
IF(CMAKE_BUILD_TYPE MATCHES DEBUG)
  set_target_properties(cudaLib PROPERTIES LINK_FLAGS
"${CUDA_LIBRARY_FLAGS};-DCUDA_DEBUG")
ENDIF(CMAKE_BUILD_TYPE MATCHES DEBUG)
IF(CMAKE_BUILD_TYPE MATCHES RELEASE)
  set_target_properties(cudaLib PROPERTIES LINK_FLAGS
"${CUDA_LIBRARY_FLAGS};-DCUDA_OPTIMIZED")
ENDIF(CMAKE_BUILD_TYPE MATCHES RELEASE)

#add_library( cudaFortran ${FORTRAN_CUDA_INTERFACE} )

#add_dependencies( cudaFortran cudaLib )

#target_link_libraries( cudaFortran cudaLib ${CUDA_LIBRARY_FLAGS} )

#if(CUDA_DEBUG)
#  add_custom_target(cudaLib 
#                        command
#                            ${CUDA_TOOLKIT_ROOT_DIR}/bin/nvcc -m${OS_type}
#                            -gencode ${genCodes} -dc -g -G -c ${CUDA_SRC_FILES}
#                            -o ${cudaLibPath} -D DEBUG)
#else()
#  add_custom_target(cudaLib 
#                        command 
#                            ${CUDA_TOOLKIT_ROOT_DIR}/bin/nvcc -m${OS_type}
#                            -gencode ${genCodes} -dc -c ${CUDA_SRC_FILES}
#                            -o ${cudaLibPath})
#endif()

#Assuming 64 bit architecture
#add_custom_target(cudaLibLink ALL 
#                          COMMAND
#                              ${CUDA_TOOLKIT_ROOT_DIR}/bin/nvcc
#                              -m${OS_type} -gencode ${genCodes} 
#                              -dlink ${cudaLibPath}
#                              -o ${cudaLibLinkPath} 
#                              -I ${CUDA_TOOLKIT_INCLUDE} 
#                              -L ${CUDA_TOOLKIT_ROOT_DIR}/lib64 -lcudart
#                              -lcublas -lcuda -lcublas_device -lcudadevrt)
                         
#add_dependencies(cudaLibLink cudaLib)

