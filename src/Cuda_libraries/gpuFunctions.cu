#include <cuda_runtime.h>
#include "legendre_poly.h"
#include "math_functions.h"
#include "math_constants.h"
#include <math.h>

Parameters_s deviceInput;
Debug h_debug, d_debug;
//cudaStream_t streams[32];

void initGpu() {
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
  deviceInput.argc = 3; 
  deviceInput.argv = (double**) malloc (deviceInput.argc * sizeof(double*));
  // Debug: 0 = P_smdt, 1 = dp_smdt
  h_debug.argc = d_debug.argc = 2;
  h_debug.argv = (double**) malloc (h_debug.argc * sizeof(double*));
  d_debug.argv = (double**) malloc (d_debug.argc * sizeof(double*));
  allocMemOnGPU();
  //for(unsigned int i=0; i<32; i++)       
  //  cudaErrorCheck(cudaStreamCreate(&streams[i]));
  cudaErrorCheck(cudaDeviceSetCacheConfig(cudaFuncCachePreferEqual));
}

void allocMemOnGPU() {
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.argv[2]), nidx_rlm[1]*sizeof(double))); 
}

void deAllocMemOnGPU() {
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
  cudaErrorCheck(cudaFree(deviceInput.argv[2]));
}

void finalizeGpu() {
  deAllocMemOnGPU();
}
