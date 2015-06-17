#include <cuda_runtime.h>
#include "legendre_poly.h"
#include "math_functions.h"
#include "math_constants.h"
#include <math.h>
#include <unistd.h>

Parameters_s deviceInput;
Debug h_debug, d_debug;
Geometry_c constants;

int countFT=0, countBT=0;

cudaStream_t streams[2];
//__constant__ Geometry_c devConstants;

void initialize_gpu_() {
  int device_count, device;
  // Gets number of GPU devices
  cudaGetDeviceCount(&device_count);
  cudaGetDevice(&device);
  cudaDeviceReset();
  #if defined(CUDA_TIMINGS)
    cudaProfilerStart();
  #endif
  cudaErrorCheck(cudaDeviceSetCacheConfig(cudaFuncCachePreferL1));
  cudaFree(0);
}

void set_constants_(int *nnod_rtp, int *nnod_rtm, int *nnod_rlm, int nidx_rtm[], int nidx_rlm[], int istep_rtm[], int istep_rlm[], int *trunc_lvl, int *np_smp) {

  for(int i=0; i<3; i++) { 
    constants.nidx_rtm[i] = nidx_rtm[i];
    constants.istep_rtm[i] = istep_rtm[i];
  }

  for(int i=0; i<2; i++) {
    constants.nidx_rlm[i] = nidx_rlm[i];
    constants.istep_rlm[i] = istep_rlm[i];
  }

  constants.nnod_rtp = *nnod_rtp;
  constants.nnod_rtm = *nnod_rtm;
  constants.nnod_rlm = *nnod_rlm;
  constants.t_lvl = *trunc_lvl; 

  constants.np_smp = *np_smp;

  #if defined(CUDA_OTF)
    initDevConstVariables();
  #endif

  for(unsigned int i=0; i<2; i++)       
    cudaErrorCheck(cudaStreamCreate(&streams[i]));


//  #if defined(CUDA_TIMINGS)
/*    t_1 = MPI_Wtime();
    char name[15];
    gethostname(name, 15);
    string str(name);
    std::cout<<"Host: " << str << "\t Memory Allocation Time: " << t_1-t_0 << "\t Device Initialization Time: " << t_3-t_2 << std::endl;*/
//  #endif

}

void setptrs_(int *idx_gl_1d_rlm_j) {
  //Necessary to filter harmonic modes across MPI nodes.
  h_debug.idx_gl_1d_rlm_j = idx_gl_1d_rlm_j;
}

/*void setptrs_(int *idx_gl_1d_rlm_j, double *P_smdt, double *dP_smdt) {
  h_debug.idx_gl_1d_rlm_j = idx_gl_1d_rlm_j;
  //h_debug.P_smdt = P_smdt;
  //h_debug.dP_smdt = dP_smdt;
}*/

void alloc_space_on_gpu_(int *ncmp, int *nvector, int *nscalar) {
  constants.ncomp = *ncmp;
  constants.nvector = *nvector;
  constants.nscalar = *nscalar;

  int ncomp = *ncmp;
  #if defined(CUDA_DEBUG) || defined(CHECK_SCHMIDT_OTF)
    allocHostDebug(&h_debug);
    allocDevDebug(&d_debug);
  #endif
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.vr_rtm), constants.nnod_rtm*ncomp*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.sp_rlm), constants.nnod_rlm*ncomp*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.g_colat_rtm), constants.nidx_rtm[1]*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.a_r_1d_rlm_r), constants.nidx_rtm[0]*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.asin_theta_1d_rtm), constants.nidx_rtm[1]*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.lstack_rlm), (constants.nidx_rtm[2]+1)*sizeof(int))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.g_sph_rlm), constants.nidx_rlm[1]*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.g_sph_rlm_7), constants.nidx_rlm[1]*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.idx_gl_1d_rlm_j), constants.nidx_rlm[1]*3*sizeof(int))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.radius_1d_rlm_r), constants.nidx_rtm[0]*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.weight_rtm), constants.nidx_rtm[1]*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.mdx_p_rlm_rtm), constants.nidx_rlm[1]*sizeof(int))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.mdx_n_rlm_rtm), constants.nidx_rlm[1]*sizeof(int))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.p_jl), sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]));
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.dP_jl), sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]));
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.p_rtm), sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]));
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.dP_rtm), sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]));
   
  cudaErrorCheck(cudaMemset(deviceInput.vr_rtm, 0, constants.nnod_rtm*ncomp*sizeof(double)));
  cudaErrorCheck(cudaMemset(deviceInput.sp_rlm, 0, constants.nnod_rlm*ncomp*sizeof(double)));
}

void memcpy_h2d_(int *lstack_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *g_sph_rlm, double *g_sph_rlm_7, double *asin_theta_1d_rtm, int *idx_gl_1d_rlm_j, double *radius_1d_rlm_r, double *weight_rtm, int *mdx_p_rlm_rtm, int *mdx_n_rlm_rtm) {
    h_debug.lstack_rlm = lstack_rlm;
 #ifdef CUDA_DEBUG 
    h_debug.g_colat_rtm = g_colat_rtm;
    h_debug.g_sph_rlm = g_sph_rlm;
#endif

  cudaErrorCheck(cudaMemcpy(deviceInput.a_r_1d_rlm_r, a_r_1d_rlm_r , constants.nidx_rtm[0]*sizeof(double), cudaMemcpyHostToDevice)); 
  cudaErrorCheck(cudaMemcpy(deviceInput.asin_theta_1d_rtm, asin_theta_1d_rtm, constants.nidx_rtm[1]*sizeof(double), cudaMemcpyHostToDevice)); 
  cudaErrorCheck(cudaMemcpy(deviceInput.g_colat_rtm, g_colat_rtm, constants.nidx_rtm[1]*sizeof(double), cudaMemcpyHostToDevice)); 
  cudaErrorCheck(cudaMemcpy(deviceInput.lstack_rlm, lstack_rlm, (constants.nidx_rtm[2]+1)*sizeof(int), cudaMemcpyHostToDevice)); 
  cudaErrorCheck(cudaMemcpy(deviceInput.g_sph_rlm, g_sph_rlm, constants.nidx_rlm[1]*sizeof(double), cudaMemcpyHostToDevice)); 
  cudaErrorCheck(cudaMemcpy(deviceInput.g_sph_rlm_7, g_sph_rlm_7, constants.nidx_rlm[1]*sizeof(double), cudaMemcpyHostToDevice)); 
  cudaErrorCheck(cudaMemcpy(deviceInput.idx_gl_1d_rlm_j, idx_gl_1d_rlm_j, constants.nidx_rlm[1]*3*sizeof(int), cudaMemcpyHostToDevice)); 
  cudaErrorCheck(cudaMemcpy(deviceInput.radius_1d_rlm_r, radius_1d_rlm_r, constants.nidx_rtm[0]*sizeof(double), cudaMemcpyHostToDevice)); 
  cudaErrorCheck(cudaMemcpy(deviceInput.weight_rtm, weight_rtm, constants.nidx_rtm[1]*sizeof(double), cudaMemcpyHostToDevice)); 
  cudaErrorCheck(cudaMemcpy(deviceInput.mdx_p_rlm_rtm, mdx_p_rlm_rtm, constants.nidx_rlm[1]*sizeof(int), cudaMemcpyHostToDevice)); 
  cudaErrorCheck(cudaMemcpy(deviceInput.mdx_n_rlm_rtm, mdx_n_rlm_rtm, constants.nidx_rlm[1]*sizeof(int), cudaMemcpyHostToDevice)); 
}

void cpy_schmidt_2_gpu_(double *P_jl, double *dP_jl, double *P_rtm, double *dP_rtm) {
    cudaErrorCheck(cudaMemcpy(deviceInput.p_jl, P_jl, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1], cudaMemcpyHostToDevice));
    cudaErrorCheck(cudaMemcpy(deviceInput.dP_jl, dP_jl, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1], cudaMemcpyHostToDevice));
    cudaErrorCheck(cudaMemcpy(deviceInput.p_rtm, P_rtm, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1], cudaMemcpyHostToDevice));
    cudaErrorCheck(cudaMemcpy(deviceInput.dP_rtm, dP_rtm, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1], cudaMemcpyHostToDevice));
}
 
void allocHostDebug(Debug* h_data) {
  #if defined(CUDA_OTF)
    h_debug.P_smdt = (double*) malloc (sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]);
    h_debug.dP_smdt = (double*) malloc (sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]);
  #endif
  #if defined(CUDA_DEBUG)
    h_debug.vr_rtm = (double*) malloc (sizeof(double)*constants.nnod_rtm*constants.ncomp);
    h_debug.sp_rlm = (double*) malloc (sizeof(double)*constants.nnod_rlm*constants.ncomp);
  #endif
//  h_data->g_sph_rlm = (double*) malloc (sizeof(double)*constants.nidx_rlm[1]);
}
 
void allocDevDebug(Debug* d_data) {
  #if defined(CUDA_OTF)
    cudaErrorCheck(cudaMalloc((void**)&(d_data->P_smdt), sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]));
    cudaErrorCheck(cudaMemset(d_data->P_smdt, -1, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]));
    cudaErrorCheck(cudaMalloc((void**)&(d_data->dP_smdt), sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]));
    cudaErrorCheck(cudaMemset(d_data->dP_smdt, -1, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]));
  #endif
//  cudaErrorCheck(cudaMalloc((void**)&(d_data->g_sph_rlm), sizeof(double)*constants.nidx_rlm[1]));
}

void cpy_field_dev2host_4_debug_() {
  #if defined(CUDA_OTF)
    cudaErrorCheck(cudaMemcpy(h_debug.P_smdt, d_debug.P_smdt, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1], cudaMemcpyDeviceToHost)); 
    cudaErrorCheck(cudaMemcpy(h_debug.dP_smdt, d_debug.dP_smdt, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1], cudaMemcpyDeviceToHost)); 
  #endif
  cudaErrorCheck(cudaMemcpy(h_debug.vr_rtm, deviceInput.vr_rtm, constants.nnod_rtm*constants.ncomp*sizeof(double), cudaMemcpyDeviceToHost)); 
//  cudaErrorCheck(cudaMemcpy(d_data->g_sph_rlm, h_data->g_sph_rlm, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1], cudaMemcpyDeviceToHost)); 
}

void cpy_spec_dev2host_4_debug_() {
  #if defined(CUDA_OTF)
    cudaErrorCheck(cudaMemcpy(h_debug.P_smdt, d_debug.P_smdt, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1], cudaMemcpyDeviceToHost)); 
    cudaErrorCheck(cudaMemcpy(h_debug.dP_smdt, d_debug.dP_smdt, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1], cudaMemcpyDeviceToHost)); 
  #endif
  cudaErrorCheck(cudaMemcpy(h_debug.sp_rlm, deviceInput.sp_rlm, constants.nnod_rlm*constants.ncomp*sizeof(double), cudaMemcpyDeviceToHost)); 
//  cudaErrorCheck(cudaMemcpy(d_data->g_sph_rlm, h_data->g_sph_rlm, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1], cudaMemcpyDeviceToHost)); 
}

void set_spectrum_data_(double *sp_rlm, int *ncomp) {
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
  cudaErrorCheck(cudaMemcpy(deviceInput.sp_rlm, sp_rlm, constants.nnod_rlm*(*ncomp)*sizeof(double), cudaMemcpyHostToDevice)); 
}

void set_physical_data_(double *vr_rtm, int *ncomp) {
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
  cudaErrorCheck(cudaMemcpy(deviceInput.vr_rtm, vr_rtm, constants.nnod_rtm*(*ncomp)*sizeof(double), cudaMemcpyHostToDevice)); 
}

void retrieve_spectrum_data_(double *sp_rlm, int *ncomp) {
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
  cudaErrorCheck(cudaMemcpy(sp_rlm, deviceInput.sp_rlm, constants.nnod_rlm*(*ncomp)*sizeof(double), cudaMemcpyDeviceToHost)); 
}

void retrieve_physical_data_(double *vr_rtm, int *ncomp) {
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
  cudaErrorCheck(cudaMemcpy(vr_rtm, deviceInput.vr_rtm, constants.nnod_rtm*(*ncomp)*sizeof(double), cudaMemcpyDeviceToHost)); 
}

void clear_spectrum_data_(int *ncomp) {
  cudaErrorCheck(cudaMemset(deviceInput.sp_rlm, 0, constants.nnod_rlm*(*ncomp)*sizeof(double)));
}

void clear_field_data_(int *ncomp) {
  cudaErrorCheck(cudaMemset(deviceInput.vr_rtm, 0, constants.nnod_rtm*(*ncomp)*sizeof(double)));
}

void deAllocMemOnGPU() {
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
    cudaErrorCheck(cudaFree(deviceInput.vr_rtm));
    cudaErrorCheck(cudaFree(deviceInput.sp_rlm));
    cudaErrorCheck(cudaFree(deviceInput.g_colat_rtm));
    cudaErrorCheck(cudaFree(deviceInput.g_sph_rlm));
    cudaErrorCheck(cudaFree(deviceInput.g_sph_rlm_7));
    cudaErrorCheck(cudaFree(deviceInput.a_r_1d_rlm_r));
    cudaErrorCheck(cudaFree(deviceInput.lstack_rlm));
    cudaErrorCheck(cudaFree(deviceInput.idx_gl_1d_rlm_j));
    cudaErrorCheck(cudaFree(deviceInput.radius_1d_rlm_r));
    cudaErrorCheck(cudaFree(deviceInput.weight_rtm));
    cudaErrorCheck(cudaFree(deviceInput.mdx_p_rlm_rtm));
    cudaErrorCheck(cudaFree(deviceInput.mdx_n_rlm_rtm));
    cudaErrorCheck(cudaFree(deviceInput.asin_theta_1d_rtm));
    cudaErrorCheck(cudaFree(deviceInput.p_jl));
    cudaErrorCheck(cudaFree(deviceInput.dP_jl));
    cudaErrorCheck(cudaFree(deviceInput.p_rtm));
    cudaErrorCheck(cudaFree(deviceInput.dP_rtm));
}

void deAllocDebugMem() {
  #if defined(CUDA_OTF) 
    free(h_debug.P_smdt);
    free(h_debug.dP_smdt);
  #endif
    free(h_debug.vr_rtm);
    free(h_debug.sp_rlm);
//  free(h_debug.g_sph_rlm);
  #if defined(CUDA_OTF) 
    cudaErrorCheck(cudaFree(d_debug.P_smdt));
    cudaErrorCheck(cudaFree(d_debug.dP_smdt));
  #endif
//  cudaErrorCheck(cudaFree(d_debug.g_sph_rlm));
}

void cleangpu_() {
  deAllocMemOnGPU();
  deAllocDebugMem();
  for(int i=0; i<2; i++)
    cudaErrorCheck(cudaStreamDestroy(streams[i]));
  #if defined(CUDA_TIMINGS)
    cudaProfilerStop();
  #endif
}

void cuda_sync_device_() {
  cudaErrorCheck(cudaDeviceSynchronize());
}

void initDevConstVariables() {
  cudaError_t error;
  error = cudaMemcpyToSymbol(devConstants, &constants, sizeof(Geometry_c), 0, cudaMemcpyHostToDevice);
  cudaErrorCheck(error);
}
