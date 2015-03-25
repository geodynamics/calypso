#include <cuda_runtime.h>
#include "legendre_poly.h"
#include "math_functions.h"
#include "math_constants.h"
#include <math.h>

Parameters_s deviceInput;
Debug h_debug, d_debug;
Geometry_c constants;

//cudaStream_t streams[32];
__constant__ Geometry_c devConstants;

void initgpu_(int *nnod_rtp, int *nnod_rtm, int *nnod_rlm, int nidx_rtm[], int nidx_rlm[], int istep_rtm[], int istep_rlm[], int *ncomp, double *g_sph_rlm, double *a_r_1d_rlm_r, int *lstack_rlm, double *g_colat_rtm, int *trunc_lvl) {
  
  constants.nidx_rtm = nidx_rtm;
  constants.nidx_rlm = nidx_rlm;
  constants.istep_rtm = istep_rtm;
  constants.istep_rlm = istep_rlm;
  constants.nnod_rtp = *nnod_rtp;
  constants.nnod_rtm = *nnod_rtm;
  constants.nnod_rlm = *nnod_rlm;
  constants.ncomp = *ncomp;
  constants.t_lvl = *trunc_lvl; 

  #ifdef CUDA_DEBUG
    allocHostDebug(h_schmidt);
    h_schmidt.g_colat_rtm = g_colat_rtm;
    sllocDevDebug(d_schmidt);
  #endif

  allocMemOnGPU(g_sph_rlm, a_r_1d_rlm_r, g_colat_rtm);
  
  //for(unsigned int i=0; i<32; i++)       
  //  cudaErrorCheck(cudaStreamCreate(&streams[i]));
  cudaErrorCheck(cudaDeviceSetCacheConfig(cudaFuncCachePreferEqual));
}

void allocMemOnGPU(double *g_sph_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm) {
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
  int ncomp = constants.ncomp;
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.vr_rtm), constants.nnod_rtm*ncomp*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.sp_rlm), constants.nnod_rlm*ncomp*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.g_sph_rlm), constants.nidx_rtm[1]*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.a_r_1d_rlm_r), constants.nidx_rtm[0]*sizeof(double))); 
   
  cudaErrorCheck(cudaMemset(deviceInput.vr_rtm, 0, constants.nnod_rtm*ncomp*sizeof(double)));
  cudaErrorCheck(cudaMemset(deviceInput.sp_rlm, 0, constants.nnod_rlm*ncomp*sizeof(double)));
  cudaErrorCheck(cudaMemcpy(deviceInput.g_sph_rlm, g_sph_rlm, constants.nidx_rtm[1]*sizeof(double), cudaMemcpyHostToDevice)); 
  cudaErrorCheck(cudaMemcpy(deviceInput.a_r_1d_rlm_r, a_r_1d_rlm_r , constants.nidx_rtm[0]*sizeof(double), cudaMemcpyHostToDevice)); 
  cudaErrorCheck(cudaMemcpy(deviceInput.g_colat_rtm, g_colat_rtm, constants.nidx_rtm[1]*sizeof(double), cudaMemcpyHostToDevice)); 
}

void allocHostDebug(Debug* h_data) {
  h_data->P_smdt = (double*) malloc (sizeof(double)*constants.nidx_rtm[1]*nidx_rlm[1]);
  h_data->g_colat_rtm = (double*) malloc (sizeof(double)*constants.nidx_rtm[1]);
}
 
void allocDevDebug(Debug* d_data) {
  cudaErrorCheck(cudaMalloc((void**)&(d_data->P_smdt), sizeof(double)*constants.nidx_rtm[1]*nidx_rlm[1]));
  cudaErrorCheck(cudaMalloc((void**)&(d_data->dP_smdt), sizeof(double)*constants.nidx_rtm[1]*nidx_rlm[1]));
}

void cpyDev2Host(Debug* d_data, Debug* h_data) {
  cudaErrorCheck(cudaMemcpy(d_data->P_smdt, h_data->P_smdt, sizeof(double)*constants.nidx_rtm[1]*nidx_rlm[1], cudaMemcpyDeviceToHost)); 
  cudaErrorCheck(cudaMemcpy(d_data->dP_smdt, h_data->dP_smdt, sizeof(double)*constants.nidx_rtm[1]*nidx_rlm[1], cudaMemcpyDeviceToHost)); 
}

void writeDebugData2File(std::ofstream *fp, Debug *data) {
  //Header for file
  *fp << "order\tdegree\tj\tidx_theta\ttheta\tP_smdt\tdP_smdt\n"; 

  for(int l=0; l<=constants.t_lvl; l++) {
    for(int m=0; m<=l; m++) {
      j = l*(l+1) + m; 
      for(int l_rtm=0; l<constants.nidx_rtm[1]; l++) {
        *fp << m << "\t" << l << "\t" << "\t" << j << "\t" << l_rtm << "\t" << data.g_colat_rtm[l_rtm] << "\t" << data.P_smdt[l_rtm + nidx_rtm[1]*j] << "\t" << data.dP_smdt[l_rtm + nidx_rtm[1]*j] << "\n"; 
      }
    }
  }
}

void set_spectrum_data_(double *sp_rlm) {
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
  cudaErrorCheck(cudaMemcpy(deviceInput.sp_rlm, sp_rlm, constants.nnod_rlm*constants.ncomp*sizeof(double), cudaMemcpyHostToDevice)); 
}

void set_physical_data_(double *vr_rtm) {
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
  cudaErrorCheck(cudaMemcpy(deviceInput.vr_rtm, vr_rtm, constants.nnod_rtm*constants.ncomp*sizeof(double), cudaMemcpyHostToDevice)); 
}

void retrieve_spectrum_data_(double *sp_rlm) {
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
  cudaErrorCheck(cudaMemcpy(sp_rlm, deviceInput.sp_rlm, constants.nnod_rlm*constants.ncomp*sizeof(double), cudaMemcpyDeviceToHost)); 
}

void retrieve_physical_data_(double *vr_rtm) {
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
  cudaErrorCheck(cudaMemcpy(vr_rtm, deviceInput.vr_rtm, constants.nnod_rtm*constants.ncomp*sizeof(double), cudaMemcpyDeviceToHost)); 
}

void deAllocMemOnGPU() {
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
    cudaErrorCheck(cudaFree(deviceInput.vr_rtm));
    cudaErrorCheck(cudaFree(deviceInput.sp_rlm));
    cudaErrorCheck(cudaFree(deviceInput.g_sph_rlm));
    cudaErrorCheck(cudaFree(deviceInput.g_colat_rtm));
    cudaErrorCheck(cudaFree(deviceInput.a_r_1d_rlm_r));
}

void deAllocDebugMem() {
  free(h_debug.P_smdt);
  free(h_debug.dP_smdt);
  cudaErrorCheck(cudaFree(d_debug.P_smdt));
  cudaErrorCheck(cudaFree(d_debug.dP_smdt));
}

void cleangpu_() {
  deAllocMemOnGPU();
  deAllocDebugMem();
}

void initDevConstVariables() {
  cudaError_t error;
  error = cudaMemcpyToSymbol(devConstants, &constants, sizeof(Geometry_c), 0, cudaMemcpyHostToDevice);
  cudaErrorCheck(error);
}
