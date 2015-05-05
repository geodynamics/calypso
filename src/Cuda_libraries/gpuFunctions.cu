#include <cuda_runtime.h>
#include "legendre_poly.h"
#include "math_functions.h"
#include "math_constants.h"
#include <math.h>

Parameters_s deviceInput;
Debug h_debug, d_debug;
Geometry_c constants;

double countFT=0, countBT=0;

cudaStream_t streams[32];
__constant__ Geometry_c devConstants;

//File Streams
std::ofstream clockD;

void initgpu_(int *nnod_rtp, int *nnod_rtm, int *nnod_rlm, int nidx_rtm[], int nidx_rlm[], int istep_rtm[], int istep_rlm[], int *ncomp, int *trunc_lvl) {

  std::string fileName = "GPUTimings.dat";
  clockD.open(fileName.c_str(), std::ofstream::out);
  
  double t_0, t_1;

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
  constants.ncomp = *ncomp;
  constants.t_lvl = *trunc_lvl; 

  #if defined(CUDA_DEBUG) || defined(CHECK_SCHMIDT_OTF)
    cudaProfilerStart();
    allocHostDebug(&h_debug);
    allocDevDebug(&d_debug);
  #endif

  allocMemOnGPU();

  for(unsigned int i=0; i<32; i++)       
    cudaErrorCheck(cudaStreamCreate(&streams[i]));

  cudaErrorCheck(cudaDeviceSetCacheConfig(cudaFuncCachePreferEqual));

  #if defined(CUDA_TIMINGS)
    cudaErrorCheck(cudaDeviceSynchronize());
  #endif
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

void allocMemOnGPU() {
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
  int ncomp = constants.ncomp;
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.vr_rtm), constants.nnod_rtm*ncomp*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.sp_rlm), constants.nnod_rlm*ncomp*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.g_colat_rtm), constants.nidx_rtm[1]*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.a_r_1d_rlm_r), constants.nidx_rtm[0]*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.asin_theta_1d_rtm), constants.nidx_rtm[1]*sizeof(double))); 
//  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.lstack_rlm), (constants.nidx_rtm[2]+1)*sizeof(int))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.g_sph_rlm), constants.nidx_rlm[1]*sizeof(double))); 
   
  cudaErrorCheck(cudaMemset(deviceInput.vr_rtm, 0, constants.nnod_rtm*ncomp*sizeof(double)));
  cudaErrorCheck(cudaMemset(deviceInput.sp_rlm, 0, constants.nnod_rlm*ncomp*sizeof(double)));
}

void memcpy_h2d_(int *lstack_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *g_sph_rlm, double *asin_theta_1d_rtm) {
    h_debug.lstack_rlm = lstack_rlm;
 #ifdef CUDA_DEBUG 
    h_debug.g_colat_rtm = g_colat_rtm;
    h_debug.g_sph_rlm = g_sph_rlm;
#endif

  cudaErrorCheck(cudaMemcpy(deviceInput.a_r_1d_rlm_r, a_r_1d_rlm_r , constants.nidx_rtm[0]*sizeof(double), cudaMemcpyHostToDevice)); 
  cudaErrorCheck(cudaMemcpy(deviceInput.asin_theta_1d_rtm, asin_theta_1d_rtm, constants.nidx_rtm[1]*sizeof(double), cudaMemcpyHostToDevice)); 
  cudaErrorCheck(cudaMemcpy(deviceInput.g_colat_rtm, g_colat_rtm, constants.nidx_rtm[1]*sizeof(double), cudaMemcpyHostToDevice)); 
//  cudaErrorCheck(cudaMemcpy(deviceInput.lstack_rlm, lstack_rlm, (constants.nidx_rtm[2]+1)*sizeof(int), cudaMemcpyHostToDevice)); 
  cudaErrorCheck(cudaMemcpy(deviceInput.g_sph_rlm, g_sph_rlm, constants.nidx_rlm[1]*sizeof(double), cudaMemcpyHostToDevice)); 
  #ifdef CUDA_TIMINGS
    cudaErrorCheck(cudaDeviceSynchronize()); 
  #endif
}

void cpy_schmidt_2_gpu_(double *P_jl, double *dP_jl) {
  #ifdef CUDA_STATIC
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.P_jl), sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]));
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.dP_jl), sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]));
  cudaErrorCheck(cudaMemcpy(P_jl, deviceInput.P_jl, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]));
  cudaErrorCheck(cudaMemcpy(dP_jl, deviceInput.dP_jl, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]));
  #endif
}
 
void allocHostDebug(Debug* h_data) {
  h_debug.P_smdt = (double*) malloc (sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]);
  h_debug.dP_smdt = (double*) malloc (sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]);
  #if defined(CUDA_DEBUG)
  h_debug.vr_rtm = (double*) malloc (sizeof(double)*constants.nnod_rtm*constants.ncomp);
  #endif
//  h_data->g_sph_rlm = (double*) malloc (sizeof(double)*constants.nidx_rlm[1]);
}
 
void allocDevDebug(Debug* d_data) {
  cudaErrorCheck(cudaMalloc((void**)&(d_data->P_smdt), sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]));
  cudaErrorCheck(cudaMemset(d_data->P_smdt, -1, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]));
  cudaErrorCheck(cudaMalloc((void**)&(d_data->dP_smdt), sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]));
  cudaErrorCheck(cudaMemset(d_data->dP_smdt, -1, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]));
//  cudaErrorCheck(cudaMalloc((void**)&(d_data->g_sph_rlm), sizeof(double)*constants.nidx_rlm[1]));
}

void cpy_dev2host_4_debug_() {
  cudaErrorCheck(cudaMemcpy(h_debug.P_smdt, d_debug.P_smdt, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1], cudaMemcpyDeviceToHost)); 
  cudaErrorCheck(cudaMemcpy(h_debug.dP_smdt, d_debug.dP_smdt, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1], cudaMemcpyDeviceToHost)); 
  cudaErrorCheck(cudaMemcpy(h_debug.vr_rtm, deviceInput.vr_rtm, constants.nnod_rtm*constants.ncomp*sizeof(double), cudaMemcpyDeviceToHost)); 
//  cudaErrorCheck(cudaMemcpy(d_data->g_sph_rlm, h_data->g_sph_rlm, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1], cudaMemcpyDeviceToHost)); 
}

void writeDebugData2File(Debug *data, std::string fileName) {
#ifdef CUDA_DEBUG
  std::ofstream fp;
  fp.open(fileName.c_str(), std::ofstream::out);
  fp.precision(16);
  //Header for file
  fp << "shell\ttheta\tvector\torder\tdegree\tg_sph_rlm\tP_smdt\tdP_smdt\tvr_rtm[0]\tvr_rtm[1]\tvr_rtm[2]\n"; 

 int idx_vr_rtm, jst, jed, m, j, l;
 int *ptr;
 for(int k=0; k<constants.nidx_rtm[0]; k++){ 
   for(int mp_rlm=constants.t_lvl+2; mp_rlm<=constants.t_lvl+3; mp_rlm++) {
     jst = data->lstack_rlm[mp_rlm-1] + 1;
     jed = data->lstack_rlm[mp_rlm];
     for(int l_rtm=0; l_rtm<constants.nidx_rtm[1]; l_rtm++) {
       for(int nd=1; nd <= constants.nvector; nd++) {
         idx_vr_rtm = (3*nd-1) + constants.ncomp*(l_rtm)*constants.istep_rtm[1] + k*constants.istep_rtm[0] + (mp_rlm-1)*constants.istep_rtm[2];
         for(int j_rlm=jst; j_rlm<=jed; j_rlm++) {
           ptr = data->idx_gl_1d_rlm_j;
           m = *(ptr + (j_rlm-1 + constants.nidx_rlm[1]*2));
           l = *(ptr + (j_rlm-1 + constants.nidx_rlm[1]*1));
           j = l*(l+1) + m;
           if(m != 2) { 
             fp << k+1 << "\t" << data->g_colat_rtm[l_rtm] << "\t" << nd << "\t" << m << "\t" << l;
             fp << "\t" << data->g_sph_rlm[j_rlm-1];
             fp << "\t" << data->P_smdt[l_rtm*constants.nidx_rlm[1]+j] << "\t" << data->dP_smdt[l_rtm*constants.nidx_rlm[1]+j];
             fp << "\t" << data->vr_rtm[idx_vr_rtm-2];
             fp << "\t" << data->vr_rtm[idx_vr_rtm-1];
             fp << "\t" << data->vr_rtm[idx_vr_rtm] << "\n"; 
            }
            else {
             fp << k+1 << "\t" << data->g_colat_rtm[l_rtm] << "\t" << nd << "\t" << m << "\t" << l;
             fp << "\t" << data->g_sph_rlm[j_rlm];
             fp << "\t" << data->P_smdt[l_rtm*constants.nidx_rlm[1]+j] << "\n";
            }
         }
       }
      }
    }
  }

  fp.close();
#endif
}

void set_spectrum_data_(double *sp_rlm) {
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
  cudaErrorCheck(cudaMemcpy(deviceInput.sp_rlm, sp_rlm, constants.nnod_rlm*constants.ncomp*sizeof(double), cudaMemcpyHostToDevice)); 
  #ifdef CUDA_TIMINGS
    cudaErrorCheck(cudaDeviceSynchronize());
  #endif
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
  #ifdef CUDA_TIMINGS
    cudaErrorCheck(cudaDeviceSynchronize());
  #endif
}

void deAllocMemOnGPU() {
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
    cudaErrorCheck(cudaFree(deviceInput.vr_rtm));
    cudaErrorCheck(cudaFree(deviceInput.sp_rlm));
    cudaErrorCheck(cudaFree(deviceInput.g_colat_rtm));
    cudaErrorCheck(cudaFree(deviceInput.g_sph_rlm));
    cudaErrorCheck(cudaFree(deviceInput.a_r_1d_rlm_r));
    cudaErrorCheck(cudaFree(deviceInput.lstack_rlm));
}

void deAllocDebugMem() {
  free(h_debug.P_smdt);
  free(h_debug.dP_smdt);
//  free(h_debug.g_sph_rlm);
  cudaErrorCheck(cudaFree(d_debug.P_smdt));
  cudaErrorCheck(cudaFree(d_debug.dP_smdt));
//  cudaErrorCheck(cudaFree(d_debug.g_sph_rlm));
}

void cleangpu_() {
  deAllocMemOnGPU();
  deAllocDebugMem();
  #ifdef CUDA_DEBUG
    cudaProfilerStop();
    cudaDeviceReset();
  #endif
 clockD.close();
}

void initDevConstVariables() {
  cudaError_t error;
  error = cudaMemcpyToSymbol(devConstants, &constants, sizeof(Geometry_c), 0, cudaMemcpyHostToDevice);
  cudaErrorCheck(error);
}
