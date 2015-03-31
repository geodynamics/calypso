#include <cuda_runtime.h>
#include "legendre_poly.h"
#include "math_functions.h"
#include "math_constants.h"
#include <math.h>

Parameters_s deviceInput;
Debug h_debug, d_debug;
Geometry_c constants;

double countFT=0, countBT=0;

//cudaStream_t streams[32];
__constant__ Geometry_c devConstants;

//File Streams
std::ofstream *clockD;

void initgpu_(int *nnod_rtp, int *nnod_rtm, int *nnod_rlm, int nidx_rtm[], int nidx_rlm[], int istep_rtm[], int istep_rlm[], int *ncomp, double *a_r_1d_rlm_r, int *lstack_rlm, double *g_colat_rtm, int *trunc_lvl, double *g_sph_rlm) {

  std::string fileName = "GPUTimings.dat";
  std::ofstream clockGPU(fileName.c_str());
  clockD = &clockGPU;  
  
  double t_0, t_1;

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
    allocHostDebug(&h_debug);
    h_debug.g_colat_rtm = g_colat_rtm;
    h_debug.lstack_rlm = lstack_rlm;
    t_0 = MPI_Wtime(); 
    allocDevDebug(&d_debug);
    cudaErrorCheck(cudaDeviceSynchronize());
    t_1 = MPI_Wtime();
    *clockD << "Allocation of Device Debug variables: P_smdt, dP_smdt, g_sph_rlm\t" << t_1-t_0 << std::endl;
  #endif

  t_0 = MPI_Wtime(); 
  allocMemOnGPU(lstack_rlm, a_r_1d_rlm_r, g_colat_rtm, g_sph_rlm);
  cudaErrorCheck(cudaDeviceSynchronize());
  t_1 = MPI_Wtime(); 
  *clockD << "Allocation of memory for Device variables: sp_rlm, vr_rtm, g_colat_rtm, a_r_1d_rlm_r, etc\t" << t_1-t_0 << std::endl; 

  //for(unsigned int i=0; i<32; i++)       
  //  cudaErrorCheck(cudaStreamCreate(&streams[i]));
  cudaErrorCheck(cudaDeviceSetCacheConfig(cudaFuncCachePreferEqual));
}

void allocMemOnGPU(int *lstack_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *g_sph_rlm) {
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm 
  int ncomp = constants.ncomp;
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.vr_rtm), constants.nnod_rtm*ncomp*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.sp_rlm), constants.nnod_rlm*ncomp*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.g_colat_rtm), constants.nidx_rtm[1]*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.a_r_1d_rlm_r), constants.nidx_rtm[0]*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.lstack_rlm), (constants.nidx_rtm[2]+1)*sizeof(double))); 
  cudaErrorCheck(cudaMalloc((void**)&(deviceInput.g_sph_rlm), constants.nidx_rlm[1]*sizeof(double))); 
   
  cudaErrorCheck(cudaMemset(deviceInput.vr_rtm, 0, constants.nnod_rtm*ncomp*sizeof(double)));
  cudaErrorCheck(cudaMemset(deviceInput.sp_rlm, 0, constants.nnod_rlm*ncomp*sizeof(double)));
  cudaErrorCheck(cudaMemcpy(deviceInput.a_r_1d_rlm_r, a_r_1d_rlm_r , constants.nidx_rtm[0]*sizeof(double), cudaMemcpyHostToDevice)); 
  cudaErrorCheck(cudaMemcpy(deviceInput.g_colat_rtm, g_colat_rtm, constants.nidx_rtm[1]*sizeof(double), cudaMemcpyHostToDevice)); 
  cudaErrorCheck(cudaMemcpy(deviceInput.lstack_rlm, lstack_rlm, (constants.nidx_rtm[2]+1)*sizeof(double), cudaMemcpyHostToDevice)); 
  cudaErrorCheck(cudaMemcpy(deviceInput.g_sph_rlm, g_sph_rlm, constants.nidx_rlm[1]*sizeof(double), cudaMemcpyHostToDevice)); 
}

void allocHostDebug(Debug* h_data) {
  h_data->P_smdt = (double*) malloc (sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]);
  h_data->dP_smdt = (double*) malloc (sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]);
  h_data->g_sph_rlm = (double*) malloc (sizeof(double)*constants.nidx_rlm[1]);
}
 
void allocDevDebug(Debug* d_data) {
  cudaErrorCheck(cudaMalloc((void**)&(d_data->P_smdt), sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]));
  cudaErrorCheck(cudaMalloc((void**)&(d_data->dP_smdt), sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1]));
  cudaErrorCheck(cudaMalloc((void**)&(d_data->g_sph_rlm), sizeof(double)*constants.nidx_rlm[1]));
}

void cpyDev2Host(Debug* d_data, Debug* h_data) {
  cudaErrorCheck(cudaMemcpy(d_data->P_smdt, h_data->P_smdt, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1], cudaMemcpyDeviceToHost)); 
  cudaErrorCheck(cudaMemcpy(d_data->dP_smdt, h_data->dP_smdt, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1], cudaMemcpyDeviceToHost)); 
  cudaErrorCheck(cudaMemcpy(d_data->g_sph_rlm, h_data->g_sph_rlm, sizeof(double)*constants.nidx_rtm[1]*constants.nidx_rlm[1], cudaMemcpyDeviceToHost)); 
}

void writeDebugData2File(std::ofstream *fp, Debug *data) {
#ifdef CUDA_DEBUG
  //Header for file
  *fp << "order\tdegree\tj\tshell\tidx_theta\ttheta\tg_sph_rlm\tP_smdt\tdP_smdt\tvr_rtm[0]\tvr_rtm[1]\tvr_rtm[2]\n"; 

 int idx_vr_rtm, jst, jed, m, j;
 for(int k=0; k<constants.nidx_rtm[0]; k++){ 
   for(int mp_rlm=constants.t_lvl+1; mp_rlm<=constants.t_lvl+2; mp_rlm++) {
     jst = data->lstack_rlm[mp_rlm-1] + 1;
     jed = data->lstack_rlm[mp_rlm];
     m = mp_rlm - (constants.t_lvl+1);
     for(int l=m; l<=constants.t_lvl; l++) {
        j = l*(l+1) + m; 
        for(int l_rtm=0; l<constants.nidx_rtm[1]; l++) {
          for(int nd=1; nd <= constants.nvector; nd++) {
            idx_vr_rtm = (3*nd-1) + constants.ncomp*(l_rtm)*constants.istep_rtm[1] + k*constants.istep_rtm[0] + (mp_rlm-1)*constants.istep_rtm[2];
            for(int j_rlm=jst; j_rlm<=jed; j_rlm++) {
            if(m==0) 
              *fp << m << "\t" << l << "\t" << j << "\t" << k+1 << "\t" << l_rtm+1 << "\t" << data->g_colat_rtm[l_rtm] << "\t" << data->g_sph_rlm[j_rlm] << "\t" << data->P_smdt[l_rtm + constants.nidx_rtm[1]*j] << "\t" << data->dP_smdt[l_rtm + constants.nidx_rtm[1]*j] << "\t" << data->vr_rtm[idx_vr_rtm-2] << "\t" << data->vr_rtm[idx_vr_rtm-1] << "\t" << data->vr_rtm[idx_vr_rtm] << "\n"; 
            else if(m==1)
              *fp << m << "\t" << l << "\t" << j << "\t" << k+1 << "\t" << l_rtm+1 << "\t" << data->g_colat_rtm[l_rtm] << "\t" << data->g_sph_rlm[j_rlm] << "\t" << data->P_smdt[l_rtm + constants.nidx_rtm[1]*j] << "\t" << "\t" << "\t" << "\n"; 
           }
         }
       }
      }
    }
  }
#endif
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
 clockD->close();
}

void initDevConstVariables() {
  cudaError_t error;
  error = cudaMemcpyToSymbol(devConstants, &constants, sizeof(Geometry_c), 0, cudaMemcpyHostToDevice);
  cudaErrorCheck(error);
}
