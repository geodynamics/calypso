//Author: Harsha V. Lokavarapu

#include <cstdlib>
#include "helper_cuda.h"
#include <string>
#include <math.h>
#include "cuda.h"
#include <fstream>
#include <mpi.h>
#ifdef CUDA_TIMINGS
  #include "cuda_profiler_api.h"
#endif
#define ARGC 3 

/*#if __CUDA_ARCH__ < 350 
#error "Incompatable compute capability for sm. Using dynamic parallelism (>= 35)"
#endif
*/
//Function and variable declarations.
extern int nComp;
//CPU pointers to GPU memory data

// Fortran function calls
extern "C" {
  void transform_f_(int*, int*, int*);
  void legendre_b_trans_vector_cuda_(int*, int*, int*);
}

//Fortran Variables
typedef struct {
  int nidx_rtm[3];
  int nidx_rlm[2];
  int istep_rtm[3];
  int istep_rlm[2];
  int nnod_rtp;
  int nnod_rlm;
  int nnod_rtm;
  int ncomp;
  int nscalar;
  int nvector;
  int t_lvl;
} Geometry_c;

//Cublas library/Cuda variables
extern cudaError_t error;
extern cudaStream_t streams[32];

//Helper functions, declared but not defined. 

extern void cudaErrorCheck(cudaError_t error);
extern void cudaErrorCheck(cufftResult error);

typedef unsigned int uint;

typedef struct 
{
  // OLD: 0 = g_point_med, 1 =  double* g_colat_med, 2 = double* weight_med;
  // Current: 0 = vr_rtm,  = g_sph_rlm
  double *vr_rtm, *g_colat_rtm, *g_sph_rlm;
  double *sp_rlm;
  double *a_r_1d_rlm_r; //Might be pssible to copy straight to constant memory
  double *asin_theta_1d_rtm;
  int *idx_gl_1d_rlm_j;
  int *lstack_rlm;
  #ifdef CUDA_STATIC
    double *p_jl;
    double *dP_jl;
  #endif
} Parameters_s;

typedef struct 
{
  double *P_smdt; 
  double *dP_smdt;
  double *g_sph_rlm;
  int *lstack_rlm;
  int *idx_gl_1d_rlm_j;
  double *g_colat_rtm;
  double *vr_rtm;
// Dim: jx3
} Debug;

extern Parameters_s deviceInput;
extern Debug h_debug, d_debug;
extern Geometry_c constants;

//FileStreams: For debugging and Timing
//D for debug
extern std::ofstream clockD;

// Counters for forward and backward Transform
extern double countFT, countBT;

/*
 *   Set of variables that take advantage of constant memory.
 *     Access to constant memory is faster than access to global memory.
 *       */

__constant__ Geometry_c devConstants;

////////////////////////////////////////////////////////////////////////////////
//! Function Defines
////////////////////////////////////////////////////////////////////////////////
extern "C" {

//void initgpu_(int *nnod_rtp, int *nnod_rtm, int *nnod_rlm, int nidx_rtm[], int nidx_rtp[], int istep_rtm[], int istep_rlm[], int *ncomp, double *a_r_1d_rlm_r, int lstack_rlm[], double *g_colat_rtm, int *trunc_lvl, double *g_sph_rlm);
void initgpu_(int *nnod_rtp, int *nnod_rtm, int *nnod_rlm, int nidx_rtm[], int nidx_rtp[], int istep_rtm[], int istep_rlm[], int *ncomp, int *t_lvl, int *nscalar, int *nvector);
void setptrs_(int *idx_gl_1d_rlm_j);
void finalizegpu_(); 
void initDevConstVariables();

void allocMemOnGPU();
void memcpy_h2d_(int *lstack_rlm, double *a_r, double *g_colat, double *g_sph_rlm, double *asin_theta_1d_rtm, int *idx_gl_1d_rlm_j);
void deAllocMemOnGPU();
void deAllocDebugMem();
void allocHostDebug(Debug*);
void allocDevDebug(Debug*);
void cpy_dev2host_4_debug_();
void cpy_schmidt_2_gpu_(double *P_jl, double *dP_jl);
void set_spectrum_data_(double *sp_rlm);
void set_physical_data_(double *vr_rtm);
void retrieve_spectrum_data_(double *sp_rlm);
void retrieve_physical_data_(double *vr_rtm);

void writeDebugData2File(Debug*, std::string);
void check_bwd_trans_cuda_(int*, double*, double*, double*);
void cleangpu_();

__device__ double nextLGP_m_eq0(int l, double x, double p_0, double p_1);
__device__ double nextDp_m_eq_0(int l, double lgp_mp);
__device__ double nextDp_m_eq_1(int l, double p_mn_l, double p_pn_l);
__device__ double nextDp_m_l(int m, int l, double p_mn, double p_pn);
__device__ double calculateLGP_m_eq_l(int mode);
__device__ double calculateLGP_mp1_eq_l(int mode, double x, double lgp_m_eq_l);
__device__ double calculateLGP_m_l(int m, int degree, double theta, double lgp_0, double lgp_1);
__device__ double scaleBySine(int mode, double lgp, double theta);
}

__global__ void transB_m_l_eq0_ver1D(int mp_rlm, int jst, int jed, double *vr_rtm,  double const* __restrict__ sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *P_smdt, double *dP_smdt, double *g_sph_rlm, double *asin_theta_1d_rtm); 
__global__ void transB_m_l_eq1_ver1D( int mp_rlm,  int jst,  int jed, int order, int degree, double *vr_rtm, double const* __restrict__ sp_rlm, double *a_r_1d_rlm_r,     double *g_colat_rtm, double *P_smdt, double *dP_smdt, double *g_sph_rlm, double *asin_theta_1d_rtm);
__global__ void transB_m_l_ver1D( int mp_rlm,  int jst,  int jed, int order, int degree, double *vr_rtm,  double *sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *P_smdt, double *dP_smdt, double *g_sph_rlm, double *asin_theta_1d_rtm);
__global__ void transB_m_l_ver2D(int *lstack_rlm, int m0, int m1, int *idx_gl_1d_rlm_j, double *vr_rtm, double *sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *P_smdt, double *dP_smdt, double *g_sph_rlm, double *asin_theta_1d_rtm);
__global__ void transB_m_l_ver3D(int *lstack_rlm, int m0, int m1, int *idx_gl_1d_rlm_j, double *vr_rtm, double *sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *P_smdt, double *dP_smdt, double *g_sph_rlm, double *asin_theta_1d_rtm);
__global__ void transB_m_l_ver4D(int *lstack_rlm, int m0, int m1, int *idx_gl_1d_rlm_j, double *vr_rtm, double const* __restrict__ sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *P_smdt, double *dP_smdt, double *g_sph_rlm, double *asin_theta_1d_rtm);
__global__ void transB_m_l_ver5D(int *lstack_rlm, int *idx_gl_1d_rlm_j, double *vr_rtm, double const* __restrict__ sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *P_smdt, double *dP_smdt, double *g_sph_rlm, double *asin_theta_1d_rtm);
__global__ void transB_m_l_ver6D(int *lstack_rlm, int m0, int m1, int *idx_gl_1d_rlm_j, double *vr_rtm, double const* __restrict__ sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *P_jl, double *dP_jl, double *g_sph_rlm, double *asin_theta_1d_rtm);
__global__ void transB(double *vr_rtm, const double *sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm); 
