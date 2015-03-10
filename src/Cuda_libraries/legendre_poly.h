//Author: Harsha V. Lokavarapu

#include <cstdlib>
#include "helper_cuda.h"
#include <cufft.h>
#include <curand.h>
#include <string>
#include <math.h>
#include "cuda.h"
#include <iostream>
#include <fstream>

#define ARGC 3 

/*#if __CUDA_ARCH__ < 350 
#error "Incompatable compute capability for sm. Using dynamic parallelism (>= 35)"
#endif
*/
//Function and variable declarations.
//CPU pointers to GPU memory data

/*
 *   Set of variables that take advantage of constant memory.
 *     Access to constant memory is faster than access to global memory.
 *       */

// Fortran function calls
extern "C" {
  //void inputcalypso_(int*, int*, double*, double*, double*);
  //void cleancalypso_();
  void transform_f_(int*, int*, int*);
  void transform_b_(int*, int*, int*);
}

//Fortran Variables
extern "C" {
  int nidx_rtm[3];
  int nidx_rlm[3];
  int ncomp_trans;
}

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
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm
  int argc; 
  double** argv;
} Parameters_s;

typedef struct 
{
  //Order:*P_smdt, *dPdt_smdt, *P_org
  int argc, l;
  double **argv;
} Debug;

extern Parameters_s deviceInput;
extern Debug h_schmidt, d_schmidt;
////////////////////////////////////////////////////////////////////////////////
//! Function Defines
////////////////////////////////////////////////////////////////////////////////
extern "C" {

void initGpu();
 
void initDevConstVariables(int *nidx_rlm, int *nidx_rtm);

void allocMemOnGPU();
void deAllocMemOnGPU();
void allocHostDebug(Debug*);
void allocDevDebug(Debug*);
void cpyDev2Host();
void writeDebugData(std::ofstream*, double**);

void doWork(int order, int degree);
void doWorkDP(int);
__global__
void cuSHT(int order, int degree, double *, double *);

//__global__
//void cuSHT1(int order, int degree, int, double **, int, double**, int);
__global__
void cuSHT1(int order, int degree, int, double*, double*, double*, double*, double*, double*);

__global__
void cuSHT_m0(int order, int degree, int, double*, double*, double*, double*, double*, double*);

__global__
void diffSchmidt(int degree, int theta, double *P_smdt, double *dP_smdt);

__device__
double cuLGP(int, int, double);

void cleanup_();
}
