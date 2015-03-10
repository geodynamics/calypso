#include <cuda_runtime.h>
#include "legendre_poly.h"
#include "math_functions.h"
#include "math_constants.h"
#include <math.h>

__global__
void transB(double *vr_rtm, double *sp_rlm, double *g_sph_rlm) {

  int nidx_theta = blockIdx.x;
  
}

void transform_b_(int *ncomp, int *nvector, int *nscalar) {
  dim3 grid(nidx_rtm[1]);
  dim3 block(nidx_rtm[0]);  
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm
  transB<<<grid, block>>> (deviceInput.argv[0], deviceInput.argv[1], deviceInput.argv[2]);
}
