#include <cuda_runtime.h>
#include "legendre_poly.h"
#include "math_functions.h"
#include "math_constants.h"
#include <sstream>

__device__
double nextLGP_m_eq0(int l, double x, double p_0, double p_1) {
// x = cos(theta)
  return __dadd_rd(__dmul_rd(__dmul_rd(p_1, __ddiv_rd(2*l-1, l)),x), __dmul_rd(p_0, __ddiv_rd(l-1, l))*-1); 
}

__device__
double nextDp_m_eq_0(int l, double lgp_mp) {
   return __dmul_rd(-1*lgp_mp, __dsqrt_rd(l*(l+1)/2)); 
}

__device__
double nextDp_m_eq_1(int l, double lgp_mn, double lgp_mp) {
  return __dmul_rd(0.5, __dadd_rd(__dmul_rd(__dsqrt_rd(2*l*(l+1)), lgp_mn),-1 * __dmul_rd(__dsqrt_rd((l-1)*(l+2)), lgp_mp) ));
}

__device__
double nextDp_m_l(int m, int l, double lgp_mn, double lgp_mp) {
  return __dmul_rd(0.5, __dadd_rd(__dmul_rd(__dsqrt_rd((l+m)*(l-m+1)), lgp_mn), -1*__dmul_rd(__dsqrt_rd((l-m)*(l+m+1)), lgp_mp)));
}

__device__
double calculateLGP_m_eq_l(int mode) {
  double lgp=1;
  for(int k=1; k<=abs(mode); k++) {
    lgp *= __ddiv_ru((double)2*k-1, (double)2*k);
  }
  
  return __dsqrt_rd(__dmul_rd(2, lgp));
}

__device__ 
double calculateLGP_mp1_eq_l(int m, double x, double lgp_m_eq_l) {
  int mode = abs(m);
  return __dmul_rd(__dmul_rd(lgp_m_eq_l, __dsqrt_rd(2*mode+1)),x); 
}

__device__
double calculateLGP_m_l(int mode, int degree, double theta, double lgp_0, double lgp_1) {
  int m = abs(mode);
  return  __ddiv_rd(__dadd_rd(__dmul_rd(2*degree-1, __dmul_rd(cos(theta), lgp_1)), __dmul_rd(-1 * lgp_0, __dsqrt_rd((degree-1)*(degree-1) - m*m))), __dsqrt_rd((degree*degree) - (m*m)));
}

__device__
double scaleBySine(int m, double lgp, double theta) {
  double reg1 = sin(theta);
  for(int r=0; r<abs(m); r++)
    lgp = __dmul_rd(lgp,reg1); 
  return lgp;
}

__global__
void transB(double *vr_rtm, const double* __restrict__ sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm) {
  unsigned int id = threadIdx.x;
  // The work is parallelized over theta within a grid
  int nTheta = devConstants.nidx_rtm[1];
  unsigned int workLoad = nTheta/(blockDim.x);
  if( nTheta%blockDim.x > (threadIdx.x)) 
    workLoad++;

  int order=0, deg=0, j[3]={0,0,0};
  
  // P(m,m)[cos theta]
  
// **** Memory needs to be freed at the end of the function
  double *p_mn_l_0 = (double*) malloc (workLoad * 8);
  double *p_mn_l_1 = (double*) malloc (workLoad * 8);
  double *p_m_l_0 = (double*) malloc (workLoad * 8);
  double *p_m_l_1 = (double*) malloc (workLoad * 8);
  double *p_mp_l_0 = (double*) malloc (workLoad * 8);
  double *p_mp_l_1 = (double*) malloc (workLoad * 8);
  double *dp_m_l_0 = (double*) malloc (workLoad * 8);
  double *dp_m_l_1 = (double*) malloc (workLoad * 8);
 
  double x=1, theta=0;
  double g_sph_rlm_dev[3] = {0,0,0};
// 3 for m-1, m, m+1
  unsigned int idx[3] = {0,0,0}, idx_rtm[3] = {0,0,0};
  double reg1, reg2, reg3;

  double *vr_reg = (double*) malloc (3*sizeof(double)*devConstants.nvector);

  //base case, m=0,1 l=0...t_lvl
  for(int i=0; i<workLoad; i++) {
    theta = g_colat_rtm[threadIdx.x + i*blockDim.x];
    x = cos(theta);
    memset(vr_reg, 0, 3*devConstants.nvector);
    // m=0, l=0 && m=0, l=1
    p_m_l_0[i*blockDim.x + id] = 1;
    p_m_l_1[i*blockDim.x + id] = x;
    // m=1, l=1 && m=1, l=2
    p_mp_l_0[i*blockDim.x + id] = calculateLGP_m_eq_l(1);
    p_mp_l_1[i*blockDim.x + id] = calculateLGP_mp1_eq_l(1, x, p_mp_l_0[i*blockDim.x + id]);
    // m=0, l=0 && m=0, l=1
    dp_m_l_0[i*blockDim.x + id] = 0;
    dp_m_l_1[i*blockDim.x + id] = nextDp_m_eq_0(1, scaleBySine(1, p_mp_l_0[i*blockDim.x + id], theta));

    for(int l=order; l<=devConstants.t_lvl; l++) {
      j[1] = l*(l+1) + 0;
      j[2] = l*(l+1) + 1;

      idx[1] = devConstants.ncomp * (j[1] * devConstants.istep_rlm[1] + blockIdx.x * devConstants.istep_rlm[0]); 
      idx[2] = devConstants.ncomp * (j[2] * devConstants.istep_rlm[1] + blockIdx.x * devConstants.istep_rlm[0]); 

      g_sph_rlm_dev[1] = (double) l*(l+1); 
      
      for(int t=1; t<=devConstants.nvector; t++) {
        idx[1] += 3;
        idx[2] += 3;

        vr_reg[t*3 - 3] += sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[blockIdx.x] * p_m_l_0[blockIdx.x*i + id] * g_sph_rlm_dev[1];    
        vr_reg[t*3 - 2] += sp_rlm[idx[1] - 2]  * a_r_1d_rlm_r[blockIdx.x] * dp_m_l_0[blockIdx.x*i + id];    
        vr_reg[t*3 - 1] -= sp_rlm[idx[1] - 3] * a_r_1d_rlm_r[blockIdx.x] * a_r_1d_rlm_r[blockIdx.x] * dp_m_l_0[blockIdx.x*i + id];    

      }

      reg1 = nextLGP_m_eq0(l+2, x, p_m_l_0[i*blockDim.x + id], p_m_l_1[i*blockDim.x + id]); 
      p_m_l_0[i*blockDim.x + id] = p_m_l_1[blockDim.x + id];
      p_m_l_1[i*blockDim.x + id] = reg1;

      reg2 = calculateLGP_m_l(1, l+3, theta, p_mp_l_0[blockIdx.x + id], p_mp_l_1[blockIdx.x + id]);
      p_mp_l_0[i*blockDim.x + id] = p_mp_l_1[blockIdx.x + id];
      p_mp_l_1[i*blockDim.x + id] = reg2;

      dp_m_l_0[i*blockDim.x + id] = dp_m_l_1[i*blockDim.x + id];
      dp_m_l_1[i*blockDim.x + id] = nextDp_m_eq_0(1, scaleBySine(1, p_mp_l_0[i*blockDim.x + id], theta));
        
    }
    
    idx_rtm[0] = devConstants.ncomp * ((threadIdx.x + i*blockDim.x) * devConstants.istep_rtm[1] + blockIdx.x*devConstants.istep_rtm[0] + ((devConstants.t_lvl + 1)-1) * devConstants.istep_rtm[2]); 
    for(int t=1; t<=devConstants.nvector; t++) {
      idx_rtm[0] += 3;
      vr_rtm[idx_rtm[0] - 2 - 1]  += vr_reg[t*3 - 3]; 
      vr_rtm[idx_rtm[0] - 1 - 1]  += vr_reg[t*3 - 2]; 
      vr_rtm[idx_rtm[0] - 1]  += vr_reg[t*3 - 1]; 
    }
  }     
   
  free(p_mn_l_0);
  free(p_mn_l_1);
  free(p_m_l_0);
  free(p_m_l_1);
  free(p_mp_l_0);
  free(p_mp_l_1);
  free(dp_m_l_0);
  free(dp_m_l_1);
  
  // asin_theta_1d_rtm = 1/sin(g_colat_rtm(n_theta)) 
  //Associated Leg Poly for l, m=l
  // ... for l, m=(l-1)
  /*
  for(int k=1; k<=(order-1); k++)
    c_0[1] *= __ddiv_ru((double)2*k-1, (double)2*k);
  c_0[0] = __dmul_ru(c_0[1], __ddiv_ru((double)2*k-1, (double)2*k));

  // 
  for(int i=0; i<workLoad; i++) {
    x = cos(g_colat_rtm[threadIdx.x + i*blockDim.x]); 
    p_0[i] = __dsqrt_rd(__dmul_rd(2, c_0));
    reg1 = p_1[i] =  __dmul_rd(cos(x), __dsqrt_rd(__dmul_rd(2*(2*order+1), c_0)));
      
      
  }
 
  int m=0, l=0;
 
    
  int idx=0, j=0;
  double c_1=c1(order),x=0, c_0=1;
  double reg1=0, reg2=0;
  for(int k=1; k<=order; k++) 
    c_0 *= __ddiv_ru((double)2*k-1, (double)2*k);
  
  
  for(int i=0; i<workLoad; i++) {
    idx = id+i*blockDim.x;
    reg1 = p_0[i] = __dsqrt_rd(__dmul_rd(2, c_0));
    j = l*(l+1) + order;
    x = g_colat[idx];
    for(int k=0; k<order; k++)
      reg1 *= sin(x);
    P_smdt[idx + j*theta] = reg1;
//    lgp += p_0[i] * weights[j];
  }
   */  
  /*vector[id] = lgp;
  __syncthreads();
  int count=0;
  //call convolution kernel
  for(lgp=0; count<blockDim.x; count++) {
    lgp += vector[count];
  }*/

  /*l++;
  for(int i=0; i<workLoad && l <= degree; i++) {
    idx = id+i*blockDim.x;
    x = g_colat[idx];
    reg1 = p_1[i] =  __dmul_rd(cos(x), __dsqrt_rd(__dmul_rd(2*(2*order+1), c_0)));
    j = l*(l+1) + order;
    for(int k=0; k<order; k++)
      reg1 *= sin(x);
    P_smdt[idx + j*theta] = reg1;
    //lgp += p_1[i] * weights[j];
  }
 */ 
/*  vector[id]=lgp;
  __syncthreads();
  //call convolution kernel
  if (l <= degree) {
    for(count=0,lgp=0; count<blockDim.x; count++) {
      lgp += vector[count];
    }
  }
  else
   return;
*/
  /*l++;

  double p_2=0, c_2=0;
  for(; l <= degree; l++) {
    c_1 = c1(l);
    c_2 = c2(order, l);
    for(int i=0; i<workLoad; i++) {
      idx = id+i*blockDim.x;
      x = g_colat[idx];
      p_2 = __ddiv_rd(__dadd_rd(__dmul_rd(c_1, __dmul_rd(cos(x), p_1[i])), __dmul_rd(p_0[i], c_2)), __dsqrt_rd((l*l) - (order*order)));
      p_0[i] = p_1[i];
      p_1[i] = p_2;
      for(int m=0; m<order; m++)
        p_2 *= sin(x);
      j = l*(l+1) + order;
      P_smdt[idx + j*theta] = p_2;
      //lgp+= p_2*weights[j];
    } */
/*
    vector[id]= lgp;
    __syncthreads();
    //call convolution kernel
    for(count=0,lgp=0; count<blockDim.x; count++) {
      lgp += vector[count];
    }
*/
  //}

}

__global__
void transB_m_l_eq0_ver1D(const int mp_rlm, const int jst, const int jed, double *vr_rtm, double const* __restrict__ sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *P_smdt, double *dP_smdt, double *g_sph_rlm, double *asin_theta_1d_rtm) {
  unsigned int id = threadIdx.x;
  // The work is parallelized over theta within a grid
  int nTheta = devConstants.nidx_rtm[1];
  unsigned int workLoad = nTheta/(blockDim.x);
  if( nTheta%blockDim.x > (threadIdx.x)) 
    workLoad++;

  int order=0, j=0;
  // P(m,m)[cos theta]
  
// **** Memory needs to be freed at the end of the function
  double p_m_l_0, p_m_l_1;
  double p_mp_l_0, p_mp_l_1;
  double dp_m_l;
 
  double x=1, theta=0;
  unsigned int idx = 0, idx2 = 0, idx_rtm[2] = {0,0};
  double reg1, reg2;

  double *vr_reg = (double*) malloc (5*sizeof(double)*devConstants.nvector);
  double *vr_reg_scalar = (double*) malloc (sizeof(double)*devConstants.nscalar);

  int mn_rlm = devConstants.nidx_rtm[2] - mp_rlm + 1;

  int l=0;
  //base case, m=0,1 l=0...t_lvl
  for(int i=0; i<workLoad; i++) {
    l = 0;
    theta = g_colat_rtm[threadIdx.x + i*blockDim.x];
    x = cos(theta);
    for(int rt=0; rt<5*devConstants.nvector;rt++)
      vr_reg[rt]=0;
    for(int rt=0; rt<devConstants.nscalar;rt++)
      vr_reg_scalar[rt]=0;
    // m=0, l=0 && m=0, l=1
    p_m_l_0 = 1;
    #ifdef CUDA_DEBUG
      P_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + 0] = p_m_l_0;
    #endif
    p_m_l_1 = x;
    // m=1, l=1 && m=1, l=2
    p_mp_l_0 = calculateLGP_m_eq_l(1);
    p_mp_l_1 = calculateLGP_mp1_eq_l(1, x, p_mp_l_0);
    // m=0, l=0 
    dp_m_l = 0;
    #ifdef CUDA_DEBUG
      dP_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + 0] = dp_m_l;
    #endif
    dp_m_l = nextDp_m_eq_0(1, scaleBySine(1, p_mp_l_0, theta));

    for(int j_rlm=jst; j_rlm<=jed; j_rlm++, l++) {
      idx = devConstants.ncomp * ((j_rlm-1) * devConstants.istep_rlm[1] + blockIdx.x * devConstants.istep_rlm[0]); 
      idx2 = 3*devConstants.nvector + idx; 
      for(int t=1; t<=devConstants.nvector; t++) {
        idx += 3;
        reg2 = __dmul_rd(__dmul_rd(-1 * p_m_l_0, (double) order), asin_theta_1d_rtm[id + i*blockDim.x]);         
        vr_reg[t*5 - 5] += sp_rlm[idx - 1] * a_r_1d_rlm_r[blockIdx.x] * reg2;
        vr_reg[t*5 - 4] += sp_rlm[idx - 2] * a_r_1d_rlm_r[blockIdx.x] * reg2;
        vr_reg[t*5 - 3] += sp_rlm[idx - 3] * __dmul_rd(a_r_1d_rlm_r[blockIdx.x], a_r_1d_rlm_r[blockIdx.x]) * p_m_l_0 * g_sph_rlm[j_rlm-1];    
        vr_reg[t*5 - 2] += sp_rlm[idx - 2]  * a_r_1d_rlm_r[blockIdx.x] * dp_m_l;    
        vr_reg[t*5 - 1] -= sp_rlm[idx - 1] * a_r_1d_rlm_r[blockIdx.x] * dp_m_l;
      }
      for(int t=1; t<=devConstants.nscalar; t++) {
        idx2 += 1;
        vr_reg_scalar[t-1] += sp_rlm[idx2 - 1] * p_m_l_0;
      }

      //m=0, l=l+2
      reg1 = nextLGP_m_eq0(l+2, x, p_m_l_0, p_m_l_1); 
      //P_smdt is set to be nTheta x nJ
      //m=0, l=l+1
      j = (l+1)*(l+2) + 0;
      p_m_l_0 = p_m_l_1;
      #if defined(CUDA_DEBUG)
      if(l<=devConstants.t_lvl)
        P_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = p_m_l_0; 
      #endif
      p_m_l_1 = reg1;

      //m=0, l=l+1
      j = (l+1)*(l+2) + 0;
      dp_m_l = nextDp_m_eq_0(l+1, scaleBySine(1, p_mp_l_0, theta));
      #ifdef CUDA_DEBUG
      if(l<=devConstants.t_lvl)
        dP_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = dp_m_l;
      #endif

      //m=1, l=l+3
      reg2 = calculateLGP_m_l(1, l+3, theta, p_mp_l_0, p_mp_l_1);
      p_mp_l_0 = p_mp_l_1;
      p_mp_l_1 = reg2;
        
    }
     
    reg2 = (threadIdx.x + i*blockDim.x) * devConstants.istep_rtm[1] + blockIdx.x*devConstants.istep_rtm[0]; 
    idx_rtm[0] = devConstants.ncomp * (reg2 + (mp_rlm-1) * devConstants.istep_rtm[2]); 
    idx_rtm[1] = devConstants.ncomp * (reg2 + (mn_rlm-1) * devConstants.istep_rtm[2]); 
    idx2 = devConstants.nvector*3 + idx_rtm[0]; 
    for(int t=1; t<=devConstants.nvector; t++) {
      idx_rtm[0] += 3;
      idx_rtm[1] += 3;
      vr_rtm[idx_rtm[0] - 2 - 1]  += vr_reg[t*5 - 3]; 
      vr_rtm[idx_rtm[0] - 1 - 1]  += vr_reg[t*5 - 2]; 
      vr_rtm[idx_rtm[0] - 1]  += vr_reg[t*5 - 1]; 
      vr_rtm[idx_rtm[1] - 1 - 1] += vr_reg[t*5 - 5];
      vr_rtm[idx_rtm[1] - 1] += vr_reg[t*5 - 4];  
    }
    for(int t=1; t<=devConstants.nscalar; t++) {
      idx2 += 1;
      vr_rtm[idx2 - 1] = vr_reg_scalar[t-1];
    } 
  }     
   
  free(vr_reg);
  free(vr_reg_scalar);
}

__global__
void transB_m_l_eq1_ver1D(const int mp_rlm, const int jst, const int jed, int order, int degree, double *vr_rtm, double const* __restrict__ sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *P_smdt, double *dP_smdt, double *g_sph_rlm, double *asin_theta_1d_rtm) {
  unsigned int id = threadIdx.x;
  // The work is parallelized over theta within a grid
  int nTheta = devConstants.nidx_rtm[1];
  unsigned int workLoad = nTheta/(blockDim.x);
  if( nTheta%blockDim.x > (threadIdx.x)) 
    workLoad++;

  int deg=0, j=0;
  // P(m,m)[cos theta]
  
  double p_mn_l_0, p_mn_l_1;
  double p_m_l_0, p_m_l_1;
  double p_mp_l_0, p_mp_l_1;
  double dp_m_l;
 
  double x=1, theta=0;
// 3 for m-1, m, m+1
  unsigned int idx[3] = {0,0,0}, idx_rtm[3] = {0,0,0};
  double reg1, reg2, reg3;

  double *vr_reg = (double*) malloc (5*sizeof(double)*devConstants.nvector);
  double *vr_reg_scalar = (double*) malloc (sizeof(double)*devConstants.nscalar);

  int mn_rlm = devConstants.nidx_rtm[2] - mp_rlm + 1;

  for(int i=0; i<workLoad; i++) {
    theta = g_colat_rtm[threadIdx.x + i*blockDim.x];
    x = cos(theta);
    // deg = 1 = order
    deg = abs(degree);
    for(int rt=0; rt<5*devConstants.nvector;rt++)
      vr_reg[rt]=0;
    for(int rt=0; rt<devConstants.nscalar;rt++)
      vr_reg_scalar[rt]=0;
    
    //m=0, l=0
    p_mn_l_0 = 1;
    //m=0, l=1
    p_mn_l_1 = x;

    //m=l=1
    p_m_l_0 = calculateLGP_m_eq_l(1); 
    #ifdef CUDA_DEBUG
      j = degree * (degree+1) + order;
      P_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = scaleBySine(1, p_m_l_0, theta);
    #endif
    //m = 1, l=2
    p_m_l_1 = calculateLGP_mp1_eq_l(1, x, p_m_l_0);

   //m = l = 2
    p_mp_l_0 = calculateLGP_m_eq_l(2);
   //m=2, l=3
    p_mp_l_1 = calculateLGP_mp1_eq_l(2, x, p_mp_l_0);

    //m=l=1
    dp_m_l = p_mn_l_1;
    #ifdef CUDA_DEBUG
      j = degree * (degree+1) + order;
      dP_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = dp_m_l; 
    #endif

    for(int j_rlm=jst, deg=degree; j_rlm<=jed; j_rlm++, deg++) {
      idx[1] = devConstants.ncomp * ((j_rlm-1) * devConstants.istep_rlm[1] + blockIdx.x * devConstants.istep_rlm[0]); 
      idx[0] = devConstants.nvector*3 + idx[1]; 
      reg1 = scaleBySine(order, p_m_l_0, theta);
      for(int t=1; t<=devConstants.nvector; t++) {
        idx[1] += 3;
        reg2 = __dmul_rd(__dmul_rd(-1 * reg1, (double) order), asin_theta_1d_rtm[id + i*blockDim.x]);         
        vr_reg[t*5 - 5] += sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[blockIdx.x] * reg2;
        vr_reg[t*5 - 4] += sp_rlm[idx[1] - 2] * a_r_1d_rlm_r[blockIdx.x] * reg2;
        vr_reg[t*5 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[blockIdx.x], a_r_1d_rlm_r[blockIdx.x]) * reg1 * g_sph_rlm[j_rlm-1];    
        vr_reg[t*5 - 2] += sp_rlm[idx[1] - 2]  * a_r_1d_rlm_r[blockIdx.x] * dp_m_l;    
        vr_reg[t*5 - 1] -= sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[blockIdx.x] * dp_m_l;    
      }
      for(int t=1; t<=devConstants.nscalar; t++) {
        idx[0] += 1;
        vr_reg_scalar[t-1] += sp_rlm[idx[0] - 1] * reg1;
      }

      // Initially l and m are set to 1
      //m=0, l = l+1 
      reg1 = nextLGP_m_eq0(deg+1, x, p_mn_l_0, p_mn_l_1); 
      p_mn_l_0 = p_mn_l_1;
      p_mn_l_1 = reg1;

      //m=1, l = l+2
      reg2 = calculateLGP_m_l(order, deg+2, theta, p_m_l_0, p_m_l_1);
      p_m_l_0 = p_m_l_1;
      //setting debug data for m=1, l = l+1
      #ifdef CUDA_DEBUG
        if(deg <= devConstants.t_lvl) {
          j = (deg+1)*(deg+2) + order;
          P_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = scaleBySine(order, p_m_l_0, theta);
        }
      #endif
      p_m_l_1 = reg2;
 
      //m=1, l = l+1
      dp_m_l = nextDp_m_eq_1(deg+1, p_mn_l_1, scaleBySine(order+1, p_mp_l_0, theta));
       
      #ifdef CUDA_DEBUG
        if(deg<=devConstants.t_lvl) {
          j = (deg+1)*(deg+2) + order;
          dP_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = dp_m_l; 
        }
      #endif

      //m=order+1, l=l+3
      reg3 = calculateLGP_m_l(order+1, deg+3, theta, p_mp_l_0, p_mp_l_1);  
      p_mp_l_0 = p_mp_l_1;
      //setting debug info for m=order+1, l=l+2
      //j = (deg+2)*(deg+3) + order+1;
 //     P_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = scaleBySine(order+1, p_mp_l_0, theta);
      p_mp_l_1 = reg3;
        
    }
    
    reg1 = (threadIdx.x + i*blockDim.x) * devConstants.istep_rtm[1] + blockIdx.x*devConstants.istep_rtm[0];
    idx_rtm[0] = devConstants.ncomp * (reg1 + (mp_rlm-1) * devConstants.istep_rtm[2]); 
    // mn_rlm
    idx_rtm[1] = devConstants.ncomp * (reg1 + (mn_rlm-1) * devConstants.istep_rtm[2]); 
    idx_rtm[2] = 3*devConstants.nvector + idx_rtm[0]; 
    for(int t=1; t<=devConstants.nvector; t++) {
      idx_rtm[0] += 3;
      idx_rtm[1] += 3;
      vr_rtm[idx_rtm[0] - 2 - 1]  += vr_reg[t*5 - 3]; 
      vr_rtm[idx_rtm[0] - 1 - 1]  += vr_reg[t*5 - 2]; 
      vr_rtm[idx_rtm[0] - 1]  += vr_reg[t*5 - 1]; 
      vr_rtm[idx_rtm[1] - 1 - 1] += vr_reg[t*5 - 5];
      vr_rtm[idx_rtm[1] - 1] += vr_reg[t*5 - 4];  
    }
    for(int t=1; t<=devConstants.nscalar; t++) {
      idx_rtm[2] += 1;
      vr_rtm[idx_rtm[2] - 1] = vr_reg_scalar[t-1];
    }
  }     
   
  free(vr_reg);
  free(vr_reg_scalar);
}

__global__
void transB_m_l_ver1D(const int mp_rlm, const int jst, const int jed, int order, int degree, double *vr_rtm, double *sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *P_smdt, double *dP_smdt, double *g_sph_rlm, double *asin_theta_1d_rtm) {
  unsigned int id = threadIdx.x;
  // The work is parallelized over theta within a grid
  int nTheta = devConstants.nidx_rtm[1];
  unsigned int workLoad = nTheta/(blockDim.x);
  if( nTheta%blockDim.x > (threadIdx.x)) 
    workLoad++;

  int deg, j;

  // P(m,m)[cos theta]
  double p_mn_l_0, p_mn_l_1;
  double p_m_l_0, p_m_l_1;
  double p_mp_l_0, p_mp_l_1;
  double dp_m_l;
 
  double x=1, theta=0;
// 3 for m-1, m, m+1
  unsigned int idx[3] = {0,0,0}, idx_rtm[3] = {0,0,0};
  double reg1, reg2, reg3;
 
  double norm=0;

  double *vr_reg = (double*) malloc (sizeof(double)*5*devConstants.nvector);
  double *vr_reg_scalar = (double*) malloc (sizeof(double)*devConstants.nscalar);

  double mn_rlm = devConstants.nidx_rtm[2] - mp_rlm + 1;

  for(int i=0; i<workLoad; i++) {
    theta = g_colat_rtm[threadIdx.x + i*blockDim.x];
    x = cos(theta);
    deg = degree;

    for(int rt=0; rt<devConstants.nvector*5;rt++)
      vr_reg[rt]=0;
    for(int rt=0; rt<devConstants.nscalar;rt++)
      vr_reg_scalar[rt]=0;
   
    // m-1,l-1
    p_mn_l_0 = calculateLGP_m_eq_l(abs(order)-1);
    //P_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = scaleBySine(order-1, p_mn_l_0, theta);
    // m-1,l
    p_mn_l_1 = calculateLGP_mp1_eq_l(abs(order)-1, x, p_mn_l_0);

    // m,l
    p_m_l_0 = calculateLGP_m_eq_l(order); 
    
    #ifdef CUDA_DEBUG
    j = degree*(degree+1) + order;
    P_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = scaleBySine(order, p_m_l_0, theta);
    #endif

    // m,l+1
    p_m_l_1 = calculateLGP_mp1_eq_l(order, x, p_m_l_0);

    // m+1,l+1 
    p_mp_l_0 = calculateLGP_m_eq_l(abs(order)+1);
    // m+1,l+2
    p_mp_l_1 = calculateLGP_mp1_eq_l(abs(order)+1, x, p_mp_l_0);

    // m,l
    dp_m_l = __dmul_rd(0.5, __dmul_rd(__dsqrt_rd(2*abs(order)), scaleBySine(abs(order)-1, p_mn_l_1, theta)));
    #ifdef CUDA_DEBUG
      j = degree*(degree+1) + order;
      dP_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = dp_m_l; 
    #endif

    for(int j_rlm=jst, deg=degree; j_rlm<=jed; j_rlm++, deg++) {
        idx[1] = devConstants.ncomp * ((j_rlm-1) * devConstants.istep_rlm[1] + blockIdx.x * devConstants.istep_rlm[0]); 
        idx[0] = 3*devConstants.nvector + idx[1]; 
        norm = scaleBySine(order, p_m_l_0, theta);
        for(int t=1; t<=devConstants.nvector; t++) {
          idx[1] += 3;
          reg2 = __dmul_rd(__dmul_rd(-1 * norm, (double) order), asin_theta_1d_rtm[id + i*blockDim.x]);         
          vr_reg[t*5 - 5] += sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[blockIdx.x] * reg2;
          vr_reg[t*5 - 4] += sp_rlm[idx[1] - 2] * a_r_1d_rlm_r[blockIdx.x] * reg2;
       //vr_reg[t*3 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[blockIdx.x], a_r_1d_rlm_r[blockIdx.x]) * scaleBySine(order, p_m_l_0, theta) * g_sph_rlm[j_rlm-1];    
          vr_reg[t*5 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[blockIdx.x], a_r_1d_rlm_r[blockIdx.x]) * norm * g_sph_rlm[j_rlm-1];    
          vr_reg[t*5 - 2] += sp_rlm[idx[1] - 2]  * a_r_1d_rlm_r[blockIdx.x] * dp_m_l;    
          vr_reg[t*5 - 1] -= sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[blockIdx.x] * dp_m_l;    
        }
      
        for(int t=1; t<=devConstants.nscalar; t++) {
          idx[0] += 1;
          vr_reg_scalar[t-1] += sp_rlm[idx[0] - 1] * norm;
        } 
      
      // m-1, l+1 
      reg1 = calculateLGP_m_l(abs(order)-1, deg+1, theta, p_mn_l_0, p_mn_l_1); 
      p_mn_l_0 = p_mn_l_1;
      p_mn_l_1 = reg1;

      // m, l+2
      reg2 = calculateLGP_m_l(order, deg+2, theta, p_m_l_0, p_m_l_1);
      p_m_l_0 = p_m_l_1;
      // p_m_l_0, m, l+1
      #ifdef CUDA_DEBUG
        if(deg<=devConstants.t_lvl) 
          j = (deg+1)*(deg+2) + order;
          P_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = scaleBySine(order, p_m_l_0, theta);
      #endif
      p_m_l_1 = reg2;
 
      //m, l+1
      dp_m_l = nextDp_m_l(order, deg+1, scaleBySine(abs(order)-1, p_mn_l_1, theta), scaleBySine(abs(order)+1, p_mp_l_0, theta));
     
      #ifdef CUDA_DEBUG
        if(deg<=devConstants.t_lvl)
          j = (deg+1)*(deg+2) + order;
          dP_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = dp_m_l; 
      #endif
      //m+1, l+3
      reg3 = calculateLGP_m_l(abs(order)+1, deg+3, theta, p_mp_l_0, p_mp_l_1);  
      p_mp_l_0 = p_mp_l_1;
      // p_mp_1_0, m+1, l+2
      p_mp_l_1 = reg3;
        
    }
    // mp_rlm 
    reg1 = (threadIdx.x + i*blockDim.x) * devConstants.istep_rtm[1] + blockIdx.x*devConstants.istep_rtm[0];
    idx_rtm[0] = devConstants.ncomp * (reg1 + (mp_rlm-1) * devConstants.istep_rtm[2]); 
    // mn_rlm
    idx_rtm[1] = devConstants.ncomp * (reg1 + (mn_rlm-1) * devConstants.istep_rtm[2]); 
    idx_rtm[2] = 3*devConstants.nvector + idx_rtm[0]; 
    for(int t=1; t<=devConstants.nvector; t++) {
      idx_rtm[0] += 3;
      idx_rtm[1] += 3;
      vr_rtm[idx_rtm[0] - 2 - 1]  += vr_reg[t*5 - 3]; 
      vr_rtm[idx_rtm[0] - 1 - 1]  += vr_reg[t*5 - 2]; 
      vr_rtm[idx_rtm[0] - 1]  += vr_reg[t*5 - 1]; 
      vr_rtm[idx_rtm[1] - 1 - 1] += vr_reg[t*5 - 5]; 
      vr_rtm[idx_rtm[1] - 1] += vr_reg[t*5 - 4]; 
    }
    for(int t=1; t<=devConstants.nscalar; t++) { 
      idx_rtm[2] += 1;
      vr_rtm[idx_rtm[2] - 1] = vr_reg_scalar[t-1];
    }
  }     
   
  free(vr_reg);
  free(vr_reg_scalar);
}

__global__
void transB_m_l_ver2D(int *lstack_rlm, int m0, int m1, int *idx_gl_1d_rlm_j, double *vr_rtm, double *sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *P_smdt, double *dP_smdt, double *g_sph_rlm, double *asin_theta_1d_rtm) {
   int id = threadIdx.x;
  // The work is parallelized over theta within a grid
  int nTheta = devConstants.nidx_rtm[1];
  unsigned int workLoad = nTheta/(blockDim.x);
  if( nTheta%blockDim.x > (threadIdx.x)) 
    workLoad++;

  int deg, j;

  // P(m,m)[cos theta]
  double p_mn_l_0, p_mn_l_1;
  double p_m_l_0, p_m_l_1;
  double p_mp_l_0, p_mp_l_1;
  double dp_m_l;
 
  double x=1, theta=0;
// 3 for m-1, m, m+1
  unsigned int idx[3] = {0,0,0}, idx_rtm[3] = {0,0,0};
  double reg1, reg2, reg3;
 
  double norm=0;

  double *vr_reg = (double*) malloc (sizeof(double)*5*devConstants.nvector);
  double *vr_reg_scalar = (double*) malloc (sizeof(double)*devConstants.nscalar);

  int mp_rlm = blockIdx.y+m0;
  double mn_rlm = devConstants.nidx_rtm[2] - mp_rlm + 1;
  int jst = lstack_rlm[mp_rlm-1] + 1;
  int jed = lstack_rlm[mp_rlm];
  int order = idx_gl_1d_rlm_j[devConstants.nidx_rlm[1]*2 + jst -1]; 
  int degree = idx_gl_1d_rlm_j[devConstants.nidx_rlm[1]*1 + jst - 1]; 

  for(int i=0; i<workLoad; i++) {
    theta = g_colat_rtm[threadIdx.x + i*blockDim.x];
    x = cos(theta);
    deg = degree;

    for(int rt=0; rt<devConstants.nvector*5;rt++)
      vr_reg[rt]=0;
    for(int rt=0; rt<devConstants.nscalar;rt++)
      vr_reg_scalar[rt]=0;
   
    // m-1,l-1
    p_mn_l_0 = calculateLGP_m_eq_l(abs(order)-1);
    //P_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = scaleBySine(order-1, p_mn_l_0, theta);
    // m-1,l
    p_mn_l_1 = calculateLGP_mp1_eq_l(abs(order)-1, x, p_mn_l_0);

    // m,l
    p_m_l_0 = calculateLGP_m_eq_l(order); 
    
    #ifdef CUDA_DEBUG
    j = degree*(degree+1) + order;
    P_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = scaleBySine(order, p_m_l_0, theta);
    #endif

    // m,l+1
    p_m_l_1 = calculateLGP_mp1_eq_l(order, x, p_m_l_0);

    // m+1,l+1 
    p_mp_l_0 = calculateLGP_m_eq_l(abs(order)+1);
    // m+1,l+2
    p_mp_l_1 = calculateLGP_mp1_eq_l(abs(order)+1, x, p_mp_l_0);

    // m,l
    dp_m_l = __dmul_rd(0.5, __dmul_rd(__dsqrt_rd(2*abs(order)), scaleBySine(abs(order)-1, p_mn_l_1, theta)));
    #ifdef CUDA_DEBUG
      j = degree*(degree+1) + order;
      dP_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = dp_m_l; 
    #endif

    for(int j_rlm=jst, deg=degree; j_rlm<=jed; j_rlm++, deg++) {
        idx[1] = devConstants.ncomp * ((j_rlm-1) * devConstants.istep_rlm[1] + blockIdx.x * devConstants.istep_rlm[0]); 
        idx[0] = 3*devConstants.nvector + idx[1]; 
        norm = scaleBySine(order, p_m_l_0, theta);
        for(int t=1; t<=devConstants.nvector; t++) {
          idx[1] += 3;
          reg2 = __dmul_rd(__dmul_rd(-1 * norm, (double) order), asin_theta_1d_rtm[id + i*blockDim.x]);         
          vr_reg[t*5 - 5] += sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[blockIdx.x] * reg2;
          vr_reg[t*5 - 4] += sp_rlm[idx[1] - 2] * a_r_1d_rlm_r[blockIdx.x] * reg2;
       //vr_reg[t*3 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[blockIdx.x], a_r_1d_rlm_r[blockIdx.x]) * scaleBySine(order, p_m_l_0, theta) * g_sph_rlm[j_rlm-1];    
          vr_reg[t*5 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[blockIdx.x], a_r_1d_rlm_r[blockIdx.x]) * norm * g_sph_rlm[j_rlm-1];    
          vr_reg[t*5 - 2] += sp_rlm[idx[1] - 2]  * a_r_1d_rlm_r[blockIdx.x] * dp_m_l;    
          vr_reg[t*5 - 1] -= sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[blockIdx.x] * dp_m_l;    
        }
      
        for(int t=1; t<=devConstants.nscalar; t++) {
          idx[0] += 1;
          vr_reg_scalar[t-1] += sp_rlm[idx[0] - 1] * norm;
        } 
      
      // m-1, l+1 
      reg1 = calculateLGP_m_l(abs(order)-1, deg+1, theta, p_mn_l_0, p_mn_l_1); 
      p_mn_l_0 = p_mn_l_1;
      p_mn_l_1 = reg1;

      // m, l+2
      reg2 = calculateLGP_m_l(order, deg+2, theta, p_m_l_0, p_m_l_1);
      p_m_l_0 = p_m_l_1;
      // p_m_l_0, m, l+1
      #ifdef CUDA_DEBUG
        if(deg<=devConstants.t_lvl) 
          j = (deg+1)*(deg+2) + order;
          P_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = scaleBySine(order, p_m_l_0, theta);
      #endif
      p_m_l_1 = reg2;
 
      //m, l+1
      dp_m_l = nextDp_m_l(order, deg+1, scaleBySine(abs(order)-1, p_mn_l_1, theta), scaleBySine(abs(order)+1, p_mp_l_0, theta));
     
      #ifdef CUDA_DEBUG
        if(deg<=devConstants.t_lvl)
          j = (deg+1)*(deg+2) + order;
          dP_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = dp_m_l; 
      #endif
      //m+1, l+3
      reg3 = calculateLGP_m_l(abs(order)+1, deg+3, theta, p_mp_l_0, p_mp_l_1);  
      p_mp_l_0 = p_mp_l_1;
      // p_mp_1_0, m+1, l+2
      p_mp_l_1 = reg3;
        
    }
    // mp_rlm 
    reg1 = (threadIdx.x + i*blockDim.x) * devConstants.istep_rtm[1] + blockIdx.x*devConstants.istep_rtm[0];
    idx_rtm[0] = devConstants.ncomp * (reg1 + (mp_rlm-1) * devConstants.istep_rtm[2]); 
    // mn_rlm
    idx_rtm[1] = devConstants.ncomp * (reg1 + (mn_rlm-1) * devConstants.istep_rtm[2]); 
    idx_rtm[2] = 3*devConstants.nvector + idx_rtm[0]; 
    for(int t=1; t<=devConstants.nvector; t++) {
      idx_rtm[0] += 3;
      idx_rtm[1] += 3;
      vr_rtm[idx_rtm[0] - 2 - 1]  += vr_reg[t*5 - 3]; 
      vr_rtm[idx_rtm[0] - 1 - 1]  += vr_reg[t*5 - 2]; 
      vr_rtm[idx_rtm[0] - 1]  += vr_reg[t*5 - 1]; 
      vr_rtm[idx_rtm[1] - 1 - 1] += vr_reg[t*5 - 5]; 
      vr_rtm[idx_rtm[1] - 1] += vr_reg[t*5 - 4]; 
    }
    for(int t=1; t<=devConstants.nscalar; t++) { 
      idx_rtm[2] += 1;
      vr_rtm[idx_rtm[2] - 1] = vr_reg_scalar[t-1];
    }
  }     
   
  free(vr_reg);
  free(vr_reg_scalar);
}

__global__
void transB_m_l_ver3D(int *lstack_rlm, int m0, int m1, int *idx_gl_1d_rlm_j, double *vr_rtm, double *sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *P_smdt, double *dP_smdt, double *g_sph_rlm, double *asin_theta_1d_rtm) {

  int deg, j;

  // P(m,m)[cos theta]
  double p_mn_l_0, p_mn_l_1;
  double p_m_l_0, p_m_l_1;
  double p_mp_l_0, p_mp_l_1;
  double dp_m_l;
 
  double x=1, theta=0;
// 3 for m-1, m, m+1
  unsigned int idx[3] = {0,0,0}, idx_rtm[3] = {0,0,0};
  double reg1, reg2, reg3;
 
  double norm=0;
  
  double *vr_reg = (double*) malloc (sizeof(double)*5*devConstants.nvector);
  double *vr_reg_scalar = (double*) malloc (sizeof(double)*devConstants.nscalar);

  int mp_rlm = blockIdx.y+m0;
  double mn_rlm = devConstants.nidx_rtm[2] - mp_rlm + 1;
  int jst = lstack_rlm[mp_rlm-1] + 1;
  int jed = lstack_rlm[mp_rlm];
  int order = abs(idx_gl_1d_rlm_j[devConstants.nidx_rlm[1]*2 + jst -1]); 
  int degree = idx_gl_1d_rlm_j[devConstants.nidx_rlm[1]*1 + jst - 1]; 

  theta = g_colat_rtm[blockIdx.z];
  x = cos(theta);
  deg = degree;

  for(int rt=0; rt<devConstants.nvector*5;rt++)
    vr_reg[rt]=0;
  for(int rt=0; rt<devConstants.nscalar;rt++)
    vr_reg_scalar[rt]=0;
  // m-1,l-1
  p_mn_l_0 = calculateLGP_m_eq_l(order-1);
  // m-1,l
  p_mn_l_1 = calculateLGP_mp1_eq_l(order-1, x, p_mn_l_0);

  // m,l
  p_m_l_0 = calculateLGP_m_eq_l(order); 
  // m,l+1
  p_m_l_1 = calculateLGP_mp1_eq_l(order, x, p_m_l_0);

  // m+1,l+1 
  p_mp_l_0 = calculateLGP_m_eq_l(order+1);
  // m+1,l+2
  p_mp_l_1 = calculateLGP_mp1_eq_l(order+1, x, p_mp_l_0);

  // m,l
  dp_m_l = __dmul_rd(0.5, __dmul_rd(__dsqrt_rd(2*order), scaleBySine(order-1, p_mn_l_1, theta)));

    for(int j_rlm=jst, deg=degree; j_rlm<=jed; j_rlm++, deg++) {
        idx[1] = devConstants.ncomp * ((j_rlm-1) * devConstants.istep_rlm[1] + blockIdx.x * devConstants.istep_rlm[0]); 
        idx[0] = 3*devConstants.nvector + idx[1]; 
        norm = scaleBySine(order, p_m_l_0, theta);
        for(int t=1; t<=devConstants.nvector; t++) {
          idx[1] += 3;
          reg2 = __dmul_rd(__dmul_rd(-1 * norm, (double) order), asin_theta_1d_rtm[blockIdx.z]);         
          vr_reg[t*5 - 5] += sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[blockIdx.x] * reg2;
          vr_reg[t*5 - 4] += sp_rlm[idx[1] - 2] * a_r_1d_rlm_r[blockIdx.x] * reg2;
       //vr_reg[t*3 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[blockIdx.x], a_r_1d_rlm_r[blockIdx.x]) * scaleBySine(order, p_m_l_0, theta) * g_sph_rlm[j_rlm-1];    
          vr_reg[t*5 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[blockIdx.x], a_r_1d_rlm_r[blockIdx.x]) * norm * g_sph_rlm[j_rlm-1];    
          vr_reg[t*5 - 2] += sp_rlm[idx[1] - 2]  * a_r_1d_rlm_r[blockIdx.x] * dp_m_l;    
          vr_reg[t*5 - 1] -= sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[blockIdx.x] * dp_m_l;    
        }
      
        for(int t=1; t<=devConstants.nscalar; t++) {
          idx[0] += 1;
          vr_reg_scalar[t-1] += sp_rlm[idx[0] - 1] * norm;
        } 
      
      // m-1, l+1 
      reg1 = calculateLGP_m_l(abs(order)-1, deg+1, theta, p_mn_l_0, p_mn_l_1); 
      p_mn_l_0 = p_mn_l_1;
      p_mn_l_1 = reg1;

      // m, l+2
      reg2 = calculateLGP_m_l(order, deg+2, theta, p_m_l_0, p_m_l_1);
      p_m_l_0 = p_m_l_1;
      // p_m_l_0, m, l+1
      #ifdef CUDA_DEBUG
        if(deg<=devConstants.t_lvl) 
          j = (deg+1)*(deg+2) + order;
          P_smdt[blockIdx.x*devConstants.nidx_rlm[1] + j] = scaleBySine(order, p_m_l_0, theta);
      #endif
      p_m_l_1 = reg2;
 
      //m, l+1
      dp_m_l = nextDp_m_l(order, deg+1, scaleBySine(abs(order)-1, p_mn_l_1, theta), scaleBySine(abs(order)+1, p_mp_l_0, theta));
     
      #ifdef CUDA_DEBUG
        if(deg<=devConstants.t_lvl)
          j = (deg+1)*(deg+2) + order;
          dP_smdt[blockIdx.x*devConstants.nidx_rlm[1] + j] = dp_m_l; 
      #endif
      //m+1, l+3
      reg3 = calculateLGP_m_l(abs(order)+1, deg+3, theta, p_mp_l_0, p_mp_l_1);  
      p_mp_l_0 = p_mp_l_1;
      // p_mp_1_0, m+1, l+2
      p_mp_l_1 = reg3;
        
    }
    // mp_rlm 
    reg1 = (blockIdx.z) * devConstants.istep_rtm[1] + blockIdx.x*devConstants.istep_rtm[0];
    idx_rtm[0] = devConstants.ncomp * (reg1 + (mp_rlm-1) * devConstants.istep_rtm[2]); 
    // mn_rlm
    idx_rtm[1] = devConstants.ncomp * (reg1 + (mn_rlm-1) * devConstants.istep_rtm[2]); 
    idx_rtm[2] = 3*devConstants.nvector + idx_rtm[0]; 
    for(int t=1; t<=devConstants.nvector; t++) {
      idx_rtm[0] += 3;
      idx_rtm[1] += 3;
      vr_rtm[idx_rtm[0] - 2 - 1]  += vr_reg[t*5 - 3]; 
      vr_rtm[idx_rtm[0] - 1 - 1]  += vr_reg[t*5 - 2]; 
      vr_rtm[idx_rtm[0] - 1]  += vr_reg[t*5 - 1]; 
      vr_rtm[idx_rtm[1] - 1 - 1] += vr_reg[t*5 - 5]; 
      vr_rtm[idx_rtm[1] - 1] += vr_reg[t*5 - 4]; 
    }
    for(int t=1; t<=devConstants.nscalar; t++) { 
      idx_rtm[2] += 1;
      vr_rtm[idx_rtm[2] - 1] = vr_reg_scalar[t-1];
    }
   
  free(vr_reg);
  free(vr_reg_scalar);
}

__global__
void transB_m_l_ver4D(int *lstack_rlm, int m0, int m1, int *idx_gl_1d_rlm_j, double *vr_rtm, double const* __restrict__ sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *P_smdt, double *dP_smdt, double *g_sph_rlm, double *asin_theta_1d_rtm) {
  int deg, j;

  // P(m,m)[cos theta]
  double p_mn_l_0, p_mn_l_1;
  double p_m_l_0, p_m_l_1;
  double p_mp_l_0, p_mp_l_1;
  double dp_m_l;
 
  double x=1, theta=0;
// 3 for m-1, m, m+1
  unsigned int idx[3] = {0,0,0}, idx_rtm[3] = {0,0,0};
  double reg1, reg2, reg3;
 
  double norm=0;

  int mp_rlm = blockIdx.y+m0;
  int mn_rlm = devConstants.nidx_rtm[2] - mp_rlm + 1;
  int jst = lstack_rlm[mp_rlm-1] + 1;
  int jed = lstack_rlm[mp_rlm];
  int order = idx_gl_1d_rlm_j[devConstants.nidx_rlm[1]*2 + jst -1]; 
  int degree = idx_gl_1d_rlm_j[devConstants.nidx_rlm[1]*1 + jst - 1]; 

  double *vr_reg = (double*) malloc (sizeof(double)*5*devConstants.nvector);
  double *vr_reg_scalar = (double*) malloc (sizeof(double)*devConstants.nscalar);

   double vr1, vr2, vr3, vr4, vr5;
   double vr_s;
    theta = g_colat_rtm[blockIdx.x];
    x = cos(theta);
  for(int t=1; t<=devConstants.nvector; t++) {
    vr1 = vr2 = vr3 = vr4 = vr5 = vr_s = 0;
    deg = degree;
   /* 
    j = devConstants.nvector*5;
    for(int rt=0; rt<j;rt++)
      vr_reg[rt]=0;
    for(int rt=0; rt<devConstants.nscalar;rt++)
      vr_reg_scalar[rt]=0;
  */ 
    // m-1,l-1
    p_mn_l_0 = calculateLGP_m_eq_l(abs(order)-1);
    //P_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = scaleBySine(order-1, p_mn_l_0, theta);
    // m-1,l
    p_mn_l_1 = calculateLGP_mp1_eq_l(abs(order)-1, x, p_mn_l_0);

    // m,l
    p_m_l_0 = calculateLGP_m_eq_l(order); 
    
    #ifdef CUDA_DEBUG
    j = degree*(degree+1) + order;
    P_smdt[blockIdx.x*devConstants.nidx_rlm[1] + j] = scaleBySine(order, p_m_l_0, theta);
    #endif

    // m,l+1
    p_m_l_1 = calculateLGP_mp1_eq_l(order, x, p_m_l_0);

    // m+1,l+1 
    p_mp_l_0 = calculateLGP_m_eq_l(abs(order)+1);
    // m+1,l+2
    p_mp_l_1 = calculateLGP_mp1_eq_l(abs(order)+1, x, p_mp_l_0);

    // m,l
    dp_m_l = __dmul_rd(0.5, __dmul_rd(__dsqrt_rd(2*abs(order)), scaleBySine(abs(order)-1, p_mn_l_1, theta)));
    #ifdef CUDA_DEBUG
      j = degree*(degree+1) + order;
      dP_smdt[blockIdx.x*devConstants.nidx_rlm[1] + j] = dp_m_l; 
    #endif

    for(int j_rlm=jst, deg=degree; j_rlm<=jed; j_rlm++, deg++) {
        idx[1] = devConstants.ncomp * ((j_rlm-1) * devConstants.istep_rlm[1] + threadIdx.x * devConstants.istep_rlm[0]); 
        idx[0] = 3*devConstants.nvector + idx[1]; 
        norm = scaleBySine(order, p_m_l_0, theta);
   //     for(int t=1; t<=devConstants.nvector; t++) {
          idx[1] += 3*t;
          reg2 = __dmul_rd(__dmul_rd(-1 * norm, (double) order), asin_theta_1d_rtm[blockIdx.x]);         
          vr5 += sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[threadIdx.x] * reg2;
          vr4 += sp_rlm[idx[1] - 2] * a_r_1d_rlm_r[threadIdx.x] * reg2;
       //vr_reg[t*3 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[blockIdx.x], a_r_1d_rlm_r[blockIdx.x]) * scaleBySine(order, p_m_l_0, theta) * g_sph_rlm[j_rlm-1];    
          vr3 += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[threadIdx.x], a_r_1d_rlm_r[threadIdx.x]) * norm * g_sph_rlm[j_rlm-1];    
          vr2 += sp_rlm[idx[1] - 2]  * a_r_1d_rlm_r[threadIdx.x] * dp_m_l;    
          vr1 -= sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[threadIdx.x] * dp_m_l;    
         /* vr_reg[t*5 - 5] += sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[threadIdx.x] * reg2;
          vr_reg[t*5 - 4] += sp_rlm[idx[1] - 2] * a_r_1d_rlm_r[threadIdx.x] * reg2;
       //vr_reg[t*3 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[blockIdx.x], a_r_1d_rlm_r[blockIdx.x]) * scaleBySine(order, p_m_l_0, theta) * g_sph_rlm[j_rlm-1];    
          vr_reg[t*5 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[threadIdx.x], a_r_1d_rlm_r[threadIdx.x]) * norm * g_sph_rlm[j_rlm-1];    
          vr_reg[t*5 - 2] += sp_rlm[idx[1] - 2]  * a_r_1d_rlm_r[threadIdx.x] * dp_m_l;    
          vr_reg[t*5 - 1] -= sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[threadIdx.x] * dp_m_l;    
        }*/
      
        /*for(int t=1; t<=devConstants.nscalar; t++) {
          idx[0] += 1;
          vr_reg_scalar[t-1] += sp_rlm[idx[0] - 1] * norm;
        }*/
        idx[0] += t; 
        vr_s += sp_rlm[idx[0] - 1] * norm;
      
      // m-1, l+1 
      reg1 = calculateLGP_m_l(abs(order)-1, deg+1, theta, p_mn_l_0, p_mn_l_1); 
      p_mn_l_0 = p_mn_l_1;
      p_mn_l_1 = reg1;

      // m, l+2
      reg2 = calculateLGP_m_l(order, deg+2, theta, p_m_l_0, p_m_l_1);
      p_m_l_0 = p_m_l_1;
      // p_m_l_0, m, l+1
      #ifdef CUDA_DEBUG
        if(deg<=devConstants.t_lvl) 
          j = (deg+1)*(deg+2) + order;
          P_smdt[blockIdx.x*devConstants.nidx_rlm[1] + j] = scaleBySine(order, p_m_l_0, theta);
      #endif
      p_m_l_1 = reg2;
 
      //m, l+1
      dp_m_l = nextDp_m_l(order, deg+1, scaleBySine(abs(order)-1, p_mn_l_1, theta), scaleBySine(abs(order)+1, p_mp_l_0, theta));
     
      #ifdef CUDA_DEBUG
        if(deg<=devConstants.t_lvl)
          j = (deg+1)*(deg+2) + order;
          dP_smdt[blockIdx.x*devConstants.nidx_rlm[1] + j] = dp_m_l; 
      #endif
      //m+1, l+3
      reg3 = calculateLGP_m_l(abs(order)+1, deg+3, theta, p_mp_l_0, p_mp_l_1);  
      p_mp_l_0 = p_mp_l_1;
      // p_mp_1_0, m+1, l+2
      p_mp_l_1 = reg3;
        
    }
    // mp_rlm 
    reg1 = blockIdx.x * devConstants.istep_rtm[1] + threadIdx.x*devConstants.istep_rtm[0];
    idx_rtm[0] = devConstants.ncomp * (reg1 + (mp_rlm-1) * devConstants.istep_rtm[2]); 
    // mn_rlm
    idx_rtm[1] = devConstants.ncomp * (reg1 + (mn_rlm-1) * devConstants.istep_rtm[2]); 
    idx_rtm[2] = 3*devConstants.nvector + idx_rtm[0]; 
    
      idx_rtm[0] += 3*t;
      idx_rtm[1] += 3*t;
      vr_rtm[idx_rtm[0] - 2 - 1]  += vr3; 
      vr_rtm[idx_rtm[0] - 1 - 1]  += vr2; 
      vr_rtm[idx_rtm[0] - 1]  += vr1; 
      vr_rtm[idx_rtm[1] - 1 - 1] += vr5; 
      vr_rtm[idx_rtm[1] - 1] += vr4; 
      idx_rtm[2] += t;
      vr_rtm[idx_rtm[2] - 1] = vr_s;
    /*for(int t=1; t<=devConstants.nvector; t++) {
      idx_rtm[0] += 3;
      idx_rtm[1] += 3;
      vr_rtm[idx_rtm[0] - 2 - 1]  += vr_reg[t*5 - 3]; 
      vr_rtm[idx_rtm[0] - 1 - 1]  += vr_reg[t*5 - 2]; 
      vr_rtm[idx_rtm[0] - 1]  += vr_reg[t*5 - 1]; 
      vr_rtm[idx_rtm[1] - 1 - 1] += vr_reg[t*5 - 5]; 
      vr_rtm[idx_rtm[1] - 1] += vr_reg[t*5 - 4]; 
    }
    for(int t=1; t<=devConstants.nscalar; t++) { 
      idx_rtm[2] += 1;
      vr_rtm[idx_rtm[2] - 1] = vr_reg_scalar[t-1];
    }*/
   
//   free(vr_reg);
 //  free(vr_reg_scalar);
  } 
}

__global__
void transB_m_l_ver5D(int *lstack_rlm, int *idx_gl_1d_rlm_j, double *vr_rtm, double *sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *P_smdt, double *dP_smdt, double *g_sph_rlm, double *asin_theta_1d_rtm) {
  extern __shared__ double vr[]; 

  int mp_rlm = blockIdx.y+1;
  double mn_rlm = devConstants.nidx_rtm[2] - mp_rlm + 1;
  int jst = lstack_rlm[mp_rlm-1] + 1;
  int jed = lstack_rlm[mp_rlm];
  int order = idx_gl_1d_rlm_j[devConstants.nidx_rlm[1]*2 + jst -1]; 
  int degree = idx_gl_1d_rlm_j[devConstants.nidx_rlm[1]*1 + jst - 1]; 

 if(degree != 1 && degree != 0) {
  int deg, j, ord = abs(order);

  // P(m,m)[cos theta]
  double p_mn_l_0, p_mn_l_1;
  double p_m_l_0, p_m_l_1;
  double p_mp_l_0, p_mp_l_1;
  double dp_m_l;
 
  double x=1, theta=0;
// 3 for m-1, m, m+1
  unsigned int idx[3] = {0,0,0}, idx_rtm[3] = {0,0,0};
  double reg1, reg2, reg3;
 
  double norm=0;
  
//  double *vr_reg = (double*) malloc (sizeof(double)*5*devConstants.nvector);
//  double *vr_reg_scalar = (double*) malloc (sizeof(double)*devConstants.nscalar);
  theta = g_colat_rtm[blockIdx.x];
  x = cos(theta);
  deg = degree;

/*  for(int rt=0; rt<devConstants.nvector*5;rt++)
    vr_reg[rt]=0;
  for(int rt=0; rt<devConstants.nscalar;rt++)
    vr_reg_scalar[rt]=0;
*/
  // m-1,l-1
  p_mn_l_0 = calculateLGP_m_eq_l(ord-1);
  // m-1,l
  p_mn_l_1 = calculateLGP_mp1_eq_l(ord-1, x, p_mn_l_0);

  // m,l
  p_m_l_0 = calculateLGP_m_eq_l(ord); 
  // m,l+1
  p_m_l_1 = calculateLGP_mp1_eq_l(ord, x, p_m_l_0);

  // m+1,l+1 
  p_mp_l_0 = calculateLGP_m_eq_l(ord+1);
  // m+1,l+2
  p_mp_l_1 = calculateLGP_mp1_eq_l(ord+1, x, p_mp_l_0);

  // m,l
  dp_m_l = __dmul_rd(0.5, __dmul_rd(__dsqrt_rd(2*ord), scaleBySine(ord-1, p_mn_l_1, theta)));
  
  int slice_smem = threadIdx.x * (5*devConstants.nvector + devConstants.nscalar);

    for(int j_rlm=jst, deg=degree; j_rlm<=jed; j_rlm++, deg++) {
        idx[1] = devConstants.ncomp * ((j_rlm-1) * devConstants.istep_rlm[1] + threadIdx.x * devConstants.istep_rlm[0]); 
        idx[0] = 3*devConstants.nvector + idx[1]; 
        norm = scaleBySine(ord, p_m_l_0, theta);
        for(int t=1; t<=devConstants.nvector; t++) {
          idx[1] += 3;
          reg2 = __dmul_rd(__dmul_rd(-1 * norm, (double) order), asin_theta_1d_rtm[blockIdx.x]);         
          reg3 = a_r_1d_rlm_r[threadIdx.x];
          vr[slice_smem + t*5 - 1] += sp_rlm[idx[1] - 1] * reg3 * reg2;
          vr[slice_smem + t*5 -2 ]  += sp_rlm[idx[1] - 2] * reg3 * reg2;
       //vr_reg[t*3 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[blockIdx.x], a_r_1d_rlm_r[blockIdx.x]) * scaleBySine(order, p_m_l_0, theta) * g_sph_rlm[j_rlm-1];    
          vr[slice_smem + t*5 -3 ] += sp_rlm[idx[1] - 3] * __dmul_rd(reg3, reg3) * norm * g_sph_rlm[j_rlm-1];    
          vr[slice_smem + t*5 -4 ] += sp_rlm[idx[1] - 2]  * reg3 * dp_m_l;    
          vr[slice_smem + t*5 -5 ] -= sp_rlm[idx[1] - 1] * reg3 * dp_m_l;    
        /*  vr_reg[t*5 - 5] += sp_rlm[idx[1] - 1] * reg3 * reg2;
          vr_reg[t*5 - 4] += sp_rlm[idx[1] - 2] * reg3 * reg2;
       //vr_reg[t*3 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[blockIdx.x], a_r_1d_rlm_r[blockIdx.x]) * scaleBySine(order, p_m_l_0, theta) * g_sph_rlm[j_rlm-1];    
          vr_reg[t*5 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(reg3, reg3) * norm * g_sph_rlm[j_rlm-1];    
          vr_reg[t*5 - 2] += sp_rlm[idx[1] - 2]  * reg3 * dp_m_l;    
          vr_reg[t*5 - 1] -= sp_rlm[idx[1] - 1] * reg3 * dp_m_l;    
        */
        }
        for(int t=0; t < devConstants.nscalar; t++) {
          idx[0] += 1;
          vr[slice_smem + 5*devConstants.nvector + t] += sp_rlm[idx[0] - 1] * norm; 
          //vr_reg_scalar[t-1] += sp_rlm[idx[0] - 1] * norm;
        } 
      // m-1, l+1 
      reg1 = calculateLGP_m_l(ord-1, deg+1, theta, p_mn_l_0, p_mn_l_1); 
      p_mn_l_0 = p_mn_l_1;
      p_mn_l_1 = reg1;

      // m, l+2
      reg2 = calculateLGP_m_l(ord, deg+2, theta, p_m_l_0, p_m_l_1);
      p_m_l_0 = p_m_l_1;
      // p_m_l_0, m, l+1
      #ifdef CUDA_DEBUG
        if(deg<=devConstants.t_lvl) 
          j = (deg+1)*(deg+2) + order;
          P_smdt[blockIdx.x*devConstants.nidx_rlm[1] + j] = scaleBySine(ord, p_m_l_0, theta);
      #endif
      p_m_l_1 = reg2;
 
      //m, l+1
      dp_m_l = nextDp_m_l(ord, deg+1, scaleBySine(ord-1, p_mn_l_1, theta), scaleBySine(ord+1, p_mp_l_0, theta));
     
      #ifdef CUDA_DEBUG
        if(deg<=devConstants.t_lvl)
          j = (deg+1)*(deg+2) + order;
          dP_smdt[blockIdx.x*devConstants.nidx_rlm[1] + j] = dp_m_l; 
      #endif
      //m+1, l+3
      reg3 = calculateLGP_m_l(ord+1, deg+3, theta, p_mp_l_0, p_mp_l_1);  
      p_mp_l_0 = p_mp_l_1;
      // p_mp_1_0, m+1, l+2
      p_mp_l_1 = reg3;
        
    }
    // mp_rlm 
    reg1 = (blockIdx.x) * devConstants.istep_rtm[1] + threadIdx.x*devConstants.istep_rtm[0];
    idx_rtm[0] = devConstants.ncomp * (reg1 + (mp_rlm-1) * devConstants.istep_rtm[2]); 
    // mn_rlm
    idx_rtm[1] = devConstants.ncomp * (reg1 + (mn_rlm-1) * devConstants.istep_rtm[2]); 
    idx_rtm[2] = 3*devConstants.nvector + idx_rtm[0]; 
    for(int t=1; t<=devConstants.nvector; t++) {
      idx_rtm[0] += 3;
      idx_rtm[1] += 3;
      vr_rtm[idx_rtm[0] - 2 - 1]  += vr[slice_smem + t*5 - 3]; 
      vr_rtm[idx_rtm[0] - 1 - 1]  += vr[slice_smem + t*5 - 4]; 
      vr_rtm[idx_rtm[0] - 1]  += vr[slice_smem + t*5 - 5]; 
      vr_rtm[idx_rtm[1] - 1 - 1] += vr[slice_smem + t*5 - 1]; 
      vr_rtm[idx_rtm[1] - 1] += vr[slice_smem + t*5 - 2]; 
    }
    for(int t=0; t<devConstants.nscalar; t++) { 
      idx_rtm[2] += 1;
      vr_rtm[idx_rtm[2] - 1] = vr[slice_smem + 5*devConstants.nvector + t];
    }
   
  //free(vr_reg);
 // free(vr_reg_scalar);
  }
}


__global__
void transB_m_l_ver6D(int *lstack_rlm, int m0, int m1, int *idx_gl_1d_rlm_j, double *vr_rtm, double *sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *P_jl, double *dP_jl, double *g_sph_rlm, double *asin_theta_1d_rtm) {
  extern __shared__ double cacheSchmidt[];

  int deg, j;
 
  double x=1, theta=0;
// 3 for m-1, m, m+1
  unsigned int idx[3] = {0,0,0}, idx_rtm[3] = {0,0,0};
  double reg1, reg2, reg3;
 
  double norm=0;
  
  double vr1, vr2, vr3, vr4, vr5, vrs1;

  int mp_rlm = blockIdx.y+m0;
  double mn_rlm = devConstants.nidx_rtm[2] - mp_rlm + 1;
  int jst = lstack_rlm[mp_rlm-1] + 1;
  int jed = lstack_rlm[mp_rlm];
  int order = idx_gl_1d_rlm_j[devConstants.nidx_rlm[1]*2 + jst -1]; 
  int degree = idx_gl_1d_rlm_j[devConstants.nidx_rlm[1]*1 + jst - 1]; 

  int ord = abs(order);
  theta = g_colat_rtm[blockIdx.x];
  x = cos(theta);
  deg = degree;

  int num_modes = jed-jst+1;
  int itr = (int) num_modes/blockDim.x;
  j = devConstants.t_lvl + 1;
  for(int i=0; i <= itr; i++) {
    if(i != itr || threadIdx.x < num_modes)
      cacheSchmidt[threadIdx.x + blockDim.x*i] = P_jl[devConstants.nidx_rlm[1]*blockIdx.x +(jst-1+threadIdx.x) + i*blockDim.x]; 
      cacheSchmidt[j + threadIdx.x + blockDim.x*i] = dP_jl[devConstants.nidx_rlm[1]*blockIdx.x +(jst-1+threadIdx.x) + i*blockDim.x]; 
  }

  __syncthreads();
/*
  for(int t=1; t<=devConstants.nvector; t++) {
    vr1=vr2=vr3=vr4=vr5=0;
    for(int j_rlm=jst, deg=degree; j_rlm<=jed; j_rlm++, deg++) {
      idx[1] = devConstants.ncomp * ((j_rlm-1) * devConstants.istep_rlm[1] + threadIdx.x * devConstants.istep_rlm[0]); 
      idx[1] += 3;
          reg2 = __dmul_rd(__dmul_rd(-1 * norm, (double) order), asin_theta_1d_rtm[blockIdx.z]);         
          vr_reg[t*5 - 5] += sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[blockIdx.x] * reg2;
          vr_reg[t*5 - 4] += sp_rlm[idx[1] - 2] * a_r_1d_rlm_r[blockIdx.x] * reg2;
       //vr_reg[t*3 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[blockIdx.x], a_r_1d_rlm_r[blockIdx.x]) * scaleBySine(order, p_m_l_0, theta) * g_sph_rlm[j_rlm-1];    
          vr_reg[t*5 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[blockIdx.x], a_r_1d_rlm_r[blockIdx.x]) * norm * g_sph_rlm[j_rlm-1];    
          vr_reg[t*5 - 2] += sp_rlm[idx[1] - 2]  * a_r_1d_rlm_r[blockIdx.x] * dp_m_l;    
          vr_reg[t*5 - 1] -= sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[blockIdx.x] * dp_m_l;    
        }
      
        for(int t=1; t<=devConstants.nscalar; t++) {
      idx[0] = 3*devConstants.nvector + idx[1]; 
          idx[0] += 1;
          vr_reg_scalar[t-1] += sp_rlm[idx[0] - 1] * norm;
        } 
      
      // m-1, l+1 
      reg1 = calculateLGP_m_l(abs(order)-1, deg+1, theta, p_mn_l_0, p_mn_l_1); 
      p_mn_l_0 = p_mn_l_1;
      p_mn_l_1 = reg1;

      // m, l+2
      reg2 = calculateLGP_m_l(order, deg+2, theta, p_m_l_0, p_m_l_1);
      p_m_l_0 = p_m_l_1;
      // p_m_l_0, m, l+1
      #ifdef CUDA_DEBUG
        if(deg<=devConstants.t_lvl) 
          j = (deg+1)*(deg+2) + order;
          P_smdt[blockIdx.x*devConstants.nidx_rlm[1] + j] = scaleBySine(order, p_m_l_0, theta);
      #endif
      p_m_l_1 = reg2;
 
      //m, l+1
      dp_m_l = nextDp_m_l(order, deg+1, scaleBySine(abs(order)-1, p_mn_l_1, theta), scaleBySine(abs(order)+1, p_mp_l_0, theta));
     
      #ifdef CUDA_DEBUG
        if(deg<=devConstants.t_lvl)
          j = (deg+1)*(deg+2) + order;
          dP_smdt[blockIdx.x*devConstants.nidx_rlm[1] + j] = dp_m_l; 
      #endif
      //m+1, l+3
      reg3 = calculateLGP_m_l(abs(order)+1, deg+3, theta, p_mp_l_0, p_mp_l_1);  
      p_mp_l_0 = p_mp_l_1;
      // p_mp_1_0, m+1, l+2
      p_mp_l_1 = reg3;
        
    }
    // mp_rlm 
    reg1 = (blockIdx.z) * devConstants.istep_rtm[1] + blockIdx.x*devConstants.istep_rtm[0];
    idx_rtm[0] = devConstants.ncomp * (reg1 + (mp_rlm-1) * devConstants.istep_rtm[2]); 
    // mn_rlm
    idx_rtm[1] = devConstants.ncomp * (reg1 + (mn_rlm-1) * devConstants.istep_rtm[2]); 
    idx_rtm[2] = 3*devConstants.nvector + idx_rtm[0]; 
    for(int t=1; t<=devConstants.nvector; t++) {
      idx_rtm[0] += 3;
      idx_rtm[1] += 3;
      vr_rtm[idx_rtm[0] - 2 - 1]  += vr_reg[t*5 - 3]; 
      vr_rtm[idx_rtm[0] - 1 - 1]  += vr_reg[t*5 - 2]; 
      vr_rtm[idx_rtm[0] - 1]  += vr_reg[t*5 - 1]; 
      vr_rtm[idx_rtm[1] - 1 - 1] += vr_reg[t*5 - 5]; 
      vr_rtm[idx_rtm[1] - 1] += vr_reg[t*5 - 4]; 
    }
    for(int t=1; t<=devConstants.nscalar; t++) { 
      idx_rtm[2] += 1;
      vr_rtm[idx_rtm[2] - 1] = vr_reg_scalar[t-1];
    }
   
  free(vr_reg);
  free(vr_reg_scalar);
*/
}

__global__
void transB_m_l_ver7D(int *lstack_rlm, int m0, int m1, int *idx_gl_1d_rlm_j, double *vr_rtm, double *sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *P_jl, double *dP_jl, double *g_sph_rlm, double *asin_theta_1d_rtm) {
  int deg, j;
 
  double x=1, theta=0;
// 3 for m-1, m, m+1
  unsigned int idx[3] = {0,0,0}, idx_rtm[3] = {0,0,0};
  double reg1, reg2;
 
  double vr1, vr2, vr3, vr4, vr5, vrs1;

  int mp_rlm = blockIdx.y+m0;
  double mn_rlm = devConstants.nidx_rtm[2] - mp_rlm + 1;
  int jst = lstack_rlm[mp_rlm-1] + 1;
  int jed = lstack_rlm[mp_rlm];
  int order = idx_gl_1d_rlm_j[devConstants.nidx_rlm[1]*2 + jst -1]; 
  int degree = idx_gl_1d_rlm_j[devConstants.nidx_rlm[1]*1 + jst - 1]; 

  int ord = abs(order);
  theta = g_colat_rtm[blockIdx.x];
  x = cos(theta);
  deg = degree;

  int idx_p_jl = devConstants.nidx_rlm[1]*blockIdx.x+jst-1;
  for(int t=1; t<=devConstants.nvector; t++) {
    vr1=vr2=vr3=vr4=vr5=0;
    for(int j_rlm=jst, deg=degree; j_rlm<=jed; j_rlm++, deg++) {
      idx[1] = 3*t + devConstants.ncomp * ((j_rlm-1) * devConstants.istep_rlm[1] + threadIdx.x * devConstants.istep_rlm[0]); 
      idx_p_jl++;
      reg2 = __dmul_rd(__dmul_rd(-1 * P_jl[idx_p_jl], (double) order), asin_theta_1d_rtm[blockIdx.x]);         
      vr5 += sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[threadIdx.x] * reg2;
      vr4 += sp_rlm[idx[1] - 2] * a_r_1d_rlm_r[threadIdx.x] * reg2;
       //vr_reg[t*3 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[blockIdx.x], a_r_1d_rlm_r[blockIdx.x]) * scaleBySine(order, p_m_l_0, theta) * g_sph_rlm[j_rlm-1];    
      vr3 += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[threadIdx.x], a_r_1d_rlm_r[threadIdx.x]) * P_jl[idx_p_jl] * g_sph_rlm[j_rlm-1];    
      vr2 += sp_rlm[idx[1] - 2]  * a_r_1d_rlm_r[threadIdx.x] * dP_jl[idx_p_jl];    
      vr1 -= sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[threadIdx.x] * dP_jl[idx_p_jl];    
    }
    reg1 = (blockIdx.x) * devConstants.istep_rtm[1] + threadIdx.x*devConstants.istep_rtm[0];
    idx_rtm[0] = 3*t + devConstants.ncomp * (reg1 + (mp_rlm-1) * devConstants.istep_rtm[2]); 
    // mn_rlm
    idx_rtm[1] = 3*t + devConstants.ncomp * (reg1 + (mn_rlm-1) * devConstants.istep_rtm[2]); 

    vr_rtm[idx_rtm[0] - 2 - 1]  += vr3; 
    vr_rtm[idx_rtm[0] - 1 - 1]  += vr2; 
    vr_rtm[idx_rtm[0] - 1]  += vr1; 
    vr_rtm[idx_rtm[1] - 1 - 1] += vr5; 
    vr_rtm[idx_rtm[1] - 1] += vr4; 
  }

  reg1 = 3*devConstants.nvector + devConstants.ncomp*threadIdx.x*devConstants.istep_rlm[0];
  idx_p_jl = devConstants.nidx_rlm[1]*blockIdx.x+jst-1;
  for(int t=1; t<=devConstants.nscalar; t++) {
    vrs1 = 0;
    for(int j_rlm=jst, deg=degree; j_rlm<=jed; j_rlm++, deg++) {
      idx[0] = reg1 + t + devConstants.ncomp*(j_rlm-1)*devConstants.istep_rlm[1]; 
      idx_p_jl++;
      vrs1 += sp_rlm[idx[0] - 1] * P_jl[idx_p_jl];
    } 
      
    idx_rtm[2] = t + 3*devConstants.nvector + devConstants.ncomp*((blockIdx.x) * devConstants.istep_rtm[1] + threadIdx.x*devConstants.istep_rtm[0] + (mp_rlm-1)*devConstants.istep_rtm[2]); 
    vr_rtm[idx_rtm[2] - 1] = vrs1;
  } 
}

void legendre_b_trans_vector_cuda_(int *ncomp, int *nvector, int *nscalar) {
  
//  static int nShells = *ked - *kst + 1;
  static int nShells = constants.nidx_rtm[0];
  static int nTheta = constants.nidx_rtm[1];

  dim3 grid(nShells, 1);
  int nThreads=0; 
  if((int) nTheta/16 < 10)
    nThreads = 16;
  else if((int) nTheta/32 < 10)
    nThreads = 32;
  else if((int) nTheta/64 < 10)
    nThreads = 64;
  else if((int) nTheta/128 < 10)
    nThreads = 128;
  else if((int) nTheta/256 < 10)
    nThreads = 256;
  else if((int) nTheta/512 < 10)
    nThreads = 512;
  else
    nThreads = 1024;
 
  dim3 block(nThreads, 1,1); 
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm, 3 = a_r_1d_rlm_r

  int jst, jed, m, l;
  int m0=0, m1=0;

  bool begin_set = false, end_set = false;

 
  /*dim3 grid5(nTheta, constants.nidx_rtm[2], 1);
  dim3 block5(nShells,1,1);
  size_t smem =  sizeof(double) * (constants.nvector*5 + constants.nscalar) * nShells;
  transB_m_l_ver5D<<<grid5, block5, smem, streams[0]>>> (deviceInput.lstack_rlm, deviceInput.idx_gl_1d_rlm_j, deviceInput.vr_rtm, deviceInput.sp_rlm, deviceInput.a_r_1d_rlm_r, deviceInput.g_colat_rtm, d_debug.P_smdt, d_debug.dP_smdt, deviceInput.g_sph_rlm, deviceInput.asin_theta_1d_rtm);
*/
  for(int mp_rlm=1; mp_rlm<=constants.nidx_rtm[2]; mp_rlm++) {
    jst = h_debug.lstack_rlm[mp_rlm-1] + 1;
    jed = h_debug.lstack_rlm[mp_rlm]; 
    m = h_debug.idx_gl_1d_rlm_j[constants.nidx_rlm[1]*2 + jst - 1]; 
    l = h_debug.idx_gl_1d_rlm_j[constants.nidx_rlm[1]*1 + jst - 1]; 
   
    if(begin_set == false && l != 0 && l != 1) {
      begin_set = true;
      m0=mp_rlm;
    }
    if(begin_set == true && end_set == false && (l == 0 || l == 1 )) {
      end_set = true;
      m1=mp_rlm-1;
    }
    if(begin_set == true && end_set == false && mp_rlm == constants.nidx_rtm[2]) {
      end_set = true;
      m1 = mp_rlm;
    }
    if(begin_set == true && end_set == true) {
        dim3 grid3(nTheta, m1-m0+1);
        dim3 block3(nShells,1,1);
      #ifdef CUDA_STATIC
//        transB_m_l_ver6D<<<grid3, block3, 2*sizeof(double)*(constants.t_lvl+1), streams[l%32]>>> (deviceInput.lstack_rlm, m0, m1, deviceInput.idx_gl_1d_rlm_j, deviceInput.vr_rtm, deviceInput.sp_rlm, deviceInput.a_r_1d_rlm_r, deviceInput.g_colat_rtm, deviceInput.p_jl, deviceInput.dP_jl, deviceInput.g_sph_rlm, deviceInput.asin_theta_1d_rtm);
        transB_m_l_ver7D<<<grid3, block3, 0, streams[l%32]>>> (deviceInput.lstack_rlm, m0, m1, deviceInput.idx_gl_1d_rlm_j, deviceInput.vr_rtm, deviceInput.sp_rlm, deviceInput.a_r_1d_rlm_r, deviceInput.g_colat_rtm, deviceInput.p_jl, deviceInput.dP_jl, deviceInput.g_sph_rlm, deviceInput.asin_theta_1d_rtm);
      #endif
        //transB_m_l_ver4D<<<grid3, block3, 0, streams[l%32]>>> (deviceInput.lstack_rlm, m0, m1, deviceInput.idx_gl_1d_rlm_j, deviceInput.vr_rtm, deviceInput.sp_rlm, deviceInput.a_r_1d_rlm_r, deviceInput.g_colat_rtm, d_debug.P_smdt, d_debug.dP_smdt, deviceInput.g_sph_rlm, deviceInput.asin_theta_1d_rtm);
    /*    dim3 grid2(nShells,m1-m0+1,nTheta);
       dim3 block2(1,1,1);
        transB_m_l_ver3D<<<grid2, block2, 0, streams[l%32]>>> (deviceInput.lstack_rlm, m0, m1, deviceInput.idx_gl_1d_rlm_j, deviceInput.vr_rtm, deviceInput.sp_rlm, deviceInput.a_r_1d_rlm_r, deviceInput.g_colat_rtm, d_debug.P_smdt, d_debug.dP_smdt, deviceInput.g_sph_rlm, deviceInput.asin_theta_1d_rtm);*/
        //transB_m_l_ver1D<<<grid, block, 0, streams[l%32]>>> (mp_rlm, jst, jed, m, l, deviceInput.vr_rtm, deviceInput.sp_rlm, deviceInput.a_r_1d_rlm_r, deviceInput.g_colat_rtm, d_debug.P_smdt, d_debug.dP_smdt, deviceInput.g_sph_rlm, deviceInput.asin_theta_1d_rtm);
      begin_set = false;
      end_set = false;
    } 
    if(l==0) {
        transB_m_l_eq0_ver1D<<<grid, block, 0, streams[l%32]>>> (mp_rlm, jst, jed, deviceInput.vr_rtm, deviceInput.sp_rlm, deviceInput.a_r_1d_rlm_r, deviceInput.g_colat_rtm, d_debug.P_smdt, d_debug.dP_smdt, deviceInput.g_sph_rlm, deviceInput.asin_theta_1d_rtm);
        continue;
    }
    else if (l==1) {
        transB_m_l_eq1_ver1D<<<grid, block, 0, streams[l%32]>>> (mp_rlm, jst, jed, m, l, deviceInput.vr_rtm, deviceInput.sp_rlm, deviceInput.a_r_1d_rlm_r, deviceInput.g_colat_rtm, d_debug.P_smdt, d_debug.dP_smdt, deviceInput.g_sph_rlm, deviceInput.asin_theta_1d_rtm);
        continue;
    }
//    cudaErrorCheck(cudaDeviceSynchronize());
//    cpy_dev2host_4_debug_();
  } 

#ifdef CUDA_TIMINGS
  cudaErrorCheck(cudaDeviceSynchronize());
#endif
}


