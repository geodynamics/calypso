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
void transB_m_l_eq0_ver1D(const int mp_rlm, const int jst, const int jed, double *vr_rtm, double *sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *P_smdt, double *dP_smdt, double *g_sph_rlm, double *asin_theta_1d_rtm) {
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
      idx2 = 3*devConstants.nvector + devConstants.ncomp * ((j_rlm-1) * devConstants.istep_rlm[1] + blockIdx.x * devConstants.istep_rlm[0]); 
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
    
    idx_rtm[0] = devConstants.ncomp * ((threadIdx.x + i*blockDim.x) * devConstants.istep_rtm[1] + blockIdx.x*devConstants.istep_rtm[0] + (mp_rlm-1) * devConstants.istep_rtm[2]); 
    idx_rtm[1] = devConstants.ncomp * ((threadIdx.x + i*blockDim.x) * devConstants.istep_rtm[1] + blockIdx.x*devConstants.istep_rtm[0] + (mn_rlm-1) * devConstants.istep_rtm[2]); 
    idx2 = devConstants.nvector*3 + devConstants.ncomp * ((threadIdx.x + i*blockDim.x) * devConstants.istep_rtm[1] + blockIdx.x*devConstants.istep_rtm[0] + (mp_rlm-1) * devConstants.istep_rtm[2]); 
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
void transB_m_l_eq1_ver1D(const int mp_rlm, const int jst, const int jed, int order, int degree, double *vr_rtm, double *sp_rlm, double *a_r_1d_rlm_r, double *g_colat_rtm, double *P_smdt, double *dP_smdt, double *g_sph_rlm, double *asin_theta_1d_rtm) {
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
      idx[0] = devConstants.nvector*3 + devConstants.ncomp * ((j_rlm-1) * devConstants.istep_rlm[1] + blockIdx.x * devConstants.istep_rlm[0]); 
      for(int t=1; t<=devConstants.nvector; t++) {
        idx[1] += 3;
        reg2 = __dmul_rd(__dmul_rd(-1 * scaleBySine(order, p_m_l_0, theta), (double) order), asin_theta_1d_rtm[id + i*blockDim.x]);         
        vr_reg[t*5 - 5] += sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[blockIdx.x] * reg2;
        vr_reg[t*5 - 4] += sp_rlm[idx[1] - 2] * a_r_1d_rlm_r[blockIdx.x] * reg2;
        vr_reg[t*5 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[blockIdx.x], a_r_1d_rlm_r[blockIdx.x]) * scaleBySine(order, p_m_l_0, theta) * g_sph_rlm[j_rlm-1];    
        vr_reg[t*5 - 2] += sp_rlm[idx[1] - 2]  * a_r_1d_rlm_r[blockIdx.x] * dp_m_l;    
        vr_reg[t*5 - 1] -= sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[blockIdx.x] * dp_m_l;    
      }
      for(int t=1; t<=devConstants.nscalar; t++) {
        idx[0] += 1;
        vr_reg_scalar[t-1] += sp_rlm[idx[0] - 1] * scaleBySine(order, p_m_l_0, theta);
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
    
    idx_rtm[0] = devConstants.ncomp * ((threadIdx.x + i*blockDim.x) * devConstants.istep_rtm[1] + blockIdx.x*devConstants.istep_rtm[0] + (mp_rlm-1) * devConstants.istep_rtm[2]); 
    idx_rtm[1] = devConstants.ncomp * ((threadIdx.x + i*blockDim.x) * devConstants.istep_rtm[1] + blockIdx.x*devConstants.istep_rtm[0] + (mn_rlm-1) * devConstants.istep_rtm[2]); 
    idx_rtm[2] = 3*devConstants.nvector + devConstants.ncomp * ((threadIdx.x + i*blockDim.x) * devConstants.istep_rtm[1] + blockIdx.x*devConstants.istep_rtm[0] + (mp_rlm-1) * devConstants.istep_rtm[2]); 
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
      idx[0] = 3*devConstants.nvector + devConstants.ncomp * ((j_rlm-1) * devConstants.istep_rlm[1] + blockIdx.x * devConstants.istep_rlm[0]); 
      for(int t=1; t<=devConstants.nvector; t++) {
        idx[1] += 3;
        reg1 = scaleBySine(order, p_m_l_0, theta);
        reg1 *= g_sph_rlm[j_rlm-1];
        reg2 = __dmul_rd(__dmul_rd(-1 * scaleBySine(order, p_m_l_0, theta), (double) order), asin_theta_1d_rtm[id + i*blockDim.x]);         
        vr_reg[t*5 - 5] += sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[blockIdx.x] * reg2;
        vr_reg[t*5 - 4] += sp_rlm[idx[1] - 2] * a_r_1d_rlm_r[blockIdx.x] * reg2;
        //vr_reg[t*3 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[blockIdx.x], a_r_1d_rlm_r[blockIdx.x]) * scaleBySine(order, p_m_l_0, theta) * g_sph_rlm[j_rlm-1];    
        vr_reg[t*5 - 3] += sp_rlm[idx[1] - 3] * __dmul_rd(a_r_1d_rlm_r[blockIdx.x], a_r_1d_rlm_r[blockIdx.x]) * reg1;    
        vr_reg[t*5 - 2] += sp_rlm[idx[1] - 2]  * a_r_1d_rlm_r[blockIdx.x] * dp_m_l;    
        vr_reg[t*5 - 1] -= sp_rlm[idx[1] - 1] * a_r_1d_rlm_r[blockIdx.x] * dp_m_l;    
      }
      
      for(int t=1; t<=devConstants.nscalar; t++) {
        idx[0] += 1;
        vr_reg_scalar[t-1] += sp_rlm[idx[0] - 1] * scaleBySine(order, p_m_l_0, theta);
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
      //j = (deg+2)*(deg+3) + order+1;
      //P_smdt[(i*blockDim.x + id)*devConstants.nidx_rlm[1] + j] = scaleBySine(order+1, p_mp_l_0, theta);
      p_mp_l_1 = reg3;
        
    }
    // mp_rlm 
    idx_rtm[0] = devConstants.ncomp * ((threadIdx.x + i*blockDim.x) * devConstants.istep_rtm[1] + blockIdx.x*devConstants.istep_rtm[0] + (mp_rlm-1) * devConstants.istep_rtm[2]); 
    // mn_rlm
    idx_rtm[1] = devConstants.ncomp * ((threadIdx.x + i*blockDim.x) * devConstants.istep_rtm[1] + blockIdx.x*devConstants.istep_rtm[0] + (mn_rlm-1) * devConstants.istep_rtm[2]); 
    idx_rtm[2] = 3*devConstants.nvector + devConstants.ncomp * ((threadIdx.x + i*blockDim.x) * devConstants.istep_rtm[1] + blockIdx.x*devConstants.istep_rtm[0] + (mp_rlm-1) * devConstants.istep_rtm[2]); 
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

void legendre_b_trans_vector_cuda_(int *ncomp, int *nvector, int *nscalar) {
  
//  static int nShells = *ked - *kst + 1;
  static int nShells = constants.nidx_rtm[0];
  static int nTheta = constants.nidx_rtm[1];

  constants.ncomp = *ncomp; 
  constants.nscalar = *nscalar;
  constants.nvector = *nvector;

  initDevConstVariables();
   
  dim3 grid(nShells, 1);
  dim3 block(16,1,1);  
  // Current: 0 = vr_rtm, 1 = sp_rlm, 2 = g_sph_rlm, 3 = a_r_1d_rlm_r

  int jst, jed, m, l;
  for(int mp_rlm=1; mp_rlm<=constants.nidx_rtm[2]; mp_rlm++) {
    jst = h_debug.lstack_rlm[mp_rlm-1] + 1;
    jed = h_debug.lstack_rlm[mp_rlm];
    m = h_debug.idx_gl_1d_rlm_j[constants.nidx_rlm[1]*2 + jst - 1]; 
    l = h_debug.idx_gl_1d_rlm_j[constants.nidx_rlm[1]*1 + jst - 1]; 

    if(l==0) {
        transB_m_l_eq0_ver1D<<<grid, block, 0, streams[l%32]>>> (mp_rlm, jst, jed, deviceInput.vr_rtm, deviceInput.sp_rlm, deviceInput.a_r_1d_rlm_r, deviceInput.g_colat_rtm, d_debug.P_smdt, d_debug.dP_smdt, deviceInput.g_sph_rlm, deviceInput.asin_theta_1d_rtm);
    }
    else if (l==1) {
        transB_m_l_eq1_ver1D<<<grid, block, 0, streams[l%32]>>> (mp_rlm, jst, jed, m, l, deviceInput.vr_rtm, deviceInput.sp_rlm, deviceInput.a_r_1d_rlm_r, deviceInput.g_colat_rtm, d_debug.P_smdt, d_debug.dP_smdt, deviceInput.g_sph_rlm, deviceInput.asin_theta_1d_rtm);
    }
    else { 
        transB_m_l_ver1D<<<grid, block, 0, streams[l%32]>>> (mp_rlm, jst, jed, m, l, deviceInput.vr_rtm, deviceInput.sp_rlm, deviceInput.a_r_1d_rlm_r, deviceInput.g_colat_rtm, d_debug.P_smdt, d_debug.dP_smdt, deviceInput.g_sph_rlm, deviceInput.asin_theta_1d_rtm);
    } 
//    cudaErrorCheck(cudaDeviceSynchronize());
//    cpy_dev2host_4_debug_();
  } 

#ifdef CUDA_TIMINGS
  cudaErrorCheck(cudaDeviceSynchronize());
#endif
}


