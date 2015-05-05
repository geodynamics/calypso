#include <cuda_runtime.h>
#include "legendre_poly.h"
#include <math.h>
#include <iostream>

void check_bwd_trans_cuda_(int *my_rank, double *vr_rtm, double *P_jl, double *dP_jl) {
  #if defined(CUDA_DEBUG) || defined(CHECK_SCHMIDT_OTF)
    std::ofstream schmidt, field_vec, field_slr;
    schmidt.open("cuda_schimdt.log");
    field_vec.open("cuda_field_vectors.log");
    field_slr.open("cuda_field_scalars.log");
      
    schmidt << "order\t degree\t P_smdt \t P_smdt_cuda\t dP_smdt \t dP_smdt_cuda\n";

    double error=0, eps = 1E-7;
    int jst, jed, ip_rtm, in_rtm, pos, m, l, mn_rlm;

    for(int mp_rlm=1; mp_rlm<=constants.nidx_rtm[2]; mp_rlm++){
      jst = h_debug.lstack_rlm[mp_rlm-1] + 1; 
      jed = h_debug.lstack_rlm[mp_rlm];
      for(int l_rtm=1; l_rtm<=constants.nidx_rtm[1]; l_rtm++) {
        for(int j_rlm=jst; j_rlm <=jed; j_rlm++) {
          m = h_debug.idx_gl_1d_rlm_j[constants.nidx_rlm[1]*2 + j_rlm-1];
          l = h_debug.idx_gl_1d_rlm_j[constants.nidx_rlm[1] + j_rlm-1];
          pos = (l_rtm-1)*constants.nidx_rlm[1] + l*(l+1) + m;
          schmidt << m << "\t" <<  l << "\t" << P_jl[constants.nidx_rlm[1]*(l_rtm-1) + j_rlm-1] << "\t" << h_debug.P_smdt[pos]<< "\t" << dP_jl[constants.nidx_rlm[1]*(l_rtm-1) + j_rlm-1] << "\t" << h_debug.dP_smdt[pos] << "\n"; 
        }
      }
    }
   schmidt.close();

   field_vec << "shell\tmeridian\tmp_rlm\tvector_index\t vr_rtm_cu[0]\t vr_rtm[0] \t vr_rtm_cu[1] \t vr_rtm[1] \t vr_rtm_cu[2] \t vr_rtm[2] \t vr_rtm_n_cu[0] \t vr_rtm_n[0] \t vr_Rtm_n_cu[1] \t vr_rtm_n[1]\n";
   field_slr << "shell\tmeridian\tmp_rlm\tscalar\t vr_rtm_cu[0]\t vr_rtm[0] \n";

    for(int k=1; k<=constants.nidx_rtm[0]; k++) {
      for(int mp_rlm=1; mp_rlm<=constants.nidx_rtm[2]; mp_rlm++) {
        jst = h_debug.lstack_rlm[mp_rlm-1] + 1; 
        jed = h_debug.lstack_rlm[mp_rlm];
        mn_rlm = constants.nidx_rtm[2] - mp_rlm + 1;
        for(int l_rtm=1; l_rtm <=constants.nidx_rtm[1]; l_rtm++) {
          for(int nd=1; nd<=constants.nvector; nd++) {
            ip_rtm = 3*nd + constants.ncomp*((l_rtm-1)*constants.istep_rtm[1] + (k-1)*constants.istep_rtm[0] + (mp_rlm-1)*constants.istep_rtm[2]) - 1;
            in_rtm = 3*nd + constants.ncomp*((l_rtm-1)*constants.istep_rtm[1] + (k-1)*constants.istep_rtm[0] + (mn_rlm-1)*constants.istep_rtm[2]) - 1;
            field_vec << k << "\t" << l_rtm << "\t" << mp_rlm << "\t" << nd << "\t"<< h_debug.vr_rtm[ip_rtm-2] << "\t" << vr_rtm[ip_rtm-2] << "\t" << h_debug.vr_rtm[ip_rtm-1] << "\t" << vr_rtm[ip_rtm-1] << "\t" << h_debug.vr_rtm[ip_rtm] << "\t" << vr_rtm[ip_rtm] << "\t" << h_debug.vr_rtm[in_rtm-1] << "\t" << vr_rtm[in_rtm-1] << h_debug.vr_rtm[in_rtm] << "\t" << vr_rtm[in_rtm] << "\n";
          }
          for(int nd=1; nd<=constants.nscalar; nd++) {
            ip_rtm = nd + 3*constants.nvector + constants.ncomp*((l_rtm-1)*constants.istep_rtm[1] + (k-1)*constants.istep_rtm[0] + (mp_rlm-1)*constants.istep_rtm[2]) - 1;
            field_slr << k << "\t" << l_rtm << "\t" << mp_rlm << "\t" << nd << "\t" << h_debug.vr_rtm[ip_rtm] << "\t" << vr_rtm[ip_rtm] << "\n";
          }
/*              error = abs(h_debug.P_smdt[pos] - P_jl[constants.nidx_rlm[1]*(l_rtm-1) + j_rlm-1]); 
              if(error > eps) { 
                data_log << "Error of " << error << "at P_smdt for l=" << l<< " and m=" << m <<std::endl; 
                //exit(-1);
              }
              error = abs(h_debug.dP_smdt[pos] - dP_jl[constants.nidx_rlm[1]*(l_rtm-1) + j_rlm-1]); 
              if(error > eps) { 
                data_log << "Error of " << error << "at dP_smdt for l=" << l<< " and m=" << m <<std::endl; 
                //exit(-1);
              }
              error = abs(h_debug.vr_rtm[ip_rtm-2] - vr_rtm[ip_rtm-2]); 
              if(error > eps) { 
                data_log << "Error of " << error << "at vr_rtm first comp for l=" << l<< " and m=" << m <<std::endl; 
                //exit(-1);
              }
              error = abs(h_debug.vr_rtm[ip_rtm-1] - vr_rtm[ip_rtm-1]); 
              if(error > eps) { 
                data_log << "Error of " << error << "at vr_rtm mid comp for l=" << l<< " and m=" << m <<std::endl; 
                //exit(-1);
              }
              error = abs(h_debug.vr_rtm[ip_rtm] - vr_rtm[ip_rtm]); 
              if(error > eps) { 
                data_log << "Error of " << error << "at vr_rtm last comp for l=" << l<< " and m=" << m <<std::endl; 
                //exit(-1);
              }
              vr_rtm_log<<h_debug.vr_rtm[in_rtm-1] << "\t" << vr_rtm[in_rtm-1] << "\n";
              vr_rtm_log<<h_debug.vr_rtm[in_rtm] << "\t" << vr_rtm[in_rtm] << "\n";
              vr_rtm_log<<h_debug.vr_rtm[ip_rtm-2] << "\t" << vr_rtm[ip_rtm-2] << "\n";
              vr_rtm_log<<h_debug.vr_rtm[ip_rtm-1] << "\t" << vr_rtm[ip_rtm-1] << "\n";
              vr_rtm_log<<h_debug.vr_rtm[ip_rtm] << "\t" << vr_rtm[ip_rtm] << "\n";
            */
        }
      }
    }
   
   field_vec.close();
   field_slr.close();
   exit(-1);
  #endif
}
