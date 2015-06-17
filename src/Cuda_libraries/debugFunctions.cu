#include <cuda_runtime.h>
#include "legendre_poly.h"
#include <math.h>
#include <sstream>

void check_bwd_trans_cuda_(int *my_rank, double *vr_rtm, double *P_jl, double *dP_jl) {

  #if defined(CUDA_DEBUG) || defined(CHECK_SCHMIDT_OTF)
    std::string fName;
    std::stringstream sc;
    sc << *my_rank;
    #if defined(CUDA_OTF)
      std::ofstream schmidt;
    #endif
    std::ofstream field_vec, field_slr;
    //std::string fName = "cuda_schmidt" + "_" + sc.str() + ".log");
    #if defined(CUDA_OTF) 
      fName = "cuda_schmidt_" + sc.str() + ".log";
      schmidt.open(fName.c_str());
    #endif
    fName = "cuda_field_vectors_" + sc.str() + ".log";
    field_vec.open(fName.c_str());
    fName = "cuda_field_scalars_" + sc.str() + ".log";
    field_slr.open(fName.c_str());
      
    #if defined(CUDA_OTF) 
      schmidt << "order\t degree\t P_smdt \t P_smdt_cuda\t dP_smdt \t dP_smdt_cuda\n";
    #endif

    double error=0, eps = 1E-7;
    int jst, jed, ip_rtm, in_rtm, pos, m, l, mn_rlm;

  #if defined(CUDA_OTF) 
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
  #endif

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
            if(h_debug.vr_rtm[ip_rtm] != vr_rtm[ip_rtm] || h_debug.vr_rtm[ip_rtm-1] != vr_rtm[ip_rtm-1] || h_debug.vr_rtm[ip_rtm-2] != vr_rtm[ip_rtm-2] || h_debug.vr_rtm[in_rtm] != vr_rtm[in_rtm] || h_debug.vr_rtm[in_rtm-1] != vr_rtm[in_rtm-1])
              field_vec << k << "\t" << l_rtm << "\t" << mp_rlm << "\t" << nd << "\t"<< h_debug.vr_rtm[ip_rtm-2] << "\t" << vr_rtm[ip_rtm-2] << "\t" << h_debug.vr_rtm[ip_rtm-1] << "\t" << vr_rtm[ip_rtm-1] << "\t" << h_debug.vr_rtm[ip_rtm] << "\t" << vr_rtm[ip_rtm] << "\t" << h_debug.vr_rtm[in_rtm-1] << "\t" << vr_rtm[in_rtm-1] <<"\t" << h_debug.vr_rtm[in_rtm] << "\t" << vr_rtm[in_rtm] << "\n";
          }
          for(int nd=1; nd<=constants.nscalar; nd++) {
            ip_rtm = nd + 3*constants.nvector + constants.ncomp*((l_rtm-1)*constants.istep_rtm[1] + (k-1)*constants.istep_rtm[0] + (mp_rlm-1)*constants.istep_rtm[2]) - 1;
            if(h_debug.vr_rtm[ip_rtm] != vr_rtm[ip_rtm])
              field_slr << k << "\t" << l_rtm << "\t" << mp_rlm << "\t" << nd << "\t" << h_debug.vr_rtm[ip_rtm] << "\t" << vr_rtm[ip_rtm] << "\n";
          }
        }
      }
    }
   
   field_vec.close();
   field_slr.close();
  #endif
}


void check_fwd_trans_cuda_(int *my_rank, double *sp_rlm) {
  #if defined(CUDA_DEBUG) || defined(CHECK_SCHMIDT_OTF)
    std::string fName;
    std::stringstream sc;
    sc << *my_rank;
  #if defined(CUDA_OTF) 
    std::ofstream schmidt;
  #endif
    std::ofstream spec_vec, spec_slr;
  #if defined(CUDA_OTF) 
    fName = "cuda_f_schmidt_" + sc.str() + ".log";
    schmidt.open(fName.c_str());
  #endif
    fName = "cuda_spec_vectors_" + sc.str() + ".log";
    spec_vec.open(fName.c_str());
    fName = "cuda_spec_scalars_" + sc.str() + ".log";
    spec_slr.open(fName.c_str());
      
  #if defined(CUDA_OTF) 
    schmidt << "order\t degree\t P_smdt \t P_smdt_cuda\t dP_smdt \t dP_smdt_cuda\n";
    schmidt.close();
  #endif

    double error=0, eps = 1E-7;
    int jst, jed, ip_rtm, in_rtm, pos, m, l, mn_rlm;
    int i_rlm;
   spec_vec<< "shell\tmode\tvector_index\t sp_rlm_cu[0]\t sp_rlm[0] \t sp_rlm_cu[1] \t sp_rlm[1] \t sp_rlm_cu[2] \t sp_rlm[2] \n";
   spec_slr<< "shell\tmode\tvector_index\t sp_rlm_cu[0]\t sp_rlm[0]\n";

    for(int k=1; k<=constants.nidx_rtm[0]; k++) {
      for(int j_rlm=1; j_rlm <=constants.nidx_rlm[1]; j_rlm++) {
        for(int nd=1; nd<=constants.nvector; nd++) {
          i_rlm = 3*nd + constants.ncomp*((j_rlm-1)*constants.istep_rlm[1] + (k-1)*constants.istep_rlm[0]) - 1;
            if(h_debug.sp_rlm[i_rlm] != sp_rlm[i_rlm] || h_debug.sp_rlm[i_rlm-1] != sp_rlm[i_rlm-1] || h_debug.sp_rlm[i_rlm-2] != sp_rlm[i_rlm-2] ) {
              spec_vec << k << "\t" << j_rlm << "\t" << nd << "\t"<< h_debug.sp_rlm[i_rlm-2] << "\t" << sp_rlm[i_rlm-2] << "\t" << h_debug.sp_rlm[i_rlm-1] << "\t" << sp_rlm[i_rlm-1] << "\t" << h_debug.sp_rlm[i_rlm] << "\t" << sp_rlm[i_rlm] << "\n";
          }
        }
        for(int nd=1; nd<=constants.nscalar; nd++) {
          i_rlm = nd + 3*constants.nvector + constants.ncomp*((j_rlm-1)*constants.istep_rlm[1] + (k-1)*constants.istep_rlm[0]) - 1;
          if(h_debug.sp_rlm[i_rlm] != sp_rlm[i_rlm]) {
            spec_slr<< k << "\t" << j_rlm << "\t" << nd << "\t" << h_debug.sp_rlm[i_rlm] << "\t" << sp_rlm[i_rlm] << "\n";
          }
        }
      }
    }
   
   spec_vec.close();
   spec_slr.close();
  #endif
}
