!Author: Harsha Lokavarapu
!Date: 9/13/14

      module cuda_optimizations 
        use calypso_mpi
        use m_work_time
        use iso_c_binding
        use m_precision
        use m_spheric_parameter
        use m_spheric_param_smp
        use m_machine_parameter
        use m_schmidt_poly_on_rtm
        use schmidt_fix_m
        use m_work_4_sph_trans
        use spherical_SRs_N
        use const_coriolis_sph_rlm
        use legendre_bwd_trans_org

        implicit none

        contains
      
        subroutine calypso_gpu_init
          use init_sph_MHD_elapsed_label

          call set_sph_MHD_elapsed_label
          call start_eleps_time(62)
          call initialize_gpu
          call end_eleps_time(62)
        end subroutine calypso_gpu_init
        
        subroutine set_mem_4_gpu
          call setPtrs(idx_gl_1d_rlm_j(1,1))  
          call cpy_schmidt_2_gpu(P_jl(1,1), dPdt_jl(1,1), P_rtm(1,1),   &
     &                           dPdt_rtm(1,1))

          call memcpy_h2d(lstack_rlm(0), a_r_1d_rlm_r(1),g_colat_rtm(1),&
     &                         g_sph_rlm(1,3), g_sph_rlm(1, 7),       &
     &                                         asin_theta_1d_rtm(1),    &
     &                         idx_gl_1d_rlm_j(1,1), radius_1d_rlm_r(1),&
     &                         weight_rtm(1), mdx_p_rlm_rtm(1),         &
     &                         mdx_n_rlm_rtm(1))

        end subroutine set_mem_4_gpu

        subroutine cpy_spectrum_dat_2_gpu(ncomp, sp_rlm)
          integer(kind = kint), intent(in) :: ncomp
          real(kind = kreal), intent(in) :: sp_rlm(ncomp*nnod_rlm)
          
          call set_spectrum_data(sp_rlm(1), ncomp)
        end subroutine cpy_spectrum_dat_2_gpu

        subroutine cpy_spectrum_dat_from_gpu(ncomp, sp_rlm)
          integer(kind = kint), intent(in) :: ncomp
          real(kind = kreal), intent(in) :: sp_rlm(ncomp*nnod_rlm)
          
          call retrieve_spectrum_data(sp_rlm(1), ncomp)
        end subroutine cpy_spectrum_dat_from_gpu
       
        subroutine cpy_physical_dat_from_gpu(ncomp, vr_rtm)
          integer(kind = kint), intent(in) :: ncomp
          real(kind = kreal), intent(in) :: vr_rtm(ncomp*nnod_rtm)
          
          call retrieve_physical_data(vr_rtm(1), ncomp)
        end subroutine cpy_physical_dat_from_gpu

        subroutine cpy_physical_dat_2_gpu(ncomp, vr_rtm)
          integer(kind = kint), intent(in) :: ncomp
          real(kind = kreal), intent(in) :: vr_rtm(ncomp*nnod_rtm)
          
          call set_physical_data(vr_rtm(1), ncomp)
        end subroutine cpy_physical_dat_2_gpu

        subroutine calypso_gpu_finalize
          call start_eleps_time(61)
          call cleangpu
          call end_eleps_time(61)
        end subroutine calypso_gpu_finalize
  
        subroutine sync_device 
          call cuda_sync_device
        end subroutine sync_device 
      end module cuda_optimizations 
