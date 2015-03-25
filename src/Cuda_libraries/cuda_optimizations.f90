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
        use FFT_selector
        use merge_polidal_toroidal_v
        use spherical_SRs_N
        use const_coriolis_sph_rlm
       
        implicit none
  
        contains
      
        subroutine initialize_gpu(ncomp)
        integer(kind = kint), intent(in) :: ncomp
          call initgpu(nnod_rtp, nnod_rtm, nnod_rlm, nidx_rtm(1),       &
     &                      nidx_rlm(1), istep_rtm(1), istep_rlm(1),    &
     &                      ncomp, g_sph_rlm(1,3), a_r_1d_rlm_r(1),     &
     &                      lstack_rlm(1), g_colat_rtm(1),            &
     &                      l_truncation)
        end subroutine initialize_gpu

        subroutine legendre_b_trans_vector_cuda                         &
     &         (ncomp, nvector, nscalar, sp_rlm, vr_rtm)
!
        integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
        real(kind = kreal), intent(inout) :: sp_rlm(ncomp*nnod_rlm)
        real(kind = kreal), intent(inout) :: vr_rtm(ncomp*nnod_rtm)
        
        integer(kind = kint) :: ip, kst, ked, jst, jed

!        kst = idx_rtm_smp_stack(ip-1,1) + 1 
!        ked = idx_rtm_smp_stack(np_smp,1) 
!        jst = lstack_rlm(0) + 1
!        jed = lstack_rlm(nidx_rtm(3))

!        call transform_b(ncomp, nvector, nscalar, kst, ked, jst, jed) 
        call transform_b(ncomp, nvector, nscalar, sp_rlm(1)) 

        end subroutine legendre_b_trans_vector_cuda                     

        subroutine fwd_transform(ncomp_, nvector_, nscalar_)
          integer(kind=kint), intent(in) :: ncomp_, nvector_, nscalar_
        !  call transform_f(ncomp_, nvector_, nscalar_)
        end subroutine fwd_transform

        subroutine cpy_spectrum_dat_2_gpu(ncomp, sp_rlm)
          integer(kind = kint), intent(in) :: ncomp
          real(kind = kreal), intent(in) :: sp_rlm(ncomp*nnod_rlm)
          
          call set_spectrum_data(sp_rlm(1))
        end subroutine cpy_spectrum_dat_2_gpu

       
        subroutine cpy_physical_dat_from_gpu(ncomp, vr_rtm)
          integer(kind = kint), intent(in) :: ncomp
          real(kind = kreal), intent(in) :: vr_rtm(ncomp*nnod_rtm)
          
          call retrieve_physical_data(vr_rtm(1))
        end subroutine cpy_physical_dat_from_gpu

        subroutine finalize_gpu
          call cleangpu
        end subroutine finalize_gpu 
 
      end module cuda_optimizations 
