!Author: Harsha Lokavarapu
!Date: 9/13/14

      module cuda_optimizations 
        use calypso_mpi
        use m_work_time
        use iso_c_binding
        use m_precision
        use m_spheric_parameter
        use m_machine_parameter
        use m_schmidt_poly_on_rtm
        use m_work_4_sph_trans
        use FFT_selector
        use merge_polidal_toroidal_v
        use spherical_SRs_N
        use const_coriolis_sph_rlm

        implicit none
  
        contains
      
        subroutine bwd_transform(nvector_2_trans, nscalar_2_trans)
!      &
!     &                ncomp_trans)
          integer(kind=kint), intent(in) :: nvector_2_trans,            &
     &                                      nscalar_2_trans
         
          integer(kind = kint) :: Nstacksmp(0:np_smp) 
          integer(kind = kint) :: nvector, ncomp_FFT, ncomp_trans
!tmp 
          ncomp_trans = 21
          nvector = ncomp_trans/3
! ncomp is ncomp_trans
          ncomp_FFT = ncomp_trans*nidx_rtp(1)*nidx_rtp(2)
          Nstacksmp(0:np_smp) = ncomp_trans*irt_rtp_smp_stack(0:np_smp)
          vr_rtp(1:ncomp_trans*nnod_rtp) = 0.0d0

          call send_recv_rj_2_rlm_N(ncomp_trans, sp_rj, sp_rlm)
        !  call send_recv_rj_2_rlm_N(ncomp_trans, sp_rj(1), sp_rlm(1))

          call sum_coriolis_rlm(ncomp_trans, sp_rlm)
        
          call clear_bwd_legendre_trans(ncomp_trans)
          call transform_b(ncomp_trans, nvector, ncomp_trans)

          call send_recv_rtm_2_rtp_N(ncomp_trans, vr_rtm, vr_rtp)
          call send_recv_rtm_2_rtp_N(ncomp_trans, vr_rtm(1), vr_rtp(1))

          call backward_FFT_select(np_smp, Nstacksmp, ncomp_FFT,        &
     &            nidx_rtp(3), vr_rtp)

           call backward_FFT_select(np_smp, Nstacksmp, ncomp_FFT,       &
     &            nidx_rtp(3), vr_rtp(1))
        end subroutine bwd_transform

        subroutine fwd_transform(ncomp_, nvector_, nscalar_)
          integer(kind=kint), intent(in) :: ncomp_, nvector_, nscalar_
          call transform_f(ncomp_, nvector_, nscalar_)
        end subroutine fwd_transform

        subroutine clean_cuda
          call cleanup()
        end subroutine clean_cuda
 
      end module cuda_optimizations 
