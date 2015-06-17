!>@file   legendre_transform_cuda.f90
!!@brief  module legendre_transform_cuda
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Legendre transforms
!!       (Original version)
!!
!!
!!@verbatim
!!      subroutine leg_backward_trans_cuda                               &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_forwawd_trans_cuda                                &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_transform_cuda
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_cuda                                 &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans_spin
      use spherical_SRs_N
      use cuda_optimizations
      use legendre_bwd_trans_org 
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call calypso_rlm_from_recv_N(ncomp, n_WR, WR, sp_rlm_wk(1))
#if defined(CUDA_DEBUG)
      call clear_bwd_legendre_work(ncomp)
#endif
      
!
      call start_eleps_time(57) 
      call clear_field_data(ncomp)
      call cpy_spectrum_dat_2_gpu(ncomp, sp_rlm_wk(1)) 
#if defined(CUDA_TIMINGS)
      call sync_device
#endif
      call end_eleps_time(57) 

      if(nvector .gt. 0 .OR. nscalar .gt. 0) then
        call start_eleps_time(59) 
        call legendre_b_trans_cuda(ncomp, nvector, nscalar)
        call end_eleps_time(59) 
#if defined(CUDA_DEBUG) || defined(CHECK_SCHMIDT_OTF)
          call legendre_b_trans_vector_org(ncomp, nvector, sp_rlm_wk(1) &
     &       , vr_rtm_wk(1))
#endif
      end if
#if defined(CUDA_DEBUG) || defined(CHECK_SCHMIDT_OTF)
      if(nscalar .gt. 0) then
        call legendre_b_trans_scalar_org                                &
     &     (ncomp, nvector, nscalar, sp_rlm_wk(1), vr_rtm_wk(1))
      end if
#endif

      call start_eleps_time(58) 
#if defined(CUDA_DEBUG) || defined(CHECK_SCHMIDT_OTF)
      call cpy_field_dev2host_4_debug
#else 
      call cpy_physical_dat_from_gpu(ncomp, vr_rtm_wk(1))
#endif
#if defined(CUDA_TIMINGS)
      call sync_device
#endif
      call end_eleps_time(58) 
!

#if defined(CUDA_DEBUG) || defined(CHECK_SCHMIDT_OTF)
        call check_bwd_trans_cuda(my_rank, vr_rtm_wk(1), P_jl(1,1),     &
     &            dPdt_jl(1,1))
#endif
      call finish_send_recv_rj_2_rlm
      call calypso_rtm_to_send_N(ncomp, n_WS, vr_rtm_wk(1), WS(1))
!
      end subroutine leg_backward_trans_cuda
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_cuda                                  &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans_spin
      use spherical_SRs_N
      use legendre_fwd_trans_org 
      use cuda_optimizations
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call calypso_rtm_from_recv_N(ncomp, n_WR, WR, vr_rtm_wk(1))
#if defined(CUDA_DEBUG)
      call clear_fwd_legendre_work(ncomp)
#endif
!
      call clear_spectrum_data(ncomp)
      call cpy_physical_dat_2_gpu(ncomp, vr_rtm_wk(1)) 

      if(nvector .gt. 0 .OR. nscalar .gt. 0) then
        call start_eleps_time(60)
        call legendre_f_trans_cuda(ncomp, nvector, nscalar) 
#if defined(CUDA_TIMINGS)
        call sync_device
#endif
        call end_eleps_time(60)
#if defined(CUDA_DEBUG) || defined(CHECK_SCHMIDT_OTF)
          call legendre_f_trans_vector_org(ncomp, nvector, vr_rtm_wk(1) &
     &       , sp_rlm_wk(1))
#endif
      end if
#if defined(CUDA_DEBUG) || defined(CHECK_SCHMIDT_OTF)
      if(nscalar .gt. 0) then
        call legendre_f_trans_scalar_org                                &
     &     (ncomp, nvector, nscalar, vr_rtm_wk(1), sp_rlm_wk(1))
      end if
#endif

        call start_eleps_time(58) 
#if defined(CUDA_DEBUG) || defined(CHECK_SCHMIDT_OTF)
         call cpy_spec_dev2host_4_debug
#elif defined(CUDA_OPTIMIZED)
        call cpy_spectrum_dat_from_gpu(ncomp, sp_rlm_wk(1))
#endif
#if defined(CUDA_TIMINGS)
        call sync_device
#endif
        call end_eleps_time(58) 
!

#if defined(CUDA_DEBUG) || defined(CHECK_SCHMIDT_OTF)
        call check_fwd_trans_cuda(my_rank, sp_rlm_wk(1))
#endif
!
      call finish_send_recv_rtp_2_rtm
      call calypso_rlm_to_send_N(ncomp, n_WS, sp_rlm_wk(1), WS)
!
      end subroutine leg_forward_trans_cuda
!
      end module legendre_transform_cuda
