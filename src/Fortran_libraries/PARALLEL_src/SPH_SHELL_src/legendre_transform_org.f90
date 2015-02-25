!>@file   legendre_transform_org.f90
!!@brief  module legendre_transform_org
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
!!      subroutine leg_backward_trans_org                               &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!      subroutine leg_backward_trans_blocked                           &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!      subroutine leg_backward_trans_sym_org,                          &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_forwawd_trans_org                                &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!      subroutine leg_forwawd_trans_blocked                            &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!      subroutine leg_forward_trans_sym_org                            &
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
      module legendre_transform_org
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
      subroutine leg_backward_trans_org                                 &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans_spin
      use legendre_bwd_trans_org
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call calypso_rlm_from_recv_N(ncomp, n_WR, WR, sp_rlm_wk(1))
      call clear_bwd_legendre_work(ncomp)
!
      if(nvector .gt. 0) then
        call legendre_b_trans_vector_org                                &
     &     (ncomp, nvector, sp_rlm_wk(1), vr_rtm_wk(1))
      end if
      if(nscalar .gt. 0) then
        call legendre_b_trans_scalar_org                                &
     &     (ncomp, nvector, nscalar, sp_rlm_wk(1), vr_rtm_wk(1))
      end if
!
      call finish_send_recv_rj_2_rlm
      call calypso_rtm_to_send_N(ncomp, n_WS, vr_rtm_wk(1), WS(1))
!
      end subroutine leg_backward_trans_org
!
! -----------------------------------------------------------------------
!
      subroutine leg_forwawd_trans_org                                  &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use m_work_4_sph_trans_spin
      use legendre_fwd_trans_org
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call calypso_rtm_from_recv_N(ncomp, n_WR, WR, vr_rtm_wk(1))
      call clear_fwd_legendre_work(ncomp)
!
      if(nvector .gt. 0) then
        call legendre_f_trans_vector_org                               &
     &     (ncomp, nvector, vr_rtm_wk(1), sp_rlm_wk(1))
      end if
      if(nscalar .gt. 0) then
        call legendre_f_trans_scalar_org                               &
     &     (ncomp, nvector, nscalar, vr_rtm_wk(1), sp_rlm_wk(1))
      end if
!
      call finish_send_recv_rtp_2_rtm
      call calypso_rlm_to_send_N(ncomp, n_WS, sp_rlm_wk(1), WS)
!
      end subroutine leg_forwawd_trans_org
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_blocked                             &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use m_sph_trans_comm_table
      use legendre_bwd_trans_blocked
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call finish_send_recv_rj_2_rlm
!$omp parallel workshare
      WS(1:ncomp*ntot_item_sr_rtm) = 0.0d0
!$omp end parallel workshare
!
      if(nvector .gt. 0) then
        call leg_b_trans_vector_blocked(ncomp, nvector,                 &
     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR(1), WS(1))
      end if
      if(nscalar .gt. 0) then
        call leg_b_trans_scalar_blocked(ncomp, nvector, nscalar,        &
     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR(1), WS(1))
      end if
!
      end subroutine leg_backward_trans_blocked
!
! -----------------------------------------------------------------------
!
      subroutine leg_forwawd_trans_blocked                              &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use m_sph_trans_comm_table
      use legendre_fwd_trans_blocked
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call finish_send_recv_rtp_2_rtm
!$omp parallel workshare
      WS(1:ncomp*ntot_item_sr_rlm) = 0.0d0
!$omp end parallel workshare
!
      if(nvector .gt. 0) then
        call leg_f_trans_vector_blocked(ncomp, nvector,                &
     &      irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR(1), WS(1))
      end if
      if(nscalar .gt. 0) then
        call leg_f_trans_scalar_blocked(ncomp, nvector, nscalar,       &
     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR(1), WS(1))
      end if
!
      end subroutine leg_forwawd_trans_blocked
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_sym_org                             &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use m_sph_trans_comm_table
      use legendre_bwd_trans_symmetry
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call finish_send_recv_rj_2_rlm
!$omp parallel workshare
      WS(1:ncomp*ntot_item_sr_rtm) = 0.0d0
!$omp end parallel workshare
!
      if(nvector .gt. 0) then
        call leg_bwd_trans_vector_sym_org(ncomp, nvector,               &
     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
      end if
      if(nscalar .gt. 0) then
        call leg_bwd_trans_scalar_sym_org(ncomp, nvector, nscalar,      &
     &          irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
      end if
!
      end subroutine leg_backward_trans_sym_org
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_sym_org                              &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use m_sph_trans_comm_table
      use legendre_fwd_trans_symmetry
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call finish_send_recv_rtp_2_rtm
!$omp parallel workshare
      WS(1:ncomp*ntot_item_sr_rlm) = 0.0d0
!$omp end parallel workshare
!
      if(nvector .gt. 0) then
        call leg_fwd_trans_vector_sym_org(ncomp, nvector,               &
     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
      end if
      if(nscalar .gt. 0) then
        call leg_fwd_trans_scalar_sym_org(ncomp, nvector, nscalar,      &
     &          irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
      end if
!
      end subroutine leg_forward_trans_sym_org
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_org
