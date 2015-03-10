!>@file   legendre_transform_krin.f90
!!@brief  module legendre_transform_krin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Legendre transforms
!!       (innermost loop is spherical harmonics)
!!
!!
!!@verbatim
!!    Backward transforms
!      subroutine leg_bwd_trans_fields_krin                             &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_fwd_trans_fields_krin                            &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_transform_krin
!
      use m_precision
      use m_work_4_sph_trans_spin
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_fields_krin                              &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use m_sph_communicators
      use m_sph_trans_comm_table
      use legendre_bwd_trans_krin
      use ordering_schmidt_trans_krin
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call order_b_trans_fields_krin(ncomp, nvector, nscalar,           &
     &    irev_sr_rlm, n_WR, WR, sp_rlm_wk(1))
!
      call legendre_b_trans_vector_krin(ncomp, nvector,                 &
     &    sp_rlm_wk(1), vr_rtm_wk(1))
      call legendre_b_trans_scalar_krin(ncomp, nvector, nscalar,        &
     &    sp_rlm_wk(1), vr_rtm_wk(1))
!
      call finish_send_recv_rj_2_rlm
      call back_b_trans_fields_krin(ncomp, nvector, nscalar,            &
     &    vr_rtm_wk(1), nmax_sr_rtp, nneib_domain_rtm,                  &
     &    istack_sr_rtm, item_sr_rtm, WS(1))
!
      end subroutine leg_bwd_trans_fields_krin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_fields_krin                              &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use m_sph_communicators
      use m_sph_trans_comm_table
      use legendre_fwd_trans_krin
      use ordering_schmidt_trans_krin
      use spherical_SRs_N
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      call order_f_trans_fields_krin(ncomp, nvector, nscalar,           &
     &    irev_sr_rtm, n_WR, WR, vr_rtm_wk(1))
!
      call legendre_f_trans_vector_krin(ncomp, nvector,                 &
     &    vr_rtm_wk(1), sp_rlm_wk(1))
      call legendre_f_trans_scalar_krin(ncomp, nvector, nscalar,        &
     &    vr_rtm_wk(1), sp_rlm_wk(1))
!
      call finish_send_recv_rtp_2_rtm
      call back_f_trans_fields_krin(ncomp, nvector, nscalar,            &
     &    sp_rlm_wk(1), nmax_sr_rj, nneib_domain_rlm,                   &
     &    istack_sr_rlm, item_sr_rlm, WS(1))
!
      end subroutine leg_fwd_trans_fields_krin
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_krin
