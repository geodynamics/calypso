!>@file   legendre_transform_matmul.f90
!!@brief  module legendre_transform_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Legendre transforms
!!       (longest loop version)
!!
!!
!!@verbatim
!!      subroutine leg_backward_trans_matmul                            &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!      subroutine leg_backward_trans_dgemm                             &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!      subroutine leg_backward_trans_matprod                           &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_forward_trans_matmul                            &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!      subroutine leg_forward_trans_dgemm                             &
!!     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!!      subroutine leg_forward_trans_matprod                           &
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
      module legendre_transform_matmul
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
      subroutine leg_backward_trans_matmul                              &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use legendre_bwd_trans_matmul
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
        call leg_b_trans_vector_matmul(ncomp, nvector,                  &
     &      irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
      end if
      if(nscalar .gt. 0) then
        call leg_b_trans_scalar_matmul(ncomp, nvector, nscalar,         &
     &      irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
      end if
!
      end subroutine leg_backward_trans_matmul
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_matmul                               &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use legendre_fwd_trans_matmul
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
        call leg_f_trans_vector_matmul(ncomp, nvector,                  &
     &      irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
      end if
      if(nscalar .gt. 0) then
        call leg_f_trans_scalar_matmul(ncomp, nvector, nscalar,         &
     &      irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
      end if
!
      end subroutine leg_forward_trans_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_dgemm                               &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use legendre_bwd_trans_matmul
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
        call leg_b_trans_vector_dgemm(ncomp, nvector,                   &
     &      irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
      end if
      if(nscalar .gt. 0) then
        call leg_b_trans_scalar_dgemm(ncomp, nvector, nscalar,          &
     &      irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
      end if
!
      end subroutine leg_backward_trans_dgemm
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_dgemm                                &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use legendre_fwd_trans_matmul
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
        call leg_f_trans_vector_dgemm(ncomp, nvector,                   &
     &      irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
      end if
      if(nscalar .gt. 0) then
        call leg_f_trans_scalar_dgemm(ncomp, nvector, nscalar,          &
     &      irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
      end if
!
      end subroutine leg_forward_trans_dgemm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_matprod                             &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use legendre_bwd_trans_matmul
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
        call leg_b_trans_vector_matprod(ncomp, nvector,                 &
     &      irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
      end if
      if(nscalar .gt. 0) then
        call leg_b_trans_scalar_matprod(ncomp, nvector, nscalar,        &
     &      irev_sr_rlm, irev_sr_rtm, n_WR, n_WS, WR, WS)
      end if
!
      end subroutine leg_backward_trans_matprod
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_matprod                              &
     &         (ncomp, nvector, nscalar, n_WR, n_WS, WR, WS)
!
      use legendre_fwd_trans_matmul
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
        call leg_f_trans_vector_matprod(ncomp, nvector,                 &
     &      irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
      end if
      if(nscalar .gt. 0) then
        call leg_f_trans_scalar_matprod(ncomp, nvector, nscalar,        &
     &      irev_sr_rtm, irev_sr_rlm, n_WR, n_WS, WR, WS)
      end if
!
      end subroutine leg_forward_trans_matprod
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_matmul
