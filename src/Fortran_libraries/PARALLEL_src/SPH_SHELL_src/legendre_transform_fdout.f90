!>@file   legendre_transform_fdout.f90
!!@brief  module legendre_transform_fdout
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Legendre transforms
!!       (outermost loop is for fields)
!!
!!
!!@verbatim
!!    Backward transforms
!!      subroutine leg_backward_trans_fdout(ncomp, nvector, nscalar)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_forward_trans_fdout(ncomp, nvector,nscalar)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_transform_fdout
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
      subroutine leg_backward_trans_fdout(ncomp, nvector, nscalar)
!
      use ordering_leg_trans_fdout
      use legendre_bwd_trans_fdout
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint) :: ist_sp_rlm, ist_vr_rtm
!
!
      ist_vr_rtm = 1 + 3*nvector*nnod_rtm
      ist_sp_rlm = 1 + 3*nvector*nnod_rlm
!
      call order_b_trans_fields_fdout(ncomp,                            &
     &    sp_rlm(1), sp_rlm_wk(1))
!
      call legendre_b_trans_vector_fdout(nvector,                       &
     &    sp_rlm_wk(1), vr_rtm_wk(1))
      call legendre_b_trans_scalar_fdout(nscalar,                       &
     &    sp_rlm_wk(ist_sp_rlm), vr_rtm_wk(ist_vr_rtm))
!
      call back_b_trans_fields_fdout(ncomp, vr_rtm_wk(1), vr_rtm(1))
!
      end subroutine leg_backward_trans_fdout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_fdout(ncomp, nvector, nscalar)
!
      use ordering_leg_trans_fdout
      use legendre_fwd_trans_fdout
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint) :: ist_sp_rlm, ist_vr_rtm
!
!
!
      ist_vr_rtm = 1 + 3*nvector*nnod_rtm
      ist_sp_rlm = 1 + 3*nvector*nnod_rlm
!
      call order_f_trans_fields_fdout(ncomp, vr_rtm(1), vr_rtm_wk(1))
!
      call legendre_f_trans_vector_fdout(nvector,                       &
     &    vr_rtm_wk(1), sp_rlm_wk(1))
      call legendre_f_trans_scalar_fdout(nscalar,                       &
     &    vr_rtm_wk(ist_vr_rtm), sp_rlm_wk(ist_sp_rlm))
!
      call back_f_trans_fields_fdout(ncomp,                             &
     &    sp_rlm_wk(1), sp_rlm(1))
!
      end subroutine leg_forward_trans_fdout
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_fdout

