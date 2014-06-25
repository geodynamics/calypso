!>@file   legendre_transform_spin.f90
!!@brief  module legendre_transform_spin
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
!!      subroutine leg_backward_trans_spin(ncomp, nvector)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_forward_trans_spin(ncomp, nvector)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_transform_spin
!
      use m_precision
      use m_work_time
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
      subroutine leg_backward_trans_spin(ncomp, nvector, nscalar)
!
      use ordering_schmidt_trans_spin
      use ordering_schmidt_trans_krin
      use legendre_bwd_trans_spin
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      call start_eleps_time(25)
      call order_b_trans_fields_spin(ncomp, nvector, nscalar,           &
     &    sp_rlm(1), sp_rlm_wk(1))
      call end_eleps_time(25)
!
      call start_eleps_time(27)
      if(nvector .gt. 0) call legendre_b_trans_vector_spin              &
     &                      (ncomp, nvector,                            &
     &                       sp_rlm_wk(1), vr_rtm_wk(1))
      if(nscalar .gt. 0) call legendre_b_trans_scalar_spin              &
     &                      (ncomp, nvector, nscalar,                   &
     &                       sp_rlm_wk(1), vr_rtm_wk(1))
      call end_eleps_time(27)
!
      call start_eleps_time(28)
      call back_b_trans_fields_krin(ncomp, nvector, nscalar,            &
     &    vr_rtm_wk(1), vr_rtm(1))
      call end_eleps_time(28)
!
      end subroutine leg_backward_trans_spin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_spin(ncomp, nvector, nscalar)
!
      use ordering_schmidt_trans_spin
      use ordering_schmidt_trans_krin
      use legendre_fwd_trans_spin
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      call start_eleps_time(29)
      call order_f_trans_fields_spin(ncomp, nvector, nscalar,           &
     &    vr_rtm(1), vr_rtm_wk(1))
      call end_eleps_time(29)
!
      call start_eleps_time(31)
      if(nvector .gt. 0) call legendre_f_trans_vector_spin              &
     &                      (ncomp, nvector,  vr_rtm_wk(1),             &
     &                       sp_rlm_wk(1))
      if(nscalar .gt. 0) call legendre_f_trans_scalar_spin              &
     &                      (ncomp, nvector, nscalar, vr_rtm_wk(1),     &
     &                       sp_rlm_wk(1))
      call end_eleps_time(31)
!
      call start_eleps_time(32)
      call back_f_trans_fields_krin(ncomp, nvector, nscalar,            &
     &    sp_rlm_wk(1), sp_rlm(1))
      call end_eleps_time(32)
!
      end subroutine leg_forward_trans_spin
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_spin

