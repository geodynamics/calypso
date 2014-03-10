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
!!      subroutine leg_bwd_trans_vector_fdout(ncomp, nvector)
!!      subroutine leg_bwd_trans_scalar_fdout(ncomp, nvector, nscalar)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_fwd_trans_vector_fdout(ncomp, nvector)
!!      subroutine leg_fwd_trans_scalar_fdout(ncomp, nvector,nscalar)
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
      use m_work_4_sph_trans_fdout
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_vector_fdout(ncomp, nvector)
!
      use ordering_leg_trans_fdout
      use legendre_bwd_trans_fdout
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
!
      call order_b_trans_vector_fdout(ncomp, nvector,                   &
     &    sp_rlm_fdout(1,1))
      call clear_b_trans_field_fdout(ione, 3*nvector)
!
      call legendre_b_trans_vector_fdout(nvector,                       &
     &    sp_rlm_fdout(1,1), vr_rtm_fdout(1,1))
!
      call back_b_trans_vector_fdout(ncomp, nvector, vr_rtm_fdout(1,1))
!
      end subroutine leg_bwd_trans_vector_fdout
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_scalar_fdout(ncomp, nvector, nscalar)
!
      use ordering_leg_trans_fdout
      use legendre_bwd_trans_fdout
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint) :: ist_fld
!
!
      ist_fld = 3*nvector+1
      call order_b_trans_scalar_fdout(ncomp, nvector, nscalar,          &
     &    sp_rlm_fdout(1,ist_fld))
      call clear_b_trans_field_fdout(ist_fld, 3*nvector+nscalar)
!
      call legendre_b_trans_scalar_fdout(nscalar,                       &
     &    sp_rlm_fdout(1,ist_fld), vr_rtm_fdout(1,ist_fld))
!
      call back_b_trans_scalar_fdout(ncomp, nvector, nscalar,           &
     &    vr_rtm_fdout(1,ist_fld))
!
      end subroutine leg_bwd_trans_scalar_fdout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_vector_fdout(ncomp, nvector)
!
      use ordering_leg_trans_fdout
      use legendre_fwd_trans_fdout
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
!
      call order_f_trans_vector_fdout(ncomp, nvector,                   &
     &    vr_rtm_fdout(1,1))
      call clear_f_trans_field_fdout(ione, 3*nvector)
!
      call legendre_f_trans_vector_fdout(nvector,                       &
     &    vr_rtm_fdout(1,1), sp_rlm_fdout(1,1))
!
      call back_f_trans_vector_fdout(ncomp, nvector, sp_rlm_fdout(1,1))
!
      end subroutine leg_fwd_trans_vector_fdout
!
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_scalar_fdout(ncomp, nvector, nscalar)
!
      use ordering_leg_trans_fdout
      use legendre_fwd_trans_fdout
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint) :: ist_fld
!
!
      ist_fld = 3*nvector+1
      call order_f_trans_scalar_fdout(ncomp, nvector, nscalar,          &
     &    vr_rtm_fdout(1,ist_fld))
      call clear_f_trans_field_fdout(ist_fld, nvector+nscalar)
!
      call legendre_f_trans_scalar_fdout(nscalar,                       &
     &    vr_rtm_fdout(1,ist_fld), sp_rlm_fdout(1,ist_fld))
!
      call back_f_trans_scalar_fdout(ncomp, nvector, nscalar,           &
     &    sp_rlm_fdout(1,ist_fld))
!
      end subroutine leg_fwd_trans_scalar_fdout
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_fdout

