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
!!      subroutine leg_bwd_trans_vector_krin(ncomp, nvector)
!!      subroutine leg_bwd_trans_scalar_krin(ncomp, nvector, nscalar)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_fwd_trans_vector_krin(ncomp, nvector)
!!      subroutine leg_fwd_trans_scalar_krin(ncomp, nvector, nscalar)
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
      use m_work_4_sph_trans_krin
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_vector_krin(ncomp, nvector)
!
      use legendre_bwd_trans_krin
      use ordering_schmidt_trans_krin
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
!
      call order_b_trans_vector_krin(ncomp, nvector, sp_rlm_krin(1,1))
      call clear_b_trans_krin(ione, 3*nvector)
!
      call legendre_b_trans_vector_krin(nvector,                        &
     &    sp_rlm_krin(1,1), vr_rtm_krin(1,1))
!
      call back_b_trans_vector_krin(ncomp, nvector, vr_rtm_krin(1,1))
!
      end subroutine leg_bwd_trans_vector_krin
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_scalar_krin(ncomp, nvector, nscalar)
!
      use legendre_bwd_trans_krin
      use ordering_schmidt_trans_krin
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint) :: ist_fld
!
!
      ist_fld = 3*nvector+1
      call order_b_trans_scalar_krin(ncomp, nvector, nscalar,           &
     &    sp_rlm_krin(1,ist_fld))
      call clear_b_trans_krin(ist_fld,3*nvector+nscalar)
!
      call legendre_b_trans_scalar_krin(nscalar,                        &
     &    sp_rlm_krin(1,ist_fld), vr_rtm_krin(1,ist_fld))
!
      call back_b_trans_scalar_krin(ncomp, nvector, nscalar,            &
     &    vr_rtm_krin(1,ist_fld))
!
      end subroutine leg_bwd_trans_scalar_krin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_vector_krin(ncomp, nvector)
!
      use legendre_fwd_trans_krin
      use ordering_schmidt_trans_krin
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
!
      call order_f_trans_vector_krin(ncomp, nvector, vr_rtm_krin(1,1))
      call clear_f_trans_krin(ione, 3*nvector)
!
      call legendre_f_trans_vector_krin(nvector,                        &
     &    vr_rtm_krin(1,1), sp_rlm_krin(1,1))
!
      call back_f_trans_vector_krin(ncomp, nvector, sp_rlm_krin(1,1))
!
      end subroutine leg_fwd_trans_vector_krin
!
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_scalar_krin(ncomp, nvector, nscalar)
!
      use legendre_fwd_trans_krin
      use ordering_schmidt_trans_krin
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint) :: ist_fld
!
!
      ist_fld = 3*nvector+1
      call order_f_trans_scalar_krin(ncomp, nvector, nscalar,           &
     &    vr_rtm_krin(1,ist_fld))
      call clear_f_trans_krin(ist_fld, 3*nvector+nscalar)
!
      call legendre_f_trans_scalar_krin(nscalar,                        &
     &    vr_rtm_krin(1,ist_fld), sp_rlm_krin(1,ist_fld))
!
      call back_f_trans_scalar_krin(ncomp, nvector, nscalar,            &
     &    sp_rlm_krin(1,ist_fld))
!
      end subroutine leg_fwd_trans_scalar_krin
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_krin
