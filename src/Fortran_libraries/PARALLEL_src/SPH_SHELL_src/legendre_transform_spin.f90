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
!!      subroutine leg_bwd_trans_vector_spin(ncomp, nvector)
!!      subroutine leg_bwd_trans_scalar_spin(ncomp, nvector, nscalar)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_fwd_trans_vector_spin(ncomp, nvector)
!!      subroutine leg_fwd_trans_scalar_spin(ncomp, nvector,nscalar)
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
      subroutine leg_bwd_trans_vector_spin(ncomp, nvector)
!
      use ordering_schmidt_trans_spin
      use legendre_bwd_trans_spin
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
!
      call order_b_trans_vector_spin(ncomp, nvector)
      call clear_b_trans_vector_spin(nvector)
!
      call legendre_b_trans_vector_spin(nvector)
!
      call back_b_trans_vector_spin(ncomp, nvector)
!
      end subroutine leg_bwd_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_scalar_spin(ncomp, nvector, nscalar)
!
      use ordering_schmidt_trans_spin
      use legendre_bwd_trans_spin
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      call order_b_trans_scalar_spin(ncomp, nvector, nscalar)
      call clear_b_trans_scalar_spin(nscalar)
!
      call legendre_b_trans_scalar_spin(nscalar)
!
      call back_b_trans_scalar_spin(ncomp, nvector, nscalar)
!
      end subroutine leg_bwd_trans_scalar_spin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_vector_spin(ncomp, nvector)
!
      use ordering_schmidt_trans_spin
      use legendre_fwd_trans_spin
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
!
      call order_f_trans_vector_spin(ncomp, nvector)
      call clear_f_trans_vector_spin(nvector)
!
      call legendre_f_trans_vector_spin(nvector)
!
      call back_f_trans_vector_spin(ncomp, nvector)
!
      end subroutine leg_fwd_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_scalar_spin(ncomp, nvector, nscalar)
!
      use ordering_schmidt_trans_spin
      use legendre_fwd_trans_spin
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      call order_f_trans_scalar_spin(ncomp, nvector, nscalar)
      call clear_f_trans_scalar_spin(nscalar)
!
      call legendre_f_trans_scalar_spin(nscalar)
!
      call back_f_trans_scalar_spin(ncomp, nvector, nscalar)
!
      end subroutine leg_fwd_trans_scalar_spin
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_spin

