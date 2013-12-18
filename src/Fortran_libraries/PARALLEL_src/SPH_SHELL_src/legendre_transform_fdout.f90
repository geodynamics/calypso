!>@file   legendre_transform_fdout.f90
!!@brief  module legendre_transform_fdout
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
!!      subroutine leg_bwd_trans_vector_fdout(nb, nvector)
!!      subroutine leg_bwd_trans_scalar_fdout(nb, nvector, nscalar)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_fwd_trans_vector_fdout(nb, nvector)
!!      subroutine leg_fwd_trans_scalar_fdout(nb, nvector, nscalar)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!@endverbatim
!!
!!@param   nb       Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_transform_fdout
!
      use m_precision
      use m_work_4_sph_trans_fldout
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_vector_fdout(nb, nvector)
!
      use legendre_bwd_trans_fdout
      use ordering_lag_trans_fldout
!
      integer(kind = kint), intent(in) :: nb, nvector
!
!
      call order_b_trans_vector_fldout(nb, nvector)
      call clear_b_trans_vector_fldout(nvector)
!
      call legendre_b_trans_vector_fdout(nvector)
!
      call back_b_trans_vector_fldout(nb, nvector)
!
      end subroutine leg_bwd_trans_vector_fdout
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_scalar_fdout(nb, nvector, nscalar)
!
      use legendre_bwd_trans_fdout
      use ordering_lag_trans_fldout
!
      integer(kind = kint), intent(in) :: nb, nvector, nscalar
!
!
      call order_b_trans_scalar_fldout(nb, nvector, nscalar)
      call clear_b_trans_scalar_fldout(nscalar)
!
      call legendre_b_trans_scalar_fdout(nscalar)
!
      call back_b_trans_scalar_fldout(nb, nvector, nscalar)
!
      end subroutine leg_bwd_trans_scalar_fdout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_vector_fdout(nb, nvector)
!
      use legendre_fwd_trans_fdout
      use ordering_lag_trans_fldout
!
      integer(kind = kint), intent(in) :: nb, nvector
!
!
      call order_f_trans_vector_fldout(nb, nvector)
      call clear_f_trans_vector_fldout(nvector)
!
      call legendre_f_trans_vector_fdout(nvector)
!
      call back_f_trans_vector_fldout(nb, nvector)
!
      end subroutine leg_fwd_trans_vector_fdout
!
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_scalar_fdout(nb, nvector, nscalar)
!
      use legendre_fwd_trans_fdout
      use ordering_lag_trans_fldout
!
      integer(kind = kint), intent(in) :: nb, nvector, nscalar
!
!
      call order_f_trans_scalar_fldout(nb, nvector, nscalar)
      call clear_f_trans_scalar_fldout(nscalar)
!
      call legendre_f_trans_scalar_fdout(nscalar)
!
      call back_f_trans_scalar_fldout(nb, nvector, nscalar)
!
      end subroutine leg_fwd_trans_scalar_fdout
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_fdout
