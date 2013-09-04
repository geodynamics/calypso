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
!!      subroutine leg_bwd_trans_vector_spin(nb)
!!      subroutine leg_bwd_trans_scalar_spin(nb)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_fwd_trans_vector_spin(nb)
!!      subroutine leg_fwd_trans_scalar_spin(nb)
!!      subroutine leg_fwd_trans_grad_spin(nb)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!@endverbatim
!!
!!@n @param  nb  number of fields to be transformed
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
      subroutine leg_bwd_trans_vector_spin(nb)
!
      use ordering_schmidt_trans_spin
      use legendre_bwd_trans_spin
!
      integer(kind = kint), intent(in) :: nb
!
!
      call order_b_trans_vector_spin(nb)
      call clear_b_trans_vector_spin(nb)
!
      call legendre_b_trans_vector_spin(nb)
!
      call back_b_trans_vector_spin(nb)
!
      end subroutine leg_bwd_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_scalar_spin(nb)
!
      use ordering_schmidt_trans_spin
      use legendre_bwd_trans_spin
!
      integer(kind = kint), intent(in) :: nb
!
!
      call order_b_trans_scalar_spin(nb)
      call clear_b_trans_scalar_spin(nb)
!
      call legendre_b_trans_scalar_spin(nb)
!
      call back_b_trans_scalar_spin(nb)
!
      end subroutine leg_bwd_trans_scalar_spin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_vector_spin(nb)
!
      use ordering_schmidt_trans_spin
      use legendre_fwd_trans_spin
!
      integer(kind = kint), intent(in) :: nb
!
!
      call order_f_trans_vector_spin(nb)
      call clear_f_trans_vector_spin(nb)
!
      call legendre_f_trans_vector_spin(nb)
!
      call back_f_trans_vector_spin(nb)
!
      end subroutine leg_fwd_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_scalar_spin(nb)
!
      use ordering_schmidt_trans_spin
      use legendre_fwd_trans_spin
!
      integer(kind = kint), intent(in) :: nb
!
!
      call order_f_trans_scalar_spin(nb)
      call clear_f_trans_scalar_spin(nb)
!
      call legendre_f_trans_scalar_spin(nb)
!
      call back_f_trans_scalar_spin(nb)
!
      end subroutine leg_fwd_trans_scalar_spin
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_spin

