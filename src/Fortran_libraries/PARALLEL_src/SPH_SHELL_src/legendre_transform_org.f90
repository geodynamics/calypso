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
!!      subroutine leg_bwd_trans_vector_org(nb)
!!      subroutine leg_bwd_trans_scalar_org(nb)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_fwd_trans_vector_org(nb)
!!      subroutine leg_fwd_trans_scalar_org(nb)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!@endverbatim
!!
!!@n @param  nb  number of fields to be transformed
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
      subroutine leg_bwd_trans_vector_org(nb)
!
      use legendre_bwd_trans_org
      use clear_schmidt_trans
!
      integer(kind = kint), intent(in) :: nb
!
!
      call clear_b_trans_vector(nb)
      call legendre_b_trans_vector_org(nb)
!
      end subroutine leg_bwd_trans_vector_org
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_scalar_org(nb)
!
      use legendre_bwd_trans_org
      use clear_schmidt_trans
!
      integer(kind = kint), intent(in) :: nb
!
!
      call clear_b_trans_scalar(nb)
      call legendre_b_trans_scalar_org(nb)
!
      end subroutine leg_bwd_trans_scalar_org
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_vector_org(nb)
!
      use legendre_fwd_trans_org
      use clear_schmidt_trans
!
      integer(kind = kint), intent(in) :: nb
!
!
      call clear_f_trans_vector(nb)
      call legendre_f_trans_vector_org(nb)
!
      end subroutine leg_fwd_trans_vector_org
!
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_scalar_org(nb)
!
      use legendre_fwd_trans_org
      use clear_schmidt_trans
!
      integer(kind = kint), intent(in) :: nb
!
!
      call clear_f_trans_scalar(nb)
      call legendre_f_trans_scalar_org(nb)
!
      end subroutine leg_fwd_trans_scalar_org
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_org

