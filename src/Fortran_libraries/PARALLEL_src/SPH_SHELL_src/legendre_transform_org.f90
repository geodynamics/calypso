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
!!      subroutine leg_bwd_trans_vector_org(ncomp, nvector)
!!      subroutine leg_bwd_trans_scalar_org(ncomp, nvector, nscalar)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_fwd_trans_vector_org(ncomp, nvector)
!!      subroutine leg_fwd_trans_scalar_org(ncomp, nvector, nscalar)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
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
      subroutine leg_bwd_trans_vector_org(ncomp, nvector)
!
      use legendre_bwd_trans_org
      use merge_polidal_toroidal_v
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
!
      call clear_bwd_legendre_trans(ncomp)
      call legendre_b_trans_vector_org(ncomp, nvector)
      call const_vect_sph_b_trans(ncomp, nvector)
!
      end subroutine leg_bwd_trans_vector_org
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_scalar_org(ncomp, nvector, nscalar)
!
      use legendre_bwd_trans_org
      use merge_polidal_toroidal_v
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      call clear_bwd_legendre_trans(ncomp)
      call legendre_b_trans_scalar_org(ncomp, nvector, nscalar)
!
      end subroutine leg_bwd_trans_scalar_org
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_vector_org(ncomp, nvector)
!
      use legendre_fwd_trans_org
      use merge_polidal_toroidal_v
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
!
      call prod_r_vect_sph_f_trans(ncomp, nvector)
!
      call clear_fwd_legendre_trans(ncomp)
      call legendre_f_trans_vector_org(ncomp, nvector)
!
      end subroutine leg_fwd_trans_vector_org
!
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_scalar_org(ncomp, nvector, nscalar)
!
      use legendre_fwd_trans_org
      use merge_polidal_toroidal_v
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      call clear_fwd_legendre_trans(ncomp)
      call legendre_f_trans_scalar_org(ncomp, nvector, nscalar)
!
      end subroutine leg_fwd_trans_scalar_org
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_org

