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
!!      subroutine leg_backward_trans_org(ncomp, nvector)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_forwawd_trans_org(ncomp, nvector)
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
      subroutine leg_backward_trans_org(ncomp, nvector, nscalar)
!
      use legendre_bwd_trans_org
      use merge_polidal_toroidal_v
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      if(nvector .gt. 0) then
        call legendre_b_trans_vector_org(ncomp, nvector)
      end if
      if(nscalar .gt. 0) then
        call legendre_b_trans_scalar_org(ncomp, nvector, nscalar)
      end if
!
!      call const_vect_sph_b_trans(ncomp, nvector)
!
      end subroutine leg_backward_trans_org
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_forwawd_trans_org(ncomp, nvector, nscalar)
!
      use legendre_fwd_trans_org
      use merge_polidal_toroidal_v
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      if(nvector .gt. 0) then
        call legendre_f_trans_vector_org(ncomp, nvector)
      end if
      if(nscalar .gt. 0) then
        call legendre_f_trans_scalar_org(ncomp, nvector, nscalar)
      end if
!
      end subroutine leg_forwawd_trans_org
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_org

