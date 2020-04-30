!>@file   single_pt_sph_mean_square.f90
!!@brief  module single_pt_sph_mean_square
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      real(kind = kreal) function sph_scalar_mean_square              &
!!     &                          (radius_1d_rj_r, g_rj_11, d_pol)
!!      real(kind = kreal) function center_scalar_mean_square(d_rj)
!!
!!      subroutine sph_vector_mean_square(a_r_1d_rj_r, g_rj_3, g_rj_12, &
!!     &          d_pol, d_dpdr, d_tor, sq_out)
!!      subroutine degree0_vector_mean_square                           &
!!     &         (a_r_1d_rj_r, d_pol, sq_out)
!!      subroutine center_vector_mean_square(d_pol, sq_out)
!!
!!      subroutine one_point_mean_sq_to_energy(ene_out)
!!@endverbatim
!!
      module single_pt_sph_mean_square
!
      use m_precision
      use m_constants
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      real(kind = kreal) function sph_scalar_mean_square                &
     &                          (radius_1d_rj_r, g_rj_11, d_pol)
!
      real(kind = kreal), intent(in) :: g_rj_11
      real(kind = kreal), intent(in) :: radius_1d_rj_r
      real(kind = kreal), intent(in) :: d_pol
!
!
      sph_scalar_mean_square                                            &
     &        = g_rj_11 * (d_pol * radius_1d_rj_r)**2
!
      end function sph_scalar_mean_square
!
! -----------------------------------------------------------------------
!
      real(kind = kreal) function center_scalar_mean_square(d_rj)
!
      real(kind = kreal), intent(in) :: d_rj
!
!
      center_scalar_mean_square = d_rj**2
!
      end function center_scalar_mean_square
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_vector_mean_square(a_r_1d_rj_r, g_rj_3, g_rj_12,   &
     &          d_pol, d_dpdr, d_tor, sq_out)
!
      real(kind = kreal), intent(in) :: g_rj_3, g_rj_12
      real(kind = kreal), intent(in) :: a_r_1d_rj_r
      real(kind = kreal), intent(in) :: d_pol, d_dpdr, d_tor
!
      real(kind=kreal), intent(inout) :: sq_out(3)
!
!
      sq_out(1) = g_rj_12 * (d_dpdr**2                                  &
     &           + g_rj_3 * (a_r_1d_rj_r * d_pol)**2)
      sq_out(2) = g_rj_12 * d_tor**2
      sq_out(3) =  sq_out(1) + sq_out(2)
!
      end subroutine sph_vector_mean_square
!
! -----------------------------------------------------------------------
!
      subroutine degree0_vector_mean_square                             &
     &         (a_r_1d_rj_r, d_pol, sq_out)
!
      real(kind = kreal), intent(in) :: a_r_1d_rj_r
      real(kind=kreal), intent(in) :: d_pol
!
      real(kind=kreal), intent(inout) :: sq_out(3)
!
!
      sq_out(1) = (half * d_pol * a_r_1d_rj_r)**2
      sq_out(2) = zero
      sq_out(3) = sq_out(1)
!
      end subroutine degree0_vector_mean_square
!
! -----------------------------------------------------------------------
!
      subroutine center_vector_mean_square(d_pol, sq_out)
!
      real(kind=kreal), intent(in) :: d_pol
!
      real(kind=kreal), intent(inout) :: sq_out(3)
!
!
      sq_out(1) = (half * d_pol)**2
      sq_out(2) = zero
      sq_out(3) = sq_out(1)
!
      end subroutine center_vector_mean_square
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine one_point_mean_sq_to_energy(ene_out)
!
      real(kind=kreal), intent(inout) :: ene_out(3)
!
!
      ene_out(1) = half * ene_out(1)
      ene_out(2) = half * ene_out(2)
      ene_out(3) = half * ene_out(3)
!
      end subroutine one_point_mean_sq_to_energy
!
! -----------------------------------------------------------------------
!
      end module single_pt_sph_mean_square
