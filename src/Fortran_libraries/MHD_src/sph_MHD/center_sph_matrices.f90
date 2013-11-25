!>@file   center_sph_matrices.f90
!!@brief  module center_sph_matrices
!!
!!@author H. Matsui
!!@date Programmed in Apr, 2009
!
!>@brief  Construct matrix for center
!!
!!@verbatim
!!      subroutine add_vector_poisson_mat_center(nri, jmax, r_CTR1,     &
!!     &          fdm2_fix_fld_center, coef_p, mat3)
!!      subroutine add_scalar_poisson_mat_center(nri, jmax, r_CTR1,     &
!!     &          fdm2_fix_fld_center, coef_p, mat3)
!!      subroutine add_scalar_poisson_mat_filled(j0, nri, jmax, r_CTR1, &
!!     &          fdm2_fix_fld_center, fdm2_fix_dr_center, coef_p, mat3)
!!@endverbatim
!
!!@n @param jmax         Number of local spherical harmonics mode
!
!
      module center_sph_matrices
!
      use m_precision
!
      use m_constants
      use m_t_int_parameter
      use m_schmidt_poly_on_rtm
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_vector_poisson_mat_center(nri, jmax, r_CTR1,       &
     &          fdm2_fix_fld_center, coef_p, mat3)
!
      integer(kind = kint), intent(in) :: jmax, nri
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: fdm2_fix_fld_center(-1:1,3)
!
      real(kind = kreal), intent(inout) :: mat3(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        mat3(2,1,j) = mat3(2,1,j) - coef_p * (fdm2_fix_fld_center(0,3)  &
     &                      - g_sph_rj(j,3)*r_CTR1(2) )
        mat3(1,2,j) = mat3(1,2,j) - coef_p *  fdm2_fix_fld_center(1,3)
      end do
!
      end subroutine add_vector_poisson_mat_center
!
! -----------------------------------------------------------------------
!
      subroutine add_scalar_poisson_mat_center(nri, jmax, r_CTR1,       &
     &          fdm2_fix_fld_center, coef_p, mat3)
!
      integer(kind = kint), intent(in) :: jmax, nri
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: fdm2_fix_fld_center(-1:1,3)
!
      real(kind = kreal), intent(inout) :: mat3(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        mat3(2,1,j) = mat3(2,1,j) - coef_p * (fdm2_fix_fld_center(0,3)  &
     &                      + two*r_CTR1(1) * fdm2_fix_fld_center(0,2)  &
     &                      - g_sph_rj(j,3)*r_CTR1(2) )
        mat3(1,2,j) = mat3(1,2,j) - coef_p * (fdm2_fix_fld_center(1,3)  &
     &                      + two*r_CTR1(1) * fdm2_fix_fld_center(1,2))
      end do
!
      end subroutine add_scalar_poisson_mat_center
!
! -----------------------------------------------------------------------
!
      subroutine add_scalar_poisson_mat_filled(j0, nri, jmax, r_CTR1,   &
     &          fdm2_fix_fld_center, fdm2_fix_dr_center, coef_p, mat3)
!
      integer(kind = kint), intent(in) :: jmax, nri, j0
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: fdm2_fix_fld_center(-1:1,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_center(-1:1,3)
!
      real(kind = kreal), intent(inout) :: mat3(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = j0 + 1, jmax
        mat3(2,1,j) = mat3(2,1,j) - coef_p * (fdm2_fix_fld_center(0,3)  &
     &                      + two*r_CTR1(1) * fdm2_fix_fld_center(0,2)  &
     &                      - g_sph_rj(j,3)*r_CTR1(2) )
        mat3(1,2,j) = mat3(1,2,j) - coef_p * (fdm2_fix_fld_center(1,3)  &
     &                      + two*r_CTR1(1) * fdm2_fix_fld_center(1,2))
      end do
!
      if(j0 .eq. 0) return
      mat3(2,1,j0) = mat3(2,1,j0) - coef_p * (fdm2_fix_dr_center(0,3)   &
     &                             - g_sph_rj(j0,3)*r_CTR1(2) )
      mat3(1,2,j0) = mat3(1,2,j0) - coef_p *  fdm2_fix_dr_center(1,3)
!
      end subroutine add_scalar_poisson_mat_filled
!
! -----------------------------------------------------------------------
!
      end module center_sph_matrices
