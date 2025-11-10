!>@file   center_sph_matrices.f90
!!@brief  module center_sph_matrices
!!
!!@author H. Matsui
!!@date Programmed in Apr, 2009
!
!>@brief  Construct matrix for center
!!
!!@verbatim
!!      subroutine add_vector_poisson_mat_center(nri, jmax, g_sph_rj,   &
!!     &          r_CTR1, fdm2_fix_fld_ctr1, coef_p, mat3)
!!      subroutine add_scalar_poisson_mat_ctr1(nri, jmax, g_sph_rj,     &
!!     &          r_CTR1, fdm2_fix_fld_ctr1, coef_p, mat3)
!!      subroutine add_scl_val_diffuse_mat_ctr1                         &
!!     &         (nri, jmax, g_sph_rj, r_CTR1, fdm2_fix_fld_ctr1,       &
!!     &          coef_p, k_ratio, dk_dr, mat3)
!!        integer(kind = kint), intent(in) :: jmax, nri
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: r_CTR1(0:2)
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind = kreal), intent(in) :: k_ratio, dk_dr
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!!        real(kind = kreal), intent(inout) :: mat3(3,nri,jmax)
!!
!!      subroutine set_unit_mat3_filter_to_center(nri, jmax, ICB_Vspec, &
!!     &                                          mat3)
!!      subroutine set_unit_mat7_filter_to_center(nri, jmax, ICB_Vspec, &
!!     &                                          mat7)
!!      subroutine set_unit_mat9_filter_to_center(nri, jmax, ICB_Vspec, &
!!     &                                          mat9)
!!        integer(kind = kint), intent(in) :: jmax, nri
!!        real(kind = kreal), intent(in) :: ICB_Vspec(jmax)
!!        real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!!@endverbatim
!
!!@n @param jmax         Number of local spherical harmonics mode
!!@n @param fdm2_fix_fld_ctr1(-1:1,3)
!!         Matrix to evaluate radial derivative
!!         for center with fixed field
!
!
      module center_sph_matrices
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_vector_poisson_mat_center(nri, jmax, g_sph_rj,     &
     &          r_CTR1, fdm2_fix_fld_ctr1, coef_p, mat3)
!
      integer(kind = kint), intent(in) :: jmax, nri
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!
      real(kind = kreal), intent(inout) :: mat3(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        mat3(2,1,j) = mat3(2,1,j) - coef_p * (fdm2_fix_fld_ctr1(0,3)    &
     &                      - g_sph_rj(j,3)*r_CTR1(2) )
        mat3(1,2,j) = mat3(1,2,j) - coef_p *  fdm2_fix_fld_ctr1(1,3)
      end do
!
      end subroutine add_vector_poisson_mat_center
!
! -----------------------------------------------------------------------
!
      subroutine add_scalar_poisson_mat_ctr1(nri, jmax, g_sph_rj,       &
     &          r_CTR1, fdm2_fix_fld_ctr1, coef_p, mat3)
!
      integer(kind = kint), intent(in) :: jmax, nri
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!
      real(kind = kreal), intent(inout) :: mat3(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        mat3(2,1,j) = mat3(2,1,j) - coef_p * (fdm2_fix_fld_ctr1(0,3)    &
     &                      + two*r_CTR1(1) * fdm2_fix_fld_ctr1(0,2)    &
     &                      - g_sph_rj(j,3)*r_CTR1(2) )
        mat3(1,2,j) = mat3(1,2,j) - coef_p * (fdm2_fix_fld_ctr1(1,3)    &
     &                      + two*r_CTR1(1) * fdm2_fix_fld_ctr1(1,2))
      end do
!
      end subroutine add_scalar_poisson_mat_ctr1
!
! -----------------------------------------------------------------------
!
      subroutine add_scl_val_diffuse_mat_ctr1                           &
     &         (nri, jmax, g_sph_rj, r_CTR1, fdm2_fix_fld_ctr1,         &
     &          coef_p, k_ratio, dk_dr, mat3)
!
      integer(kind = kint), intent(in) :: jmax, nri
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: k_ratio, dk_dr
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!
      real(kind = kreal), intent(inout) :: mat3(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        mat3(2,1,j) = mat3(2,1,j)                                       &
     &               - coef_p * k_ratio * (fdm2_fix_fld_ctr1(0,3)       &
     &                   + two*r_CTR1(1) * fdm2_fix_fld_ctr1(0,2)       &
     &                   - g_sph_rj(j,3)*r_CTR1(2))                     &
     &                  - coef_p * dk_dr * fdm2_fix_fld_ctr1(0,2)
        mat3(1,2,j) = mat3(1,2,j)                                       &
     &               - coef_p * k_ratio * (fdm2_fix_fld_ctr1(1,3)       &
     &                   + two*r_CTR1(1) * fdm2_fix_fld_ctr1(1,2))      &
     &                  - coef_p * dk_dr * fdm2_fix_fld_ctr1(1,2)
      end do
!
      end subroutine add_scl_val_diffuse_mat_ctr1
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_unit_mat3_filter_to_center(nri, jmax, ICB_Vspec,   &
     &                                          mat3)
!
      integer(kind = kint), intent(in) :: jmax, nri
      real(kind = kreal), intent(in) :: ICB_Vspec(jmax)
!
      real(kind = kreal), intent(inout) :: mat3(3,nri,jmax)
!
      integer(kind = kint) :: k, j
!
!
!$omp parallel do private(k,j)
      do j = 1, jmax
        if(ICB_Vspec(j) .gt. one) then
          mat3(2,1,j) = one
          mat3(1,2,j) = zero
        end if
        do k = 2, int(ICB_Vspec(j))
          mat3(3,k-1,j) = zero
          mat3(2,k,  j) = one
          mat3(1,k+1,j) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine set_unit_mat3_filter_to_center
!
! -----------------------------------------------------------------------
!
      subroutine set_unit_mat7_filter_to_center(nri, jmax, ICB_Vspec,   &
     &                                          mat7)
!
      integer(kind = kint), intent(in) :: jmax, nri
      real(kind = kreal), intent(in) :: ICB_Vspec(jmax)
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: k, j
!
!
!$omp parallel do private(k,j)
      do j = 1, jmax
        if(ICB_Vspec(j) .gt. one) then
          mat7(5,1,j) = zero
          mat7(4,2,j) = one
          mat7(3,3,j) = zero
          mat7(2,4,j) = zero
          if(nri .gt. 2) mat7(1,5,j) = zero
        end if
        do k = 2, int(ICB_Vspec(j))
          mat7(7,2*k-3,j) = zero
          mat7(6,2*k-2,j) = zero
          mat7(5,2*k-1,j) = zero
          mat7(4,2*k,  j) = one
          mat7(3,2*k+1,j) = zero
          mat7(2,2*k+2,j) = zero
          mat7(1,2*k+3,j) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine set_unit_mat7_filter_to_center
!
! -----------------------------------------------------------------------
!
      subroutine set_unit_mat9_filter_to_center(nri, jmax, ICB_Vspec,   &
     &                                          mat9)
!
      integer(kind = kint), intent(in) :: jmax, nri
      real(kind = kreal), intent(in) :: ICB_Vspec(jmax)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: k, j
!
!
!$omp parallel do private(k,j)
      do j = 1, jmax
        if(ICB_Vspec(j) .gt. one) then
          mat9(6,1,j) = zero
          mat9(5,2,j) = one
          mat9(4,3,j) = zero
          mat9(3,4,j) = zero
          if(nri .gt. 2) mat9(2,5,j) = zero
          if(nri .gt. 2) mat9(1,6,j) = zero
        end if
        if(ICB_Vspec(j) .gt. two) then
          mat9(8,1,j) = zero
          mat9(7,2,j) = zero
          mat9(6,3,j) = zero
          mat9(5,4,j) = one
          if(nri .gt. 2) mat9(4,5,j) = zero
          if(nri .gt. 2) mat9(3,6,j) = zero
          if(nri .gt. 3) mat9(2,7,j) = zero
          if(nri .gt. 3) mat9(1,8,j) = zero
        end if
        do k = 3, int(ICB_Vspec(j))
          mat9(9,2*k-4,j) = zero
          mat9(8,2*k-3,j) = zero
          mat9(7,2*k-2,j) = zero
          mat9(6,2*k-1,j) = zero
          mat9(5,2*k,  j) = one
          mat9(4,2*k+1,j) = zero
          mat9(3,2*k+2,j) = zero
          mat9(2,2*k+3,j) = zero
          mat9(1,2*k+4,j) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine set_unit_mat9_filter_to_center
!
! -----------------------------------------------------------------------
!
      end module center_sph_matrices
