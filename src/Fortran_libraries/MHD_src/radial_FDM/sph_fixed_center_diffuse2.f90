!>@file   sph_fixed_center_diffuse2.f90
!!@brief  module sph_fixed_center_diffuse2
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Set diffusion at center for explicit method
!!
!!@verbatim
!!      subroutine sph_fixed_ctr_diffuse_ctr1                           &
!!     &         (inod_rj_center, idx_rj_degree_zero, nnod_rj, jmax,    &
!!     &          g_sph_rj, r_CTR1, fdm2_fix_fld_ctr1, fix_CTR, coef_d, &
!!     &          scl_rj, dfs_rj)
!!      subroutine sph_fixed_ctr_val_diffuse_ctr1                       &
!!     &         (inod_rj_center, idx_rj_degree_zero, nnod_rj, jmax,    &
!!     &          g_sph_rj, r_CTR1, fdm2_fix_fld_ctr1, fix_CTR,         &
!!     &          coef_d, k_ratio, dk_dr, scl_rj, dfs_rj)
!!        integer(kind = kint), intent(in) :: inod_rj_center
!!        integer(kind = kint), intent(in) :: idx_rj_degree_zero
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: fix_CTR(jmax)
!!        real(kind = kreal), intent(in) :: r_CTR1(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in) :: k_ratio, dk_dr
!!        real(kind = kreal), intent(in) :: scl_rj(nnod_rj)
!!        real(kind = kreal), intent(inout) :: dfs_rj(nnod_rj)
!!@endverbatim
!!
!!@n @param inod_rj_center        Local address for center
!!@n @param idx_rj_degree_zero    Local address for degree 0
!!@n @param jmax         Number of local spherical harmonics mode
!!@n @param r_CTR1(0:2)   Radius at innermost point
!!@n @param fdm2_fix_fld_ctr1(-1:1,3)
!!         Matrix to evaluate radial derivative
!!         for center with fixed field
!!@n @param fdm2_fixed_center(0:2,3)
!!         Matrix to evaluate radial derivative
!!         for center with fixed field
!!@n @param fix_CTR(jmax) Spectr data for fixed scalar at center
!!@n @param coef_d        Coefficient for diffusion term
!!
      module sph_fixed_center_diffuse2
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
      subroutine sph_fixed_ctr_diffuse_ctr1                             &
     &         (inod_rj_center, idx_rj_degree_zero, nnod_rj, jmax,      &
     &          g_sph_rj, r_CTR1, fdm2_fix_fld_ctr1, fix_CTR, coef_d,   &
     &          scl_rj, dfs_rj)
!
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: fix_CTR(jmax)
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: scl_rj(nnod_rj)
!
      real(kind = kreal), intent(inout) :: dfs_rj(nnod_rj)
!
      real(kind = kreal) :: d1s_dr1, d2s_dr2
      integer(kind = kint) :: i_p1, j
!
!
!$omp parallel do private(i_p1,d1s_dr1,d2s_dr2)
      do j = 1, jmax
        i_p1 = j + jmax
!
        d1s_dr1 =  fdm2_fix_fld_ctr1(-1,2) * fix_CTR(j)                 &
     &           + fdm2_fix_fld_ctr1( 0,2) * scl_rj(j   )               &
     &           + fdm2_fix_fld_ctr1( 1,2) * scl_rj(i_p1)
        d2s_dr2 =  fdm2_fix_fld_ctr1(-1,3) * fix_CTR(j)                 &
     &           + fdm2_fix_fld_ctr1( 0,3) * scl_rj(j   )               &
     &           + fdm2_fix_fld_ctr1( 1,3) * scl_rj(i_p1)
!
        dfs_rj(j) = coef_d * (d2s_dr2 + two*r_CTR1(1)*d1s_dr1           &
     &                      - g_sph_rj(j,3)*r_CTR1(2) * scl_rj(j))
      end do
!$omp end parallel do
!
      if(inod_rj_center .gt. 0)  then
        dfs_rj(inod_rj_center) = dfs_rj(idx_rj_degree_zero)
      end if
!
      end subroutine sph_fixed_ctr_diffuse_ctr1
!
! -----------------------------------------------------------------------
!
      subroutine sph_fixed_ctr_val_diffuse_ctr1                         &
     &         (inod_rj_center, idx_rj_degree_zero, nnod_rj, jmax,      &
     &          g_sph_rj, r_CTR1, fdm2_fix_fld_ctr1, fix_CTR,           &
     &          coef_d, k_ratio, dk_dr, scl_rj, dfs_rj)
!
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: fix_CTR(jmax)
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: k_ratio, dk_dr
      real(kind = kreal), intent(in) :: scl_rj(nnod_rj)
!
      real(kind = kreal), intent(inout) :: dfs_rj(nnod_rj)
!
      real(kind = kreal) :: d1s_dr1, d2s_dr2
      integer(kind = kint) :: i_p1, j
!
!
!$omp parallel do private(i_p1,d1s_dr1,d2s_dr2)
      do j = 1, jmax
        i_p1 = j + jmax
!
        d1s_dr1 =  fdm2_fix_fld_ctr1(-1,2) * fix_CTR(j)                 &
     &           + fdm2_fix_fld_ctr1( 0,2) * scl_rj(j   )               &
     &           + fdm2_fix_fld_ctr1( 1,2) * scl_rj(i_p1)
        d2s_dr2 =  fdm2_fix_fld_ctr1(-1,3) * fix_CTR(j)                 &
     &           + fdm2_fix_fld_ctr1( 0,3) * scl_rj(j   )               &
     &           + fdm2_fix_fld_ctr1( 1,3) * scl_rj(i_p1)
!
        dfs_rj(j) = coef_d * k_ratio * (d2s_dr2 + two*r_CTR1(1)*d1s_dr1 &
     &                           - g_sph_rj(j,3)*r_CTR1(2) * scl_rj(j)) &
     &             + coef_d * dk_dr * d1s_dr1
      end do
!$omp end parallel do
!
      if(inod_rj_center .gt. 0)  then
        dfs_rj(inod_rj_center) = dfs_rj(idx_rj_degree_zero)
      end if
!
      end subroutine sph_fixed_ctr_val_diffuse_ctr1
!
! -----------------------------------------------------------------------
!
      end module sph_fixed_center_diffuse2
