!>@file   sph_filled_center_diffuse2.f90
!!@brief  module sph_filled_center_diffuse2
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Set diffusion at center for explicit method
!!
!!@verbatim
!!      subroutine sph_filled_ctr_diffuse_ctr2                          &
!!     &       (inod_rj_center, idx_rj_degree_zero, nnod_rj, jmax,      &
!!     &        g_sph_rj, r_CTR1, fdm2_fix_fld_ctr1, fdm2_fix_dr_center,&
!!     &        coef_d, scl_rj, dfs_rj)
!!      subroutine sph_filled_ctr_val_diffuse_ctr2                      &
!!     &       (inod_rj_center, idx_rj_degree_zero, nnod_rj, jmax,      &
!!     &        g_sph_rj, r_CTR1, fdm2_fix_fld_ctr1, fdm2_fix_dr_center,&
!!     &        coef_d, k_ratio, dk_dr, scl_rj, dfs_rj)
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        integer(kind = kint), intent(in) :: inod_rj_center
!!        integer(kind = kint), intent(in) :: idx_rj_degree_zero
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: r_CTR1(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!!        real(kind = kreal), intent(in) :: fdm2_fix_dr_center(-1:1,3)
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in) :: k_ratio(0:1)
!!        real(kind = kreal), intent(in) :: dk_dr(0:1)
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
      module sph_filled_center_diffuse2
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
      subroutine sph_filled_ctr_diffuse_ctr2                            &
     &       (inod_rj_center, idx_rj_degree_zero, nnod_rj, jmax,        &
     &        g_sph_rj, r_CTR1, fdm2_fix_fld_ctr1, fdm2_fix_dr_center,  &
     &        coef_d, scl_rj, dfs_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_center(-1:1,3)
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
        d1s_dr1 =  fdm2_fix_fld_ctr1( 0,2) * scl_rj(j   )               &
     &           + fdm2_fix_fld_ctr1( 1,2) * scl_rj(i_p1)
        d2s_dr2 =  fdm2_fix_fld_ctr1( 0,3) * scl_rj(j   )               &
     &           + fdm2_fix_fld_ctr1( 1,3) * scl_rj(i_p1)
!
        dfs_rj(j) = coef_d * (d2s_dr2 + two*r_CTR1(1)*d1s_dr1           &
     &             - g_sph_rj(j,3)*r_CTR1(2) * scl_rj(j))
      end do
!$omp end parallel do
!
!
      if(inod_rj_center .eq. 0) return
!
      i_p1 = idx_rj_degree_zero + jmax
      d1s_dr1 =  fdm2_fix_fld_ctr1(-1,2) * scl_rj(inod_rj_center)       &
     &         + fdm2_fix_fld_ctr1( 0,2) * scl_rj(idx_rj_degree_zero)   &
     &         + fdm2_fix_fld_ctr1( 1,2) * scl_rj(i_p1)
      d2s_dr2 =  fdm2_fix_fld_ctr1(-1,3) * scl_rj(inod_rj_center)       &
     &         + fdm2_fix_fld_ctr1( 0,3) * scl_rj(idx_rj_degree_zero)   &
     &         + fdm2_fix_fld_ctr1( 1,3) * scl_rj(i_p1)
!
      dfs_rj(idx_rj_degree_zero)                                        &
     &        = coef_d * (d2s_dr2 + two*r_CTR1(1)*d1s_dr1)
!
      d2s_dr2 =  fdm2_fix_dr_center( 0,3) * scl_rj(inod_rj_center)      &
     &         + fdm2_fix_dr_center( 1,3) * scl_rj(idx_rj_degree_zero)
!
      dfs_rj(inod_rj_center) = coef_d * d2s_dr2
!
      end subroutine sph_filled_ctr_diffuse_ctr2
!
! -----------------------------------------------------------------------
!
      subroutine sph_filled_ctr_val_diffuse_ctr2                        &
     &       (inod_rj_center, idx_rj_degree_zero, nnod_rj, jmax,        &
     &        g_sph_rj, r_CTR1, fdm2_fix_fld_ctr1, fdm2_fix_dr_center,  &
     &        coef_d, k_ratio, dk_dr, scl_rj, dfs_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_center(-1:1,3)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: k_ratio(0:1)
      real(kind = kreal), intent(in) :: dk_dr(0:1)
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
        d1s_dr1 =  fdm2_fix_fld_ctr1( 0,2) * scl_rj(j   )               &
     &           + fdm2_fix_fld_ctr1( 1,2) * scl_rj(i_p1)
        d2s_dr2 =  fdm2_fix_fld_ctr1( 0,3) * scl_rj(j   )               &
     &           + fdm2_fix_fld_ctr1( 1,3) * scl_rj(i_p1)
!
        dfs_rj(j) = coef_d * k_ratio(1)                                 &
     &             * (d2s_dr2 + two*r_CTR1(1)*d1s_dr1                   &
     &                - g_sph_rj(j,3)*r_CTR1(2) * scl_rj(j))            &
     &             + coef_d * dk_dr(1) * d1s_dr1
      end do
!$omp end parallel do
!
!
      if(inod_rj_center .eq. 0) return
!
      i_p1 = idx_rj_degree_zero + jmax
!
      d1s_dr1 =  fdm2_fix_fld_ctr1(-1,2) * scl_rj(inod_rj_center)       &
     &         + fdm2_fix_fld_ctr1( 0,2) * scl_rj(idx_rj_degree_zero)   &
     &         + fdm2_fix_fld_ctr1( 1,2) * scl_rj(i_p1)
      d2s_dr2 =  fdm2_fix_fld_ctr1(-1,3) * scl_rj(inod_rj_center)       &
     &         + fdm2_fix_fld_ctr1( 0,3) * scl_rj(idx_rj_degree_zero)   &
     &         + fdm2_fix_fld_ctr1( 1,3) * scl_rj(i_p1)
!
      dfs_rj(idx_rj_degree_zero)                                        &
     &       = coef_d * (k_ratio(1) * (d2s_dr2 + two*r_CTR1(1)*d1s_dr1) &
     &                   + dk_dr(1) * d1s_dr1)
!
      d2s_dr2 =  fdm2_fix_dr_center( 0,3) * scl_rj(inod_rj_center)      &
     &         + fdm2_fix_dr_center( 1,3) * scl_rj(idx_rj_degree_zero)
!
      dfs_rj(inod_rj_center) = coef_d * k_ratio(0) * d2s_dr2
!
      end subroutine sph_filled_ctr_val_diffuse_ctr2
!
! -----------------------------------------------------------------------
!
      end module sph_filled_center_diffuse2
