!>@file   sph_exp_fix_scl_diffuse_ICB.f90
!!@brief  module sph_exp_fix_scl_diffuse_ICB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Set fixed scalar boundarry for explicit method
!!
!!@verbatim
!!      subroutine sph_in_fix_scalar_diffuse2(nnod_rj, jmax, g_sph_rj,  &
!!     &          kr_in, r_ICB, fdm2_fix_fld_ICB, fix_ICB, coef_d,      &
!!     &          scl_rj, dfs_rj)
!!      subroutine sph_in_fix_scl_val_diffuse2(nnod_rj, jmax, g_sph_rj, &
!!     &          kr_in, r_ICB, fdm2_fix_fld_ICB, fix_ICB,              &
!!     &          coef_d, k_ratio, dk_dr, scl_rj, dfs_rj)
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        integer(kind = kint), intent(in) :: kr_in
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: fix_ICB(jmax)
!!        real(kind = kreal), intent(in) :: r_ICB(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in) :: k_ratio, dk_dr
!!        real(kind = kreal), intent(inout) :: scl_rj(nnod_rj)
!!        real(kind = kreal), intent(inout) :: dfs_rj(nnod_rj)
!!@endverbatim
!!
!!@n @param idx_rj_degree_zero    Local address for degree 0
!!@n @param n_point  Number of points for spectrum data
!!@n @param jmax         Number of local spherical harmonics mode
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param r_ICB(0:2 )   Radius at ICB
!!@n @param fdm2_fix_fld_ICB(0:2,3)
!!         Matrix to evaluate radial derivative at ICB with fixed field
!!
!!@n @param fix_ICB(jmax) Spectr data for fixed scalar at ICB
!!@n @param coef_d        Coefficient for diffusion term
!!
!!@n @param is_fld       Field address of input field
!!@n @param is_diffuse   Field address for diffusion of field
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module sph_exp_fix_scl_diffuse_ICB
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
      subroutine sph_in_fix_scalar_diffuse2(nnod_rj, jmax, g_sph_rj,    &
     &          kr_in, r_ICB, fdm2_fix_fld_ICB, fix_ICB, coef_d,        &
     &          scl_rj, dfs_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: fix_ICB(jmax)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
      real(kind = kreal), intent(in) :: coef_d
!
      real(kind = kreal), intent(inout) :: scl_rj(nnod_rj)
      real(kind = kreal), intent(inout) :: dfs_rj(nnod_rj)
!
      real(kind = kreal) :: d1s_dr1, d2s_dr2
      integer(kind = kint) :: inod, i_p1, i_p2, j
!
!
!$omp parallel do private(inod,i_p1,i_p2,d1s_dr1,d2s_dr2)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d1s_dr1 =  fdm2_fix_fld_ICB( 0,2) * fix_ICB(j)                  &
     &           + fdm2_fix_fld_ICB( 1,2) * scl_rj(i_p1)                &
     &           + fdm2_fix_fld_ICB( 2,2) * scl_rj(i_p2)
        d2s_dr2 =  fdm2_fix_fld_ICB( 0,3) * fix_ICB(j)                  &
     &           + fdm2_fix_fld_ICB( 1,3) * scl_rj(i_p1)                &
     &           + fdm2_fix_fld_ICB( 2,3) * scl_rj(i_p2)
!
        scl_rj(inod) = fix_ICB(j)
        dfs_rj(inod) = coef_d * (d2s_dr2 + two*r_ICB(1)*d1s_dr1         &
     &                         - g_sph_rj(j,3)*r_ICB(2) * scl_rj(inod))
!
      end do
!$omp end parallel do
!
      end subroutine sph_in_fix_scalar_diffuse2
!
! -----------------------------------------------------------------------
!
      subroutine sph_in_fix_scl_val_diffuse2(nnod_rj, jmax, g_sph_rj,   &
     &          kr_in, r_ICB, fdm2_fix_fld_ICB, fix_ICB,                &
     &          coef_d, k_ratio, dk_dr, scl_rj, dfs_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: fix_ICB(jmax)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: k_ratio, dk_dr
!
      real(kind = kreal), intent(inout) :: scl_rj(nnod_rj)
      real(kind = kreal), intent(inout) :: dfs_rj(nnod_rj)
!
      real(kind = kreal) :: d1s_dr1, d2s_dr2
      integer(kind = kint) :: inod, i_p1, i_p2, j
!
!
!$omp parallel do private(inod,i_p1,i_p2,d1s_dr1,d2s_dr2)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d1s_dr1 =  fdm2_fix_fld_ICB( 0,2) * fix_ICB(j)                  &
     &           + fdm2_fix_fld_ICB( 1,2) * scl_rj(i_p1)                &
     &           + fdm2_fix_fld_ICB( 2,2) * scl_rj(i_p2)
        d2s_dr2 =  fdm2_fix_fld_ICB( 0,3) * fix_ICB(j)                  &
     &           + fdm2_fix_fld_ICB( 1,3) * scl_rj(i_p1)                &
     &           + fdm2_fix_fld_ICB( 2,3) * scl_rj(i_p2)
!
        scl_rj(inod) = fix_ICB(j)
        dfs_rj(inod) = coef_d*k_ratio * (d2s_dr2 + two*r_ICB(1)*d1s_dr1 &
     &                        - g_sph_rj(j,3)*r_ICB(2) * scl_rj(inod))  &
     &                        + coef_d * dk_dr * d1s_dr1
      end do
!$omp end parallel do
!
      end subroutine sph_in_fix_scl_val_diffuse2
!
! -----------------------------------------------------------------------
!
      end module sph_exp_fix_scl_diffuse_ICB
