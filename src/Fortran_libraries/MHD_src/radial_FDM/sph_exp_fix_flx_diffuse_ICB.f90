!>@file   sph_exp_fix_flx_diffuse_ICB.f90
!!@brief  module sph_exp_fix_flx_diffuse_ICB
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2010
!
!>@brief  Evaluate scalar fields using fixed flux condition
!!
!!@verbatim
!!      subroutine sph_in_fix_flux_scl_diffuse2(nnod_rj, jmax, g_sph_rj,&
!!     &          kr_in, r_ICB, fdm2_fix_dr_ICB, flux_ICB,              &
!!     &          coef_d, scl_rj, dfs_rj)
!!      subroutine sph_in_fix_flux_val_diffuse2(nnod_rj, jmax, g_sph_rj,&
!!     &          kr_in, r_ICB, fdm2_fix_dr_ICB, flux_ICB,              &
!!     &          coef_d, k_ratio, dk_dr, scl_rj, dfs_rj)
!!        integer (kind = kint), intent(in) :: nnod_rj, jmax
!!        integer(kind = kint), intent(in) :: kr_in
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in) :: k_ratio, dk_dr
!!        real(kind = kreal), intent(in) :: flux_ICB(jmax)
!!        real(kind = kreal), intent(in) :: r_ICB(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!!        real(kind = kreal), intent(in) :: scl_rj(nnod_rj)
!!        real(kind = kreal), intent(inout) :: dfs_rj(nnod_rj)
!!
!!      subroutine adjust_sph_in_fix_flux(nnod_rj, jmax,                &
!!     &          kr_in, r_ICB, fdm2_fix_dr_ICB, flux_ICB,              &
!!     &          coef_d, coef_imp, dt, scl_rj)
!!      subroutine adjust_sph_in_fix_flx_v_diff(nnod_rj, jmax,          &
!!     &          kr_in, r_ICB, fdm2_fix_dr_ICB, flux_ICB,              &
!!     &          coef_d, k_ratio, dk_dr, coef_imp, dt, scl_rj)
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        integer(kind = kint), intent(in) :: kr_in
!!        real(kind = kreal), intent(in) :: coef_imp, dt
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in) :: k_ratio, dk_dr
!!        real(kind = kreal), intent(in) :: flux_ICB(jmax)
!!        real(kind = kreal), intent(in) :: r_ICB(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!!        real (kind=kreal), intent(inout) :: scl_rj(nnod_rj)
!!@endverbatim
!!
!!@n @param idx_rj_degree_zero    Local address for degree 0
!!@n @param n_point  Number of points for spectrum data
!!@n @param jmax  Number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param g_sph_rj(jmax,13)   Normalization coefficients
!!@n @param j0    Local harmonics mode address for l = m = 0
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param r_ICB(0:2)   Radius at ICB
!!@n @param flux_ICB(jamx)  Spectrum of fixed flux at ICB
!!@n @param fdm2_fix_dr_ICB(-1:1,3)
!!         Matrix to evaluate field at ICB with fixed radial derivative
!!
!!@n @param coef_d        Coefficient for diffusion term
!!
!!@n @param is_fld      Address of spectrum data d_rj
!!                      (poloidal component for vector)
!!@n @param is_grd      Address of gradient of spectrum data d_rj
!!                      (poloidal component)
!!@n @param is_div      Address of divergence of spectrum data d_rj
!!@n @param is_diffuse  Address of divergence of spectrum data d_rj
!!                      (poloidal component for vector)
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module sph_exp_fix_flx_diffuse_ICB
!
      use m_precision
!
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
      subroutine sph_in_fix_flux_scl_diffuse2(nnod_rj, jmax, g_sph_rj,  &
     &          kr_in, r_ICB, fdm2_fix_dr_ICB, flux_ICB,                &
     &          coef_d, scl_rj, dfs_rj)
!
      integer (kind = kint), intent(in) :: nnod_rj, jmax
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: flux_ICB(jmax)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
      real(kind = kreal), intent(in) :: scl_rj(nnod_rj)
!
      real(kind = kreal), intent(inout) :: dfs_rj(nnod_rj)
!
      real(kind = kreal) :: d2t_dr2
      integer(kind = kint) :: inod, i_p1, j
!
!
!$omp parallel do private(inod,i_p1,d2t_dr2)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
!
        d2t_dr2 =  fdm2_fix_dr_ICB(-1,3) * flux_ICB(j)                  &
     &           + fdm2_fix_dr_ICB( 0,3) * scl_rj(inod)                 &
     &           + fdm2_fix_dr_ICB( 1,3) * scl_rj(i_p1)
!
        dfs_rj(inod) = coef_d * (d2t_dr2 + two*r_ICB(1) * flux_ICB(j)   &
     &                         - g_sph_rj(j,3)*r_ICB(2) * scl_rj(inod))
!
      end do
!$omp end parallel do
!
      end subroutine sph_in_fix_flux_scl_diffuse2
!
! -----------------------------------------------------------------------
!
      subroutine sph_in_fix_flux_val_diffuse2(nnod_rj, jmax, g_sph_rj,  &
     &          kr_in, r_ICB, fdm2_fix_dr_ICB, flux_ICB,                &
     &          coef_d, k_ratio, dk_dr, scl_rj, dfs_rj)
!
      integer (kind = kint), intent(in) :: nnod_rj, jmax
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: k_ratio, dk_dr
      real(kind = kreal), intent(in) :: flux_ICB(jmax)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
      real(kind = kreal), intent(in) :: scl_rj(nnod_rj)
!
      real(kind = kreal), intent(inout) :: dfs_rj(nnod_rj)
!
      real(kind = kreal) :: d2t_dr2
      integer(kind = kint) :: inod, i_p1, j
!
!
!$omp parallel do private(inod,i_p1,d2t_dr2)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
!
        d2t_dr2 =  fdm2_fix_dr_ICB(-1,3) * flux_ICB(j)                  &
     &           + fdm2_fix_dr_ICB( 0,3) * scl_rj(inod)                 &
     &           + fdm2_fix_dr_ICB( 1,3) * scl_rj(i_p1)
!
        dfs_rj(inod) = coef_d * (d2t_dr2 + two*r_ICB(1) * flux_ICB(j)   &
     &                         - g_sph_rj(j,3)*r_ICB(2) * scl_rj(inod))
!
      end do
!$omp end parallel do
!
      end subroutine sph_in_fix_flux_val_diffuse2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine adjust_sph_in_fix_flux(nnod_rj, jmax,                  &
     &          kr_in, r_ICB, fdm2_fix_dr_ICB, flux_ICB,                &
     &          coef_d, coef_imp, dt, scl_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: coef_d, coef_imp, dt
      real(kind = kreal), intent(in) :: flux_ICB(jmax)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      real (kind=kreal), intent(inout) :: scl_rj(nnod_rj)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
!
        scl_rj(inod) = scl_rj(inod) + dt * coef_imp * coef_d            &
     &                         * (fdm2_fix_dr_ICB(-1,3) + two*r_ICB(1)) &
     &                         * flux_ICB(j) 
      end do
!$omp end parallel do
!
      end subroutine adjust_sph_in_fix_flux
!
! -----------------------------------------------------------------------
!
      subroutine adjust_sph_in_fix_flx_v_diff(nnod_rj, jmax,            &
     &          kr_in, r_ICB, fdm2_fix_dr_ICB, flux_ICB,                &
     &          coef_d, k_ratio, dk_dr, coef_imp, dt, scl_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: coef_imp, dt
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: k_ratio, dk_dr
      real(kind = kreal), intent(in) :: flux_ICB(jmax)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      real (kind=kreal), intent(inout) :: scl_rj(nnod_rj)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
!
        scl_rj(inod) = scl_rj(inod) + dt * coef_imp * coef_d            &
     &                      * (k_ratio * (fdm2_fix_dr_ICB(-1,3)         &
     &                       + two*r_ICB(1)) + dk_dr) * flux_ICB(j)
      end do
!$omp end parallel do
!
      end subroutine adjust_sph_in_fix_flx_v_diff
!
! -----------------------------------------------------------------------
!
      end module sph_exp_fix_flx_diffuse_ICB
