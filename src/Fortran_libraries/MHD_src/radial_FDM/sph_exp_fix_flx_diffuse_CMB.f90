!>@file   sph_exp_fix_flx_diffuse_CMB.f90
!!@brief  module sph_exp_fix_flx_diffuse_CMB
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2010
!
!>@brief  Evaluate scalar fields using fixed flux condition
!!
!!@verbatim
!!      subroutine sph_out_fix_flux_scl_diffuse2(nnod_rj, jmax,         &
!!     &          g_sph_rj, kr_out, r_CMB, fdm2_fix_dr_CMB, flux_OUT,   &
!!     &          coef_d, scl_rj, dfs_rj)
!!      subroutine sph_out_fix_flux_val_diffuse2(nnod_rj, jmax,         &
!!     &          g_sph_rj, kr_out, r_CMB, fdm2_fix_dr_CMB, flux_OUT,   &
!!     &          coef_d, k_ratio, dk_dr, scl_rj, dfs_rj)
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        integer(kind = kint), intent(in) :: kr_out
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!!        real(kind = kreal), intent(in) :: r_CMB(0:2)
!!        real(kind = kreal), intent(in) :: flux_OUT(jmax)
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in) :: k_ratio, dk_dr
!!        real(kind = kreal), intent(in) :: scl_rj(nnod_rj)
!!        real(kind = kreal), intent(inout) :: dfs_rj(nnod_rj)
!!
!!      subroutine adjust_sph_out_fix_flux(nnod_rj, jmax,               &
!!     &          kr_out, r_CMB, fdm2_fix_dr_CMB, flux_CMB,             &
!!     &          coef_d, coef_imp, dt, scl_rj)
!!      subroutine adjust_sph_out_fix_flx_v_diff(nnod_rj, jmax,         &
!!     &          kr_out, r_CMB, fdm2_fix_dr_CMB, flux_CMB,             &
!!     &          coef_d, k_ratio, dk_dr, coef_imp, dt, is_fld, scl_rj)
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        integer(kind = kint), intent(in) :: kr_out
!!        integer(kind = kint), intent(in) :: is_fld
!!        real(kind = kreal), intent(in) :: coef_imp, dt
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in) :: k_ratio, dk_dr
!!        real(kind = kreal), intent(in) :: flux_CMB(jmax)
!!        real(kind = kreal), intent(in) :: r_CMB(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!!        real(kind = kreal), intent(inout) :: scl_rj(nnod_rj)
!!@endverbatim
!!
!!@n @param idx_rj_degree_zero    Local address for degree 0
!!@n @param n_point  Number of points for spectrum data
!!@n @param jmax  Number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param g_sph_rj(jmax,13)   Normalization coefficients
!!@n @param j0    Local harmonics mode address for l = m = 0
!!@n @param kr_out       Radial ID for outer boundary
!!@n @param r_CMB(0:2)   Radius at CMB
!!@n @param flux_CMB(jamx)  Spectrum of fixed flux at CMB
!!@n @param fdm2_fix_dr_CMB(-1:1,3)
!!         Matrix to evaluate field at CMB with fixed radial derivative
!!
!!@n @param coef_d        Coefficient for diffusion term
!!
!!@n @param scl_rj         Scalar spherical harmonic coefficients
!!@n @param dfs_rj         Diffusion term spherical harmonic coefficients
!
      module sph_exp_fix_flx_diffuse_CMB
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
      subroutine sph_out_fix_flux_scl_diffuse2(nnod_rj, jmax,           &
     &          g_sph_rj, kr_out, r_CMB, fdm2_fix_dr_CMB, flux_OUT,     &
     &          coef_d, scl_rj, dfs_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: flux_OUT(jmax)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: scl_rj(nnod_rj)
!
      real(kind = kreal), intent(inout) :: dfs_rj(nnod_rj)
!
      real(kind = kreal) :: d2s_dr2
      integer(kind = kint) :: inod, i_n1, j
!
!
!$omp parallel do private(inod,i_n1,d2s_dr2)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
!
        d2s_dr2 =  fdm2_fix_dr_CMB(-1,3) * scl_rj(i_n1)                 &
     &           + fdm2_fix_dr_CMB( 0,3) * scl_rj(inod)                 &
     &           + fdm2_fix_dr_CMB( 1,3) * flux_OUT(j)
!
        dfs_rj(inod) = coef_d * (d2s_dr2 + two*r_CMB(1) * flux_OUT(j)   &
     &                         - g_sph_rj(j,3)*r_CMB(2) * scl_rj(inod))
      end do
!$omp end parallel do
!
      end subroutine sph_out_fix_flux_scl_diffuse2
!
! -----------------------------------------------------------------------
!
      subroutine sph_out_fix_flux_val_diffuse2(nnod_rj, jmax,           &
     &          g_sph_rj, kr_out, r_CMB, fdm2_fix_dr_CMB, flux_OUT,     &
     &          coef_d, k_ratio, dk_dr, scl_rj, dfs_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: flux_OUT(jmax)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: k_ratio, dk_dr
      real(kind = kreal), intent(in) :: scl_rj(nnod_rj)
!
      real(kind = kreal), intent(inout) :: dfs_rj(nnod_rj)
!
      real(kind = kreal) :: d2s_dr2
      integer(kind = kint) :: inod, i_n1, j
!
!
!$omp parallel do private(inod,i_n1,d2s_dr2)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
!
        d2s_dr2 =  fdm2_fix_dr_CMB(-1,3) * scl_rj(i_n1)                 &
     &           + fdm2_fix_dr_CMB( 0,3) * scl_rj(inod)                 &
     &           + fdm2_fix_dr_CMB( 1,3) * flux_OUT(j)
!
        dfs_rj(inod) = coef_d * k_ratio                                 &
     &                   * (d2s_dr2 + two*r_CMB(1) * flux_OUT(j)        &
     &                    - g_sph_rj(j,3)*r_CMB(2) * scl_rj(inod))      &
     &                   + coef_d * dk_dr * flux_OUT(j)
      end do
!$omp end parallel do
!
      end subroutine sph_out_fix_flux_val_diffuse2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine adjust_sph_out_fix_flux(nnod_rj, jmax,                 &
     &          kr_out, r_CMB, fdm2_fix_dr_CMB, flux_CMB,               &
     &          coef_d, coef_imp, dt, scl_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: coef_imp, dt
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: flux_CMB(jmax)
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      real(kind = kreal), intent(inout) :: scl_rj(nnod_rj)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
!
        scl_rj(inod) = scl_rj(inod)                                     &
     &                + dt * coef_imp * coef_d * flux_CMB(j)            &
     &                 * (fdm2_fix_dr_CMB( 1,3) + two*r_CMB(1))
      end do
!$omp end parallel do
!
      end subroutine adjust_sph_out_fix_flux
!
! -----------------------------------------------------------------------
!
      subroutine adjust_sph_out_fix_flx_v_diff(nnod_rj, jmax,           &
     &          kr_out, r_CMB, fdm2_fix_dr_CMB, flux_CMB,               &
     &          coef_d, k_ratio, dk_dr, coef_imp, dt, scl_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: coef_imp, dt
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: k_ratio, dk_dr
      real(kind = kreal), intent(in) :: flux_CMB(jmax)
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      real(kind = kreal), intent(inout) :: scl_rj(nnod_rj)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
!
        scl_rj(inod) = scl_rj(inod) + dt * coef_imp * coef_d            &
     &                      * (k_ratio * (fdm2_fix_dr_CMB( 1,3)         &
     &                       + two*r_CMB(1)) + dk_dr) * flux_CMB(j)
      end do
!$omp end parallel do
!
      end subroutine adjust_sph_out_fix_flx_v_diff
!
! -----------------------------------------------------------------------
!
      end module sph_exp_fix_flx_diffuse_CMB
