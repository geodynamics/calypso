!>@file   sph_exp_fixed_flux_CMB.f90
!!@brief  module sph_exp_fixed_flux_CMB
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2010
!
!>@brief  Evaluate scalar fields using fixed flux condition
!!
!!@verbatim
!!      subroutine dsdr_sph_out_fix_flux_2(jmax, g_sph_rj,              &
!!     &          kr_out, r_CMB, flux_CMB, is_fld, is_grd,              &
!!     &          n_point, ntot_phys_rj, d_rj)
!!      subroutine dsdr_sph_lm0_out_fix_flux_2(idx_rj_degree_zero,      &
!!     &          jmax, kr_out, r_CMB, flux_CMB, is_grd,                &
!!     &          n_point, ntot_phys_rj, d_rj)
!!      subroutine cal_div_sph_out_fix_flux_2(jmax, g_sph_rj,           &
!!     &          kr_out, r_CMB, flux_CMB, is_fld, is_div,              &
!!     &          n_point, ntot_phys_rj, d_rj)
!!
!!      subroutine poisson_out_fixed_flux_sph                           &
!!     &         (jmax, kr_out, r_CMB, fdm2_fix_dr_CMB, flux_CMB,       &
!!     &          is_fld, n_point, ntot_phys_rj, d_rj)
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
      module sph_exp_fixed_flux_CMB
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
      subroutine dsdr_sph_out_fix_flux_2(jmax, g_sph_rj,                &
     &          kr_out, r_CMB, flux_CMB, is_fld, is_grd,                &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_grd
      integer (kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: flux_CMB(jmax)
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        d_rj(inod,is_grd  ) = flux_CMB(j)*g_sph_rj(j,13) * r_CMB(0)**2
        d_rj(inod,is_grd+1) = d_rj(inod,is_fld  )
        d_rj(inod,is_grd+2) = zero
      end do
!$omp end parallel do
!
      end subroutine dsdr_sph_out_fix_flux_2
!
! -----------------------------------------------------------------------
!
      subroutine dsdr_sph_lm0_out_fix_flux_2(idx_rj_degree_zero,        &
     &          jmax, kr_out, r_CMB, flux_CMB, is_grd,                  &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_grd
      integer (kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: flux_CMB(jmax)
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
      if(idx_rj_degree_zero .eq. 0) return
      inod = idx_rj_degree_zero + (kr_out-1) * jmax
      d_rj(inod,is_grd  ) = flux_CMB(idx_rj_degree_zero) * r_CMB(0)**2
      d_rj(inod,is_grd+1) = zero
      d_rj(inod,is_grd+2) = zero
!
      end subroutine dsdr_sph_lm0_out_fix_flux_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_div_sph_out_fix_flux_2(jmax, g_sph_rj,             &
     &          kr_out, r_CMB, flux_CMB, is_fld, is_div,                &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_div
      integer (kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: flux_CMB(jmax)
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        d_rj(inod,is_div) =  (flux_CMB(j) - d_rj(inod,is_fld+1) )       &
     &                      * max(g_sph_rj(j,3),half) * r_CMB(2)
      end do
!$omp end parallel do
!
      end subroutine cal_div_sph_out_fix_flux_2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine poisson_out_fixed_flux_sph                             &
     &         (jmax, kr_out, r_CMB, fdm2_fix_dr_CMB, flux_CMB,         &
     &          is_fld, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld
      real(kind = kreal), intent(in) :: flux_CMB(jmax)
      real(kind = kreal), intent(in) :: r_CMB(0:1)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
!
        d_rj(inod,is_fld) = (fdm2_fix_dr_CMB( 1,3) + two*r_CMB(1))      &
     &                     * flux_CMB(j)
      end do
!$omp end parallel do
!
      end subroutine poisson_out_fixed_flux_sph
!
! -----------------------------------------------------------------------
!
      end module sph_exp_fixed_flux_CMB
