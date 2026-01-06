!>@file   sph_exp_fixed_flux_ICB.f90
!!@brief  module sph_exp_fixed_flux_ICB
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2010
!
!>@brief  Evaluate scalar fields using fixed flux condition
!!
!!@verbatim
!!      subroutine dsdr_sph_in_fix_flux_2                               &
!!     &         (jmax, g_sph_rj, kr_in, r_ICB, flux_ICB,               &
!!     &          n_point, d_rj_fld, d_rj_grad)
!!        integer(kind = kint), intent(in) :: jmax, kr_in
!!        integer (kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: flux_ICB(jmax)
!!        real(kind = kreal), intent(in) :: r_ICB(0:2)
!!        real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_grad(n_point,3)
!!      subroutine dsdr_sph_lm0_in_fix_flux_2(idx_rj_degree_zero,       &
!!     &          jmax, kr_in, r_ICB, flux_ICB, n_point, d_rj_grad)
!!        integer(kind = kint), intent(in) :: idx_rj_degree_zero
!!        integer(kind = kint), intent(in) :: jmax, kr_in
!!        integer (kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: flux_ICB(jmax)
!!        real(kind = kreal), intent(in) :: r_ICB(0:2)
!!        real (kind=kreal), intent(inout) :: d_rj_grad(n_point,3)
!!      subroutine cal_div_sph_in_fix_flux_2(jmax, g_sph_rj, kr_in,     &
!!     &          r_ICB, flux_ICB, n_point, d_rj_fld, d_rj_div)
!!        integer(kind = kint), intent(in) :: jmax, kr_in
!!        integer (kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: flux_ICB(jmax)
!!        real(kind = kreal), intent(in) :: r_ICB(0:2)
!!        real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_div(n_point)
!!
!!      subroutine poisson_in_fixed_flux_sph(jmax, kr_in, r_ICB,        &
!!     &          fdm2_fix_dr_ICB, flux_ICB, n_point, d_rj_fld)
!!        integer(kind = kint), intent(in) :: jmax, kr_in
!!        real(kind = kreal), intent(in) :: flux_ICB(jmax)
!!        real(kind = kreal), intent(in) :: r_ICB(0:1)
!!        real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: d_rj_fld(n_point)
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
!!@n @param d_rj           Spectrum data
!
      module sph_exp_fixed_flux_ICB
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
      subroutine dsdr_sph_in_fix_flux_2                                 &
     &         (jmax, g_sph_rj, kr_in, r_ICB, flux_ICB,                 &
     &          n_point, d_rj_fld, d_rj_grad)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer (kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: flux_ICB(jmax)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!
      real(kind = kreal), intent(inout) :: d_rj_grad(n_point,3)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        d_rj_grad(inod,1) = flux_ICB(j) * g_sph_rj(j,13)*r_ICB(0)**2
        d_rj_grad(inod,2) = d_rj_fld(inod,1)
        d_rj_grad(inod,3) = zero
      end do
!$omp end parallel do
!
      end subroutine dsdr_sph_in_fix_flux_2
!
! -----------------------------------------------------------------------
!
      subroutine dsdr_sph_lm0_in_fix_flux_2(idx_rj_degree_zero,         &
     &          jmax, kr_in, r_ICB, flux_ICB, n_point, d_rj_grad)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer (kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: flux_ICB(jmax)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
!
      real(kind = kreal), intent(inout) :: d_rj_grad(n_point,3)
!
      integer(kind = kint) :: inod
!
!
      if(idx_rj_degree_zero .eq. 0) return
      inod = idx_rj_degree_zero + (kr_in-1) * jmax
      d_rj_grad(inod,1) = flux_ICB(idx_rj_degree_zero)*r_ICB(0)**2
      d_rj_grad(inod,2) = zero
      d_rj_grad(inod,3) = zero
!
      end subroutine dsdr_sph_lm0_in_fix_flux_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_div_sph_in_fix_flux_2(jmax, g_sph_rj, kr_in,       &
     &          r_ICB, flux_ICB, n_point, d_rj_fld, d_rj_div)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer (kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: flux_ICB(jmax)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!
      real(kind = kreal), intent(inout) :: d_rj_div(n_point)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        d_rj_div(inod) =  (flux_ICB(j) - d_rj_fld(inod,2) )             &
     &                   * max(g_sph_rj(j,3),half) * r_ICB(2)
      end do
!$omp end parallel do
!
      end subroutine cal_div_sph_in_fix_flux_2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine poisson_in_fixed_flux_sph(jmax, kr_in, r_ICB,          &
     &          fdm2_fix_dr_ICB, flux_ICB, n_point, d_rj_fld)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      real(kind = kreal), intent(in) :: flux_ICB(jmax)
      real(kind = kreal), intent(in) :: r_ICB(0:1)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: d_rj_fld(n_point)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
!
        d_rj_fld(inod) = (fdm2_fix_dr_ICB(-1,3) + two*r_ICB(1))         &
     &                  * flux_ICB(j)
      end do
!$omp end parallel do
!
      end subroutine poisson_in_fixed_flux_sph
!
! -----------------------------------------------------------------------
!
      end module sph_exp_fixed_flux_ICB
