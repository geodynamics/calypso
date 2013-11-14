!>@file   cal_sph_exp_fixed_flux.f90
!!@brief  module cal_sph_exp_fixed_flux
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2010
!
!>@brief  Evaluate scalar fields using fixed flux condition
!!
!!@verbatim
!!      subroutine cal_dsdr_sph_in_fix_flux_2(idx_rj_degree_zero,       &
!!     &          jmax, kr_in, r_ICB, flux_ICB, is_fld, is_grd)
!!      subroutine cal_div_sph_in_fix_flux_2(jmax, kr_in, r_ICB,        &
!!     &          flux_ICB, is_fld, is_div)
!!      subroutine cal_sph_in_fix_flux_diffuse2(jmax, kr_in, r_ICB,     &
!!     &          fdm2_fix_dr_ICB, flux_IN, coef_d, is_fld, is_diffuse)
!!
!!      subroutine cal_dsdr_sph_out_fix_flux_2(idx_rj_degree_zero,      &
!!     &          jmax, kr_out, r_CMB, flux_CMB, is_fld, is_grd)
!!      subroutine cal_div_sph_out_fix_flux_2(jmax, kr_out, r_CMB,      &
!!     &          flux_CMB, is_fld, is_div)
!!      subroutine cal_sph_out_fix_flux_diffuse2(jmax, kr_out, r_CMB,   &
!!     &          fdm2_fix_dr_CMB, flux_OUT, coef_d, is_fld, is_diffuse)
!!@endverbatim
!!
!!@n @param jmax  Number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param flux_ICB(jamx)  Spectrum of fixed flux at ICB
!!@n @param flux_CMB(jamx)  Spectrum of fixed flux at CMB
!!@n @param coef_d        Coefficient for diffusion term
!!
!!@n @param is_fld      Address of spectrum data d_rj
!!                      (poloidal component for vector)
!!@n @param is_grd      Address of gradient of spectrum data d_rj
!!                      (poloidal component)
!!@n @param is_div      Address of divergence of spectrum data d_rj
!!@n @param is_diffuse  Address of divergence of spectrum data d_rj
!!                      (poloidal component for vector)
!
      module cal_sph_exp_fixed_flux
!
      use m_precision
!
      use m_constants
      use m_schmidt_poly_on_rtm
      use m_sph_spectr_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_dsdr_sph_in_fix_flux_2(idx_rj_degree_zero,         &
     &          jmax, kr_in, r_ICB, flux_ICB, is_fld, is_grd)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld, is_grd
      real(kind = kreal), intent(in) :: flux_ICB(jmax)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        d_rj(inod,is_grd  ) = flux_ICB(j) * g_sph_rj(j,13)*r_ICB(0)**2
        d_rj(inod,is_grd+1) = d_rj(inod,is_fld)
        d_rj(inod,is_grd+2) = zero
      end do
!$omp end parallel do
!
      if(idx_rj_degree_zero .eq. 0) return
      inod = idx_rj_degree_zero + (kr_in-1) * jmax
      d_rj(inod,is_grd  ) = flux_ICB(idx_rj_degree_zero)*r_ICB(0)**2
!
      end subroutine cal_dsdr_sph_in_fix_flux_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_div_sph_in_fix_flux_2(jmax, kr_in, r_ICB,          &
     &          flux_ICB, is_fld, is_div)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld, is_div
      real(kind = kreal), intent(in) :: flux_ICB(jmax)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        d_rj(inod,is_div) =  (flux_ICB(j) - d_rj(inod,is_fld+1) )       &
     &                     * max(g_sph_rj(j,3),half) * r_ICB(2)
      end do
!$omp end parallel do
!
      end subroutine cal_div_sph_in_fix_flux_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_in_fix_flux_diffuse2(jmax, kr_in, r_ICB,       &
     &          fdm2_fix_dr_ICB, flux_IN, coef_d, is_fld, is_diffuse)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: flux_IN(jmax)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
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
        d2t_dr2 =  fdm2_fix_dr_ICB(-1,3) * flux_IN(j)                   &
     &           + fdm2_fix_dr_ICB( 0,3) * d_rj(inod,is_fld)            &
     &           + fdm2_fix_dr_ICB( 1,3) * d_rj(i_p1,is_fld)
!
        d_rj(inod,is_diffuse) = coef_d * (d2t_dr2                       &
     &                    + two*r_ICB(1) * flux_IN(j)                   &
     &                    - g_sph_rj(j,3)*r_ICB(2) * d_rj(inod,is_fld))
!
      end do
!$omp end parallel do
!
      end subroutine cal_sph_in_fix_flux_diffuse2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_dsdr_sph_out_fix_flux_2(idx_rj_degree_zero,        &
     &          jmax, kr_out, r_CMB, flux_CMB, is_fld, is_grd)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_grd
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: flux_CMB(jmax)
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
      if(idx_rj_degree_zero .eq. 0) return
      inod = idx_rj_degree_zero + (kr_out-1) * jmax
      d_rj(inod,is_grd  ) = flux_CMB(idx_rj_degree_zero) * r_CMB(0)**2
!
      end subroutine cal_dsdr_sph_out_fix_flux_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_div_sph_out_fix_flux_2(jmax, kr_out, r_CMB,        &
     &          flux_CMB, is_fld, is_div)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_div
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: flux_CMB(jmax)
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
!
      subroutine cal_sph_out_fix_flux_diffuse2(jmax, kr_out, r_CMB,     &
     &          fdm2_fix_dr_CMB, flux_OUT, coef_d, is_fld, is_diffuse)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: flux_OUT(jmax)
      real(kind = kreal), intent(in) :: coef_d
!
      real(kind = kreal) :: d2t_dr2
      integer(kind = kint) :: inod, i_n1, j
!
!
!$omp parallel do private(inod,i_n1,d2t_dr2)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
!
        d2t_dr2 =  fdm2_fix_dr_CMB(-1,3) * d_rj(i_n1,is_fld)            &
     &           + fdm2_fix_dr_CMB( 0,3) * d_rj(inod,is_fld)            &
     &           + fdm2_fix_dr_CMB( 1,3) * flux_OUT(j)
!
        d_rj(inod,is_diffuse) = coef_d * (d2t_dr2                       &
     &                    + two*r_CMB(1) * flux_OUT(j)                  &
     &                    - g_sph_rj(j,3)*r_CMB(2) * d_rj(inod,is_fld))
!
      end do
!$omp end parallel do
!
      end subroutine cal_sph_out_fix_flux_diffuse2
!
! -----------------------------------------------------------------------
!
      end module cal_sph_exp_fixed_flux
