!>@file   cal_sph_exp_fixed_flux.f90
!!@brief  module cal_sph_exp_fixed_flux
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2010
!
!>@brief  Evaluate scalar fields using fixed flux condition
!!
!!@verbatim
!!      subroutine cal_dsdr_sph_icb_fix_flux_2(jmax, flux_ICB,          &
!!     &          is_fld, is_grd)
!!      subroutine cal_div_sph_icb_fix_flux_2(jmax, flux_ICB,           &
!!     &          is_fld, is_div)
!!      subroutine cal_sph_icb_fix_flux_diffuse2(jmax, flux_ICB,        &
!!     &          is_fld, is_diffuse)
!!      subroutine cal_dsdr_sph_cmb_fix_flux_2(jmax, flux_CMB,          &
!!     &          is_fld, is_grd)
!!      subroutine cal_div_sph_cmb_fix_flux_2(jmax, flux_CMB,           &
!!     &          is_fld, is_div)
!!      subroutine cal_sph_cmb_fix_flux_diffuse2(jmax, flux_CMB,        &
!!     &          coef_d, is_fld, is_diffuse)
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
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_sph_spectr_data
!
      implicit none
!
      private :: cal_dsdr_sph_in_fix_flux_2, cal_div_sph_in_fix_flux_2
      private :: cal_sph_in_fix_flux_diffuse2
      private :: cal_dsdr_sph_out_fix_flux_2, cal_div_sph_out_fix_flux_2
      private :: cal_sph_out_fix_flux_diffuse2
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_dsdr_sph_icb_fix_flux_2(jmax, flux_ICB,            &
     &          is_fld, is_grd)
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: is_fld, is_grd
      real(kind = kreal), intent(in) :: flux_ICB(jmax)
!
!
      call cal_dsdr_sph_in_fix_flux_2(jmax, nlayer_ICB, flux_ICB,       &
     &    is_fld, is_grd)
!
      end subroutine cal_dsdr_sph_icb_fix_flux_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_div_sph_icb_fix_flux_2(jmax, flux_ICB,             &
     &          is_fld, is_div)
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: is_fld, is_div
      real(kind = kreal), intent(in) :: flux_ICB(jmax)
!
!
      call cal_div_sph_in_fix_flux_2(jmax, nlayer_ICB, flux_ICB,        &
     &    is_fld, is_div)
!
      end subroutine cal_div_sph_icb_fix_flux_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_icb_fix_flux_diffuse2(jmax, flux_ICB,          &
     &          coef_d, is_fld, is_diffuse)
!
      use m_coef_fdm_fixed_ICB
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: flux_ICB(jmax)
      real(kind = kreal), intent(in) :: coef_d
!
!
      call cal_sph_in_fix_flux_diffuse2(jmax, nlayer_ICB, flux_ICB,     &
     &    coef_fdm_fix_dr_ICB_2, coef_d, is_fld, is_diffuse)
!
      end subroutine cal_sph_icb_fix_flux_diffuse2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_dsdr_sph_cmb_fix_flux_2(jmax, flux_CMB,            &
     &          is_fld, is_grd)
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: is_fld, is_grd
      real(kind = kreal), intent(in) :: flux_CMB(jmax)
!
!
      call cal_dsdr_sph_out_fix_flux_2(jmax, nlayer_CMB, flux_CMB,      &
     &    is_fld, is_grd)
!
      end subroutine cal_dsdr_sph_cmb_fix_flux_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_div_sph_cmb_fix_flux_2(jmax, flux_CMB,             &
     &          is_fld, is_div)
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: is_fld, is_div
      real(kind = kreal), intent(in) :: flux_CMB(jmax)
!
!
      call cal_div_sph_out_fix_flux_2(jmax, nlayer_CMB, flux_CMB,       &
     &    is_fld, is_div)
!
      end subroutine cal_div_sph_cmb_fix_flux_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_cmb_fix_flux_diffuse2(jmax, flux_CMB,          &
     &          coef_d, is_fld, is_diffuse)
!
      use m_coef_fdm_fixed_CMB
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: flux_CMB(jmax)
      real(kind = kreal), intent(in) :: coef_d
!
!
      call cal_sph_out_fix_flux_diffuse2(jmax, nlayer_CMB, flux_CMB,    &
     &    coef_fdm_fix_dr_CMB_2, coef_d, is_fld, is_diffuse)
!
      end subroutine cal_sph_cmb_fix_flux_diffuse2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_dsdr_sph_in_fix_flux_2(jmax, kr_in, flux_ICB,      &
     &          is_fld, is_grd)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld, is_grd
      real(kind = kreal), intent(in) :: flux_ICB(jmax)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        d_rj(inod,is_grd  ) = flux_ICB(j) * g_sph_rj(j,13)              &
     &                       * radius_1d_rj_r(kr_in)**2
        d_rj(inod,is_grd+1) = d_rj(inod,is_fld  )
        d_rj(inod,is_grd+2) = zero
      end do
!$omp end parallel do
!
      if(idx_rj_degree_zero .eq. 0) return
      inod = idx_rj_degree_zero + (kr_in-1) * jmax
      d_rj(inod,is_grd  ) = flux_ICB(idx_rj_degree_zero)                &
     &                     * radius_1d_rj_r(kr_in)**2
!
      end subroutine cal_dsdr_sph_in_fix_flux_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_div_sph_in_fix_flux_2(jmax, kr_in, flux_ICB,      &
     &          is_fld, is_div)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld, is_div
      real(kind = kreal), intent(in) :: flux_ICB(jmax)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        d_rj(inod,is_div) =  (flux_ICB(j) - d_rj(inod,is_fld+1) )       &
     &                   * max(g_sph_rj(j,3),half) * ar_1d_rj(kr_in,2)
      end do
!$omp end parallel do
!
      end subroutine cal_div_sph_in_fix_flux_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_in_fix_flux_diffuse2(jmax, kr_in, flux_IN,     &
     &          coef_fdm_fix_dr_in_2, coef_d, is_fld, is_diffuse)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: flux_IN(jmax)
      real(kind = kreal), intent(in) :: coef_fdm_fix_dr_in_2(-1:1,3)
      real(kind = kreal), intent(in) :: coef_d
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
        d2t_dr2 =  coef_fdm_fix_dr_in_2(-1,3) * flux_IN(j)              &
     &           + coef_fdm_fix_dr_in_2( 0,3) * d_rj(inod,is_fld)       &
     &           + coef_fdm_fix_dr_in_2( 1,3) * d_rj(i_p1,is_fld)
!
        d_rj(inod,is_diffuse) = coef_d * (d2t_dr2                       &
     &                    + two*ar_1d_rj(kr_in,1) * flux_IN(j)          &
     &                    - g_sph_rj(j,3)*ar_1d_rj(kr_in,2)             &
     &                     * d_rj(inod,is_fld) )
!
      end do
!$omp end parallel do
!
      end subroutine cal_sph_in_fix_flux_diffuse2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_dsdr_sph_out_fix_flux_2(jmax, kr_out, flux_CMB,    &
     &          is_fld, is_grd)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_grd
      real(kind = kreal), intent(in) :: flux_CMB(jmax)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        d_rj(inod,is_grd  ) = flux_CMB(j) * g_sph_rj(j,13)              &
     &                       * radius_1d_rj_r(kr_out)**2
        d_rj(inod,is_grd+1) = d_rj(inod,is_fld  )
        d_rj(inod,is_grd+2) = zero
      end do
!$omp end parallel do
!
      if(idx_rj_degree_zero .eq. 0) return
      inod = idx_rj_degree_zero + (kr_out-1) * jmax
      d_rj(inod,is_grd  ) = flux_CMB(idx_rj_degree_zero)                &
     &                     * radius_1d_rj_r(kr_out)**2
!
      end subroutine cal_dsdr_sph_out_fix_flux_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_div_sph_out_fix_flux_2(jmax, kr_out, flux_CMB,    &
     &          is_fld, is_div)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_div
      real(kind = kreal), intent(in) :: flux_CMB(jmax)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        d_rj(inod,is_div) =  (flux_CMB(j) - d_rj(inod,is_fld+1) )       &
     &                  * max(g_sph_rj(j,3),half) * ar_1d_rj(kr_out,2)
      end do
!$omp end parallel do
!
      end subroutine cal_div_sph_out_fix_flux_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_out_fix_flux_diffuse2(jmax, kr_out, flux_OUT,  &
     &          coef_fdm_fix_dr_out_2, coef_d, is_fld, is_diffuse)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: coef_fdm_fix_dr_out_2(-1:1,3)
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
        d2t_dr2 =  coef_fdm_fix_dr_out_2(-1,3) * d_rj(i_n1,is_fld)      &
     &           + coef_fdm_fix_dr_out_2( 0,3) * d_rj(inod,is_fld)      &
     &           + coef_fdm_fix_dr_out_2( 1,3) * flux_OUT(j)
!
        d_rj(inod,is_diffuse) = coef_d * (d2t_dr2                       &
     &                    + two*ar_1d_rj(kr_out,1) * flux_OUT(j)        &
     &                    - g_sph_rj(j,3)*ar_1d_rj(kr_out,2)            &
     &                     * d_rj(inod,is_fld) )
!
      end do
!$omp end parallel do
!
      end subroutine cal_sph_out_fix_flux_diffuse2
!
! -----------------------------------------------------------------------
!
      end module cal_sph_exp_fixed_flux
