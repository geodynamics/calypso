!>@file   cal_sph_exp_fixed_scalar.f90
!!@brief  module cal_sph_exp_fixed_scalar
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Set fixed scalar boundarry for explicit method
!!
!!@verbatim
!!      subroutine cal_dsdr_sph_icb_fix_scalar_2(jmax, fix_ICB,         &
!!     &          is_fld, is_grd)
!!      subroutine cal_sph_icb_fix_scalar_diffuse2(jmax, fix_ICB,       &
!!     &          coef_d, is_fld, is_diffuse)
!!      subroutine cal_dsdr_sph_icb_nobc_2(is_fld, is_grd)
!!
!!      subroutine cal_dsdr_sph_cmb_fix_scalar_2(jmax, fix_CMB,         &
!!     &          is_fld, is_grd)
!!      subroutine cal_sph_cmb_fix_scalar_diffuse2(jmax, fix_CMB,       &
!!     &          coef_d, is_fld, is_diffuse)
!!      subroutine cal_dsdr_sph_cmb_nobc_2(is_fld, is_grd)
!!
!!      subroutine cal_sph_div_flux_4_icb_fix(jmax, fix_ICB,            &
!!     &          is_fld, is_div)
!!      subroutine cal_sph_div_flux_4_cmb_fix(jmax, fix_CMB,            &
!!     &          is_fld, is_div)
!!@endverbatim
!!
!!@n @param jmax         Number of local spherical harmonics mode
!!
!!@n @param fix_ICB(jmax) Spectr data for fixed fixed scalar at ICB
!!@n @param fix_CMB(jmax) Spectr data for fixed fixed scalar at CMB
!!@n @param coef_d        Coefficient for diffusion term
!!
!!@n @param is_fld       Field address of input field
!!@n @param is_grd       Field address of radial gradient of field
!!@n @param is_diffuse   Field address for diffusion of field
!
      module cal_sph_exp_fixed_scalar
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
      private :: cal_dsdr_sph_fix_scalar_in_2, cal_dsdr_sph_no_bc_in_2
      private :: cal_sph_fix_scalar_in_diffuse2
      private :: cal_dsdr_sph_fix_scalar_out_2,cal_dsdr_sph_no_bc_out_2
      private :: cal_sph_out_fix_scalar_diffuse2
      private :: cal_sph_div_flux_4_fix_in
      private :: cal_sph_div_flux_4_fix_out
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_dsdr_sph_icb_fix_scalar_2(jmax, fix_ICB,           &
     &          is_fld, is_grd)
!
      use m_coef_fdm_fixed_ICB
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: is_fld, is_grd
      real(kind = kreal), intent(in) :: fix_ICB(jmax)
!
!
      call cal_dsdr_sph_fix_scalar_in_2(jmax, nlayer_ICB, fix_ICB,      &
     &    coef_fdm_fix_ICB_2, is_fld, is_grd)
!
      end subroutine cal_dsdr_sph_icb_fix_scalar_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_icb_fix_scalar_diffuse2(jmax, fix_ICB,         &
     &          coef_d, is_fld, is_diffuse)
!
      use m_coef_fdm_fixed_ICB
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: fix_ICB(jmax)
      real(kind = kreal), intent(in) :: coef_d
!
!
      call cal_sph_fix_scalar_in_diffuse2(jmax, nlayer_ICB, fix_ICB,    &
     &    coef_fdm_fix_ICB_2, coef_d, is_fld, is_diffuse)
!
      end subroutine cal_sph_icb_fix_scalar_diffuse2
!
! -----------------------------------------------------------------------
!
      subroutine cal_dsdr_sph_icb_nobc_2(is_fld, is_grd)
!
      use m_coef_fdm_fixed_ICB
!
      integer(kind = kint), intent(in) :: is_fld, is_grd
!
!
      call cal_dsdr_sph_no_bc_in_2(nlayer_ICB, coef_fdm_fix_ICB_2,      &
     &     is_fld, is_grd)
!
      end subroutine cal_dsdr_sph_icb_nobc_2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_dsdr_sph_cmb_fix_scalar_2(jmax, fix_CMB,           &
     &          is_fld, is_grd)
!
      use m_coef_fdm_fixed_CMB
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: is_fld, is_grd
      real(kind = kreal), intent(in) :: fix_CMB(jmax)
!
!
      call cal_dsdr_sph_fix_scalar_out_2(jmax, nlayer_CMB, fix_CMB,     &
     &    coef_fdm_fix_CMB_2, is_fld, is_grd)
!
      end subroutine cal_dsdr_sph_cmb_fix_scalar_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_cmb_fix_scalar_diffuse2(jmax, fix_CMB,         &
     &          coef_d, is_fld, is_diffuse)
!
      use m_coef_fdm_fixed_CMB
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: fix_CMB(jmax)
!
!
      call cal_sph_out_fix_scalar_diffuse2(jmax, nlayer_CMB, fix_CMB,   &
     &    coef_fdm_fix_CMB_2, coef_d, is_fld, is_diffuse)
!
      end subroutine cal_sph_cmb_fix_scalar_diffuse2
!
! -----------------------------------------------------------------------
!
      subroutine cal_dsdr_sph_cmb_nobc_2(is_fld, is_grd)
!
      use m_coef_fdm_fixed_CMB
!
      integer(kind = kint), intent(in) :: is_fld, is_grd
!
!
      call cal_dsdr_sph_no_bc_out_2(nlayer_CMB, coef_fdm_fix_CMB_2,     &
     &    is_fld, is_grd)
!
      end subroutine cal_dsdr_sph_cmb_nobc_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_div_flux_4_icb_fix(jmax, fix_ICB,              &
     &          is_fld, is_div)
!
      use m_coef_fdm_fixed_ICB
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: is_fld, is_div
      real(kind = kreal), intent(in) :: fix_ICB(jmax)
!
!
      call cal_sph_div_flux_4_fix_in(jmax, nlayer_ICB, fix_ICB,         &
     &    coef_fdm_fix_ICB_2, is_fld, is_div)
!
      end subroutine cal_sph_div_flux_4_icb_fix
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_div_flux_4_cmb_fix(jmax, fix_CMB,              &
     &          is_fld, is_div)
!
      use m_coef_fdm_fixed_CMB
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: is_fld, is_div
      real(kind = kreal), intent(in) :: fix_CMB(jmax)
!
!
      call cal_sph_div_flux_4_fix_out(jmax, nlayer_CMB, fix_CMB,        &
     &    coef_fdm_fix_CMB_2, is_fld, is_div)
!
      end subroutine cal_sph_div_flux_4_cmb_fix
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_dsdr_sph_fix_scalar_in_2(jmax, kr_in, fix_ICB,     &
     &          coef_fdm_fix_in_2, is_fld, is_grd)
!
      integer(kind = kint), intent(in) :: is_fld, is_grd
      integer(kind = kint), intent(in) :: jmax, kr_in
      real(kind = kreal), intent(in) :: fix_ICB(jmax)
      real(kind = kreal), intent(in) :: coef_fdm_fix_in_2(0:2,3)
!
      real(kind = kreal) :: d1t_dr1
      integer(kind = kint) :: inod, i_p1, i_p2, j
!
!
!$omp parallel do private(inod,i_p1,i_p2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d1t_dr1 =  coef_fdm_fix_in_2( 0,2) * fix_ICB(j)                 &
     &           + coef_fdm_fix_in_2( 1,2) * d_rj(i_p1,is_fld)          &
     &           + coef_fdm_fix_in_2( 2,2) * d_rj(i_p2,is_fld)
!
        d_rj(inod,is_fld  ) = fix_ICB(j)
        d_rj(inod,is_grd  ) = d1t_dr1 * g_sph_rj(j,13)                  &
     &                       * radius_1d_rj_r(kr_in)**2
        d_rj(inod,is_grd+1) = fix_ICB(j)
        d_rj(inod,is_grd+2) = zero
      end do
!$omp end parallel do
!
      if(idx_rj_degree_zero .eq. 0) return
        inod = idx_rj_degree_zero + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
        d1t_dr1 = coef_fdm_fix_in_2( 0,2) * fix_ICB(idx_rj_degree_zero) &
     &          + coef_fdm_fix_in_2( 1,2) * d_rj(i_p1,is_fld)           &
     &          + coef_fdm_fix_in_2( 2,2) * d_rj(i_p2,is_fld)
!
        d_rj(inod,is_grd  ) = d1t_dr1 * radius_1d_rj_r(kr_in)**2
!
      end subroutine cal_dsdr_sph_fix_scalar_in_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_fix_scalar_in_diffuse2(jmax, kr_in, fix_ICB, &
     &          coef_fdm_fix_in_2, coef_d, is_fld, is_diffuse)
!
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      integer(kind = kint), intent(in) :: jmax, kr_in
      real(kind = kreal), intent(in) :: fix_ICB(jmax)
      real(kind = kreal), intent(in) :: coef_fdm_fix_in_2(0:2,3)
      real(kind = kreal), intent(in) :: coef_d
!
      real(kind = kreal) :: d1t_dr1, d2t_dr2
      integer(kind = kint) :: inod, i_p1, i_p2, j
!
!
!$omp parallel do private(inod,i_p1,i_p2,d1t_dr1,d2t_dr2)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d1t_dr1 =  coef_fdm_fix_in_2( 0,2) * fix_ICB(j)                 &
     &           + coef_fdm_fix_in_2( 1,2) * d_rj(i_p1,is_fld)          &
     &           + coef_fdm_fix_in_2( 2,2) * d_rj(i_p2,is_fld)
        d2t_dr2 =  coef_fdm_fix_in_2( 0,3) * fix_ICB(j)                 &
     &           + coef_fdm_fix_in_2( 1,3) * d_rj(i_p1,is_fld)          &
     &           + coef_fdm_fix_in_2( 2,3) * d_rj(i_p2,is_fld)
!
        d_rj(inod,is_fld) = fix_ICB(j)
        d_rj(inod,is_diffuse)                                           &
     &         = coef_d * (d2t_dr2 + two*ar_1d_rj(kr_in,1)*d1t_dr1      &
     &                       - g_sph_rj(j,3)*ar_1d_rj(kr_in,2)          &
     &                        * d_rj(inod,is_fld) )
!
      end do
!$omp end parallel do
!
      end subroutine cal_sph_fix_scalar_in_diffuse2
!
! -----------------------------------------------------------------------
!
      subroutine cal_dsdr_sph_no_bc_in_2(kr_in, coef_fdm_fix_in_2,      &
     &          is_fld, is_grd)
!
      integer(kind = kint), intent(in) :: is_fld, is_grd
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: coef_fdm_fix_in_2(0:2,3)
!
      integer(kind = kint) :: inod, i_p1, i_p2, j
!
!
!$omp parallel do private(inod,i_p1,i_p2)
      do j = 1, nidx_rj(2)
        inod = j + (kr_in-1) * nidx_rj(2)
        i_p1 = inod + nidx_rj(2)
        i_p2 = i_p1 + nidx_rj(2)
!
        d_rj(inod,is_grd) =  coef_fdm_fix_in_2(0,2) * d_rj(inod,is_fld) &
     &                     + coef_fdm_fix_in_2(1,2) * d_rj(i_p1,is_fld) &
     &                     + coef_fdm_fix_in_2(2,2) * d_rj(i_p2,is_fld)
      end do
!$omp end parallel do
!
      end subroutine cal_dsdr_sph_no_bc_in_2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_dsdr_sph_fix_scalar_out_2(jmax, kr_out, fix_CMB,   &
     &          coef_fdm_fix_out_2, is_fld, is_grd)
!
      integer(kind = kint), intent(in) :: is_fld, is_grd
      integer(kind = kint), intent(in) :: jmax, kr_out
      real(kind = kreal), intent(in) :: fix_CMB(jmax)
      real(kind = kreal), intent(in) :: coef_fdm_fix_out_2(0:2,3)
!
      real(kind = kreal) :: d1t_dr1
      integer(kind = kint) :: inod, i_n1, i_n2, j
!
!
!$omp parallel do private(inod,i_n1,i_n2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d1t_dr1 =  coef_fdm_fix_out_2(2,2) * d_rj(i_n2,is_fld)          &
     &           + coef_fdm_fix_out_2(1,2) * d_rj(i_n1,is_fld)          &
     &           + coef_fdm_fix_out_2(0,2) * fix_CMB(j)
!
        d_rj(inod,is_fld  ) = fix_CMB(j)
        d_rj(inod,is_grd  ) = d1t_dr1 * g_sph_rj(j,13)                  &
     &                       * radius_1d_rj_r(kr_out)**2
        d_rj(inod,is_grd+1) = fix_CMB(j)
        d_rj(inod,is_grd+2) = zero
      end do
!$omp end parallel do
!
      if(idx_rj_degree_zero .eq. 0) return
        inod = idx_rj_degree_zero + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d1t_dr1 = coef_fdm_fix_out_2(2,2) * d_rj(i_n2,is_fld)           &
     &          + coef_fdm_fix_out_2(1,2) * d_rj(i_n1,is_fld)           &
     &          + coef_fdm_fix_out_2(0,2) * fix_CMB(idx_rj_degree_zero)
!
        d_rj(inod,is_grd  ) = d1t_dr1 * radius_1d_rj_r(kr_out)**2
!
      end subroutine cal_dsdr_sph_fix_scalar_out_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_out_fix_scalar_diffuse2(jmax, kr_out, fix_CMB, &
     &          coef_fdm_fix_out_2, coef_d, is_fld, is_diffuse)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: fix_CMB(jmax)
      real(kind = kreal), intent(in) :: coef_fdm_fix_out_2(0:2,3)
!
      real(kind = kreal) :: d1t_dr1, d2t_dr2
      integer(kind = kint) :: inod, i_n1, i_n2, j
!
!
!$omp parallel do private(inod,i_n1,i_n2,d2t_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d1t_dr1 =  coef_fdm_fix_out_2(2,2) * d_rj(i_n2,is_fld)          &
     &           + coef_fdm_fix_out_2(1,2) * d_rj(i_n1,is_fld)          &
     &           + coef_fdm_fix_out_2(0,2) * fix_CMB(j)
        d2t_dr2 =  coef_fdm_fix_out_2(2,3) * d_rj(i_n2,is_fld)          &
     &           + coef_fdm_fix_out_2(1,3) * d_rj(i_n1,is_fld)          &
     &           + coef_fdm_fix_out_2(0,3) * fix_CMB(j)
!
        d_rj(inod,is_fld) = fix_CMB(j)
        d_rj(inod,is_diffuse)                                           &
     &          =  coef_d * (d2t_dr2 + two*ar_1d_rj(kr_out,1) * d1t_dr1 &
     &                       - g_sph_rj(j,3)*ar_1d_rj(kr_out,2)         &
     &                        * d_rj(inod,is_fld))
!
      end do
!$omp end parallel do
!
      end subroutine cal_sph_out_fix_scalar_diffuse2
!
! -----------------------------------------------------------------------
!
      subroutine cal_dsdr_sph_no_bc_out_2(kr_out, coef_fdm_fix_out_2,   &
     &          is_fld, is_grd)
!
      integer(kind = kint), intent(in) :: is_fld, is_grd
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: coef_fdm_fix_out_2(0:2,3)
!
      integer(kind = kint) :: inod, i_n1, i_n2, j
!
!
!$omp parallel do private(inod,i_n1,i_n2)
      do j = 1, nidx_rj(2)
        inod = j + (kr_out-1) * nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        i_n2 = i_n1 - nidx_rj(2)
!
        d_rj(inod,is_grd) = coef_fdm_fix_out_2(2,2) * d_rj(i_n2,is_fld) &
     &                    + coef_fdm_fix_out_2(1,2) * d_rj(i_n1,is_fld) &
     &                    + coef_fdm_fix_out_2(0,2) * d_rj(inod,is_fld)
      end do
!$omp end parallel do
!
      end subroutine cal_dsdr_sph_no_bc_out_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_div_flux_4_fix_in(jmax, kr_in, fix_ICB,        &
     &          coef_fdm_fix_in_2, is_fld, is_div)
!
      integer(kind = kint), intent(in) :: is_fld, is_div
      integer(kind = kint), intent(in) :: jmax, kr_in
      real(kind = kreal), intent(in) :: fix_ICB(jmax)
      real(kind = kreal), intent(in) :: coef_fdm_fix_in_2(0:2,3)
!
      real(kind = kreal) :: d1s_dr1
      integer(kind = kint) :: inod, i_p1, i_p2, j
!
!
!$omp parallel do private(inod,i_p1,i_p2,j,d1s_dr1)
!cdir nodep
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d1s_dr1 =  coef_fdm_fix_in_2( 0,2) * fix_ICB(j)                 &
     &           + coef_fdm_fix_in_2( 1,2) * d_rj(i_p1,is_fld)          &
     &           + coef_fdm_fix_in_2( 2,2) * d_rj(i_p2,is_fld)
!
        d_rj(inod,is_div) =  (d1s_dr1 - d_rj(inod,is_fld+1) )           &
     &                   * max(g_sph_rj(j,3),half) * ar_1d_rj(kr_in,2)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_div_flux_4_fix_in
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_div_flux_4_fix_out(jmax, kr_out, fix_CMB,      &
     &          coef_fdm_fix_out_2, is_fld, is_div)
!
      integer(kind = kint), intent(in) :: is_fld, is_div
      integer(kind = kint), intent(in) :: jmax, kr_out
      real(kind = kreal), intent(in) :: fix_CMB(jmax)
      real(kind = kreal), intent(in) :: coef_fdm_fix_out_2(0:2,3)
!
      real(kind = kreal) :: d1s_dr1
      integer(kind = kint) :: inod, i_n1, i_n2, j
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d1s_dr1)
!cdir nodep
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d1s_dr1 =  coef_fdm_fix_out_2(2,2) * d_rj(i_n2,is_fld)          &
     &           + coef_fdm_fix_out_2(1,2) * d_rj(i_n1,is_fld)          &
     &           + coef_fdm_fix_out_2(0,2) * fix_CMB(j)
!
        d_rj(inod,is_div) =  (d1s_dr1 - d_rj(inod,is_fld+1) )           &
     &                   * max(g_sph_rj(j,3),half) * ar_1d_rj(kr_out,2)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_div_flux_4_fix_out
!
! -----------------------------------------------------------------------
!
      end module cal_sph_exp_fixed_scalar
