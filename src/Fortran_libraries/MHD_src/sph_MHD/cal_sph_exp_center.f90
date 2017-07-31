!>@file   cal_sph_exp_center.f90
!!@brief  module cal_sph_exp_center
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Set center fields by explicit method
!!
!!@verbatim
!!      subroutine cal_sph_fixed_center(inod_rj_center, CTR_fld, is_fld,&
!!     &          n_point, ntot_phys_rj, d_rj)
!!      subroutine cal_sph_center1_grad22(jmax, r_CTR1, g_sph_rj,       &
!!     &          fdm2_fix_fld_ctr1, is_fld, is_grd,                    &
!!     &          n_point, ntot_phys_rj, d_rj)
!!      subroutine sph0_scalar_fill_ctr_grad2                           &
!!     &         (inod_rj_center, idx_rj_degree_zero, jmax,             &
!!     &          fdm2_fix_fld_ctr1, is_fld, is_grd,                    &
!!     &          n_point, ntot_phys_rj, d_rj)
!!      subroutine dsdr_sph_lm0_fixed_ctr_2                             &
!!     &        (inod_rj_center, idx_rj_degree_zero, jmax, r_CTR1,      &
!!     &         g_sph_rj, d_center, fdm2_fix_fld_ctr1,                 &
!!     &         fdm2_fixed_center, is_fld, is_grd,                     &
!!     &         n_point, ntot_phys_rj, d_rj)
!!
!!      subroutine cal_sph_div_flux_4_fix_ctr(jmax, r_CTR1, g_sph_rj,   &
!!     &          fix_ICB, fdm2_fix_fld_ctr1, is_fld, is_div,           &
!!     &          n_point, ntot_phys_rj, d_rj)
!!
!!      subroutine cal_sph_fixed_center1_diffuse2(jmax, r_CTR1,         &
!!     &          g_sph_rj, fdm2_fix_fld_ctr1, fix_CTR, coef_d,         &
!!     &          is_fld, is_diffuse, n_point, ntot_phys_rj, d_rj)
!!      subroutine cal_sph_filled_center_diffuse2                       &
!!     &         (inod_rj_center, idx_rj_degree_zero, jmax, r_CTR1,     &
!!     &          g_sph_rj, fdm2_fix_fld_ctr1, fdm2_fix_dr_center,      &
!!     &          coef_d, is_fld, is_diffuse,                           &
!!     &          n_point, ntot_phys_rj, d_rj)
!!      subroutine cal_sph_fixed_center_diffuse2                        &
!!     &          (inod_rj_center, idx_rj_degree_zero,                  &
!!     &           is_diffuse, n_point, ntot_phys_rj, d_rj)
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
!!@n @param is_fld       Field address of input field
!!@n @param is_grd       Field address of radial gradient of field
!!@n @param is_diffuse   Field address for diffusion of field
!!
      module cal_sph_exp_center
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
      subroutine cal_sph_fixed_center(inod_rj_center, CTR_fld, is_fld,  &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_fld
      real(kind = kreal), intent(in) :: CTR_fld
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      if(inod_rj_center .eq. 0) return
      d_rj(inod_rj_center,is_fld) = CTR_fld
!
      end subroutine cal_sph_fixed_center
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_center1_grad22(jmax, r_CTR1, g_sph_rj,         &
     &          fdm2_fix_fld_ctr1, is_fld, is_grd,                      &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_fld, is_grd
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, i_p1
      real(kind = kreal) :: d1sdr
!
!
!$omp parallel do private(inod,i_p1,d1sdr)
      do inod = 1, jmax
        i_p1 = inod + jmax
!
        d1sdr =  fdm2_fix_fld_ctr1( 0,2) * d_rj(inod,is_fld)            &
     &         + fdm2_fix_fld_ctr1( 1,2) * d_rj(i_p1,is_fld)
!
        d_rj(inod,is_grd  ) = d1sdr * g_sph_rj(inod,13) * r_CTR1(0)**2
        d_rj(inod,is_grd+1) = d_rj(inod,is_fld)
        d_rj(inod,is_grd+2) = zero
      end do
!$omp end parallel do
!
      end subroutine cal_sph_center1_grad22
!
! -----------------------------------------------------------------------
!
      subroutine sph0_scalar_fill_ctr_grad2                             &
     &         (inod_rj_center, idx_rj_degree_zero, jmax,               &
     &          fdm2_fix_fld_ctr1, is_fld, is_grd,                      &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_grd
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal) :: d1s_dr1
      integer(kind = kint) :: inod, i_p1, i_n1
!
!
      if(inod_rj_center .eq. 0) return
      inod = idx_rj_degree_zero
      i_p1 = inod + jmax
      i_n1 = inod_rj_center
!
      d1s_dr1 =  fdm2_fix_fld_ctr1(-1,2) * d_rj(i_n1,is_fld  )          &
     &         + fdm2_fix_fld_ctr1( 0,2) * d_rj(inod,is_fld  )          &
     &         + fdm2_fix_fld_ctr1( 1,2) * d_rj(i_p1,is_fld  )
!
      d_rj(inod,is_grd  ) = d1s_dr1
      d_rj(inod,is_grd+1) = zero
!
      d_rj(i_n1,is_grd  ) = zero
      d_rj(i_n1,is_grd+1) = zero
      d_rj(i_n1,is_grd+2) = zero
!
      end subroutine sph0_scalar_fill_ctr_grad2
!
! -----------------------------------------------------------------------
!
      subroutine dsdr_sph_lm0_fixed_ctr_2                               &
     &        (inod_rj_center, idx_rj_degree_zero, jmax, r_CTR1,        &
     &         g_sph_rj, d_center, fdm2_fix_fld_ctr1,                   &
     &         fdm2_fixed_center, is_fld, is_grd,                       &
     &         n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: d_center
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
      real(kind = kreal), intent(in) :: fdm2_fixed_center( 0:2,3)
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_grd
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal) :: d1sdr
      integer(kind = kint) :: inod, i_p1, i_n1
!
!
      if(inod_rj_center .eq. 0) return
      inod = idx_rj_degree_zero
      i_p1 = inod + jmax
      i_n1 = inod_rj_center
!
      d1sdr =  fdm2_fix_fld_ctr1(-1,2) * d_center                       &
     &       + fdm2_fix_fld_ctr1( 0,2) * d_rj(inod,is_fld)              &
     &       + fdm2_fix_fld_ctr1( 1,2) * d_rj(i_p1,is_fld)
!
      d_rj(inod,is_grd  ) = d1sdr * g_sph_rj(inod,13) * r_CTR1(0)**2
      d_rj(inod,is_grd+1) = zero
!
      d1sdr =  fdm2_fixed_center( 0,2) * d_rj(i_n1,is_fld  )            &
     &       + fdm2_fixed_center( 1,2) * d_rj(inod,is_fld  )            &
     &       + fdm2_fixed_center( 2,2) * d_rj(i_p1,is_fld  )
!
!
      d_rj(i_n1,is_grd  ) = d1sdr
      d_rj(i_n1,is_grd+1) = zero
      d_rj(i_n1,is_grd+2) = zero
!
      end subroutine dsdr_sph_lm0_fixed_ctr_2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_div_flux_4_fix_ctr(jmax, r_CTR1, g_sph_rj,     &
     &          fix_ICB, fdm2_fix_fld_ctr1, is_fld, is_div,             &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_fld, is_div
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: fix_ICB(jmax)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal) :: d1s_dr1
      integer(kind = kint) :: i_p1, j
!
!
!$omp parallel do private(i_p1,j,d1s_dr1)
      do j = 1, jmax
        i_p1 = j + jmax
!
        d1s_dr1 =  fdm2_fix_fld_ctr1(-1,2) * fix_ICB(j)                 &
     &           + fdm2_fix_fld_ctr1( 0,2) * d_rj(j,is_fld)             &
     &           + fdm2_fix_fld_ctr1( 1,2) * d_rj(i_p1,is_fld)
!
        d_rj(j,is_div) =  (d1s_dr1 - d_rj(j,is_fld+1) )                 &
     &                   * max(g_sph_rj(j,3),half) * r_CTR1(2)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_div_flux_4_fix_ctr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_fixed_center1_diffuse2(jmax, r_CTR1,           &
     &          g_sph_rj, fdm2_fix_fld_ctr1, fix_CTR, coef_d,           &
     &          is_fld, is_diffuse, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: fix_CTR(jmax)
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
      real(kind = kreal), intent(in) :: coef_d
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
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
     &           + fdm2_fix_fld_ctr1( 0,2) * d_rj(j,is_fld)             &
     &           + fdm2_fix_fld_ctr1( 1,2) * d_rj(i_p1,is_fld)
        d2s_dr2 =  fdm2_fix_fld_ctr1(-1,3) * fix_CTR(j)                 &
     &           + fdm2_fix_fld_ctr1( 0,3) * d_rj(j,is_fld)             &
     &           + fdm2_fix_fld_ctr1( 1,3) * d_rj(i_p1,is_fld)
!
        d_rj(j,is_diffuse)                                              &
     &         = coef_d * (d2s_dr2 + two*r_CTR1(1)*d1s_dr1              &
     &          - g_sph_rj(j,3)*r_CTR1(2) * d_rj(j,is_fld) )
!
      end do
!$omp end parallel do
!
      end subroutine cal_sph_fixed_center1_diffuse2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_filled_center_diffuse2                         &
     &         (inod_rj_center, idx_rj_degree_zero, jmax, r_CTR1,       &
     &          g_sph_rj, fdm2_fix_fld_ctr1, fdm2_fix_dr_center,        &
     &          coef_d, is_fld, is_diffuse,                             &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_center(-1:1,3)
      real(kind = kreal), intent(in) :: coef_d
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal) :: d1s_dr1, d2s_dr2
      integer(kind = kint) :: inod, i_p1, j, i_n1
!
!
!$omp parallel do private(i_p1,d1s_dr1,d2s_dr2)
      do j = 1, jmax
        i_p1 = j + jmax
!
        d1s_dr1 =  fdm2_fix_fld_ctr1( 0,2) * d_rj(j,is_fld)             &
     &           + fdm2_fix_fld_ctr1( 1,2) * d_rj(i_p1,is_fld)
        d2s_dr2 =  fdm2_fix_fld_ctr1( 0,3) * d_rj(j,is_fld)             &
     &           + fdm2_fix_fld_ctr1( 1,3) * d_rj(i_p1,is_fld)
!
        d_rj(j,is_diffuse)                                              &
     &         = coef_d * (d2s_dr2 + two*r_CTR1(1)*d1s_dr1              &
     &          - g_sph_rj(j,3)*r_CTR1(2) * d_rj(j,is_fld) )
!
      end do
!$omp end parallel do
!
!
      if(inod_rj_center .eq. 0) return
!
      i_n1 = inod_rj_center
      inod = idx_rj_degree_zero
      i_p1 = inod + jmax
!
      d1s_dr1 =  fdm2_fix_fld_ctr1(-1,2) * d_rj(i_n1,is_fld)            &
     &         + fdm2_fix_fld_ctr1( 0,2) * d_rj(inod,is_fld)            &
     &         + fdm2_fix_fld_ctr1( 1,2) * d_rj(i_p1,is_fld)
      d2s_dr2 =  fdm2_fix_fld_ctr1(-1,3) * d_rj(i_n1,is_fld)            &
     &         + fdm2_fix_fld_ctr1( 0,3) * d_rj(inod,is_fld)            &
     &         + fdm2_fix_fld_ctr1( 1,3) * d_rj(i_p1,is_fld)
!
      d_rj(inod,is_diffuse)                                             &
     &         = coef_d * (d2s_dr2 + two*r_CTR1(1)*d1s_dr1)
!
      d2s_dr2 =  fdm2_fix_dr_center( 0,3) * d_rj(i_n1,is_fld  )         &
     &         + fdm2_fix_dr_center( 1,3) * d_rj(inod,is_fld  )
!
      d_rj(i_n1,is_diffuse) = coef_d * d2s_dr2
!
      end subroutine cal_sph_filled_center_diffuse2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_fixed_center_diffuse2                          &
     &          (inod_rj_center, idx_rj_degree_zero,                    &
     &           is_diffuse, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_diffuse
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      if(inod_rj_center .eq. 0) return
      d_rj(inod_rj_center,is_diffuse)                                   &
     &      = d_rj(idx_rj_degree_zero,is_diffuse)
!
      end subroutine cal_sph_fixed_center_diffuse2
!
! -----------------------------------------------------------------------
!
      end module cal_sph_exp_center
