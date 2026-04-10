!>@file   sph_exp_div_scl_flux_center.f90
!!@brief  module sph_exp_div_scl_flux_center
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Set scalar advection around center
!!
!!@verbatim
!!      subroutine sph_div_flux_4_fix_center                            &
!!     &         (inod_rj_center, idx_rj_degree_zero, nnod_rj, jmax,    &
!!     &          g_sph_rj, r_CTR1, fix_ICB, fdm2_fix_fld_ctr1,         &
!!     &          flx_rj, adv_rj)
!!      subroutine sph_div_flux_4_fill_center                           &
!!     &         (inod_rj_center, idx_rj_degree_zero, nnod_rj, jmax,    &
!!     &          g_sph_rj, r_CTR1, fdm2_fix_fld_ctr1, flx_rj, adv_rj)
!!        integer(kind = kint), intent(in) :: inod_rj_center
!!        integer(kind = kint), intent(in) :: idx_rj_degree_zero
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: r_CTR1(0:2)
!!        real(kind = kreal), intent(in) :: fix_ICB(jmax)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!!        real(kind = kreal), intent(in) :: flx_rj(nnod_rj,3)
!!        real(kind = kreal), intent(inout) :: adv_rj(nnod_rj)
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
      module sph_exp_div_scl_flux_center
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
      subroutine sph_div_flux_4_fix_center                              &
     &         (inod_rj_center, idx_rj_degree_zero, nnod_rj, jmax,      &
     &          g_sph_rj, r_CTR1, fix_ICB, fdm2_fix_fld_ctr1,           &
     &          flx_rj, adv_rj)
!
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: fix_ICB(jmax)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
      real(kind = kreal), intent(in) :: flx_rj(nnod_rj,3)
!
      real(kind = kreal), intent(inout) :: adv_rj(nnod_rj)
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
     &           + fdm2_fix_fld_ctr1( 0,2) * flx_rj(j,   1)             &
     &           + fdm2_fix_fld_ctr1( 1,2) * flx_rj(i_p1,1)
!
        adv_rj(j) =  (d1s_dr1 - flx_rj(j,2) )                           &
     &              * max(g_sph_rj(j,3),half) * r_CTR1(2)
      end do
!$omp end parallel do
!
      if(inod_rj_center .gt. 0)  then
        adv_rj(inod_rj_center) = adv_rj(idx_rj_degree_zero)
      end if
!
      end subroutine sph_div_flux_4_fix_center
!
! -----------------------------------------------------------------------
!
      subroutine sph_div_flux_4_fill_center                             &
     &         (inod_rj_center, idx_rj_degree_zero, nnod_rj, jmax,      &
     &          g_sph_rj, r_CTR1, fdm2_fix_fld_ctr1, flx_rj, adv_rj)
!
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
      real(kind = kreal), intent(in) :: flx_rj(nnod_rj,3)
!
      real(kind = kreal), intent(inout) :: adv_rj(nnod_rj)
!
      real(kind = kreal) :: d1s_dr1
      integer(kind = kint) :: i_p1, j
!
!
!$omp parallel do private(i_p1,j,d1s_dr1)
      do j = 1, jmax
        i_p1 = j + jmax
        d1s_dr1 =  fdm2_fix_fld_ctr1( 0,2) * flx_rj(j,   1)             &
     &           + fdm2_fix_fld_ctr1( 1,2) * flx_rj(i_p1,1)
!
        adv_rj(j) =  (d1s_dr1 - flx_rj(j,2))                            &
     &              * max(g_sph_rj(j,3),half) * r_CTR1(2)
      end do
!$omp end parallel do
!
      if(inod_rj_center .le. 0) return
!
      i_p1 = idx_rj_degree_zero + jmax
      d1s_dr1 =  fdm2_fix_fld_ctr1(-1,2) * flx_rj(inod_rj_center,1)     &
     &         + fdm2_fix_fld_ctr1( 0,2) * flx_rj(idx_rj_degree_zero,1) &
     &         + fdm2_fix_fld_ctr1( 1,2) * flx_rj(i_p1,1)
      adv_rj(idx_rj_degree_zero)                                        &
     &    = half * r_CTR1(2) * (d1s_dr1 - flx_rj(idx_rj_degree_zero,2))
!
!
      adv_rj(inod_rj_center) = zero
!
      end subroutine sph_div_flux_4_fill_center
!
! -----------------------------------------------------------------------
!
      end module sph_exp_div_scl_flux_center
