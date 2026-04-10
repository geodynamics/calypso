!>@file   cal_sph_exp_center.f90
!!@brief  module cal_sph_exp_center
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Set center fields by explicit method
!!
!!@verbatim
!!      subroutine cal_sph_fixed_center(inod_rj_center, CTR_fld,        &
!!     &                                n_point, d_rj_fld)
!!        integer(kind = kint), intent(in) :: inod_rj_center
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: CTR_fld
!!        real(kind = kreal), intent(inout) :: d_rj_fld(n_point)
!!      subroutine cal_sph_center1_grad22                               &
!!     &         (jmax, r_CTR1, g_sph_rj, fdm2_fix_fld_ctr1,            &
!!     &          n_point, d_rj_fld, d_rj_grad)
!!        integer(kind = kint), intent(in) :: jmax
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: r_CTR1(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!!        real(kind = kreal), intent(in) :: d_rj_fld(n_point)
!!        real(kind = kreal), intent(inout) :: d_rj_grad(n_point,3)
!!      subroutine sph0_scalar_fill_ctr_grad2                           &
!!     &         (inod_rj_center, idx_rj_degree_zero, jmax,             &
!!     &          fdm2_fix_fld_ctr1, n_point, d_rj_fld, d_rj_grad)
!!        integer(kind = kint), intent(in) :: inod_rj_center
!!        integer(kind = kint), intent(in) :: idx_rj_degree_zero
!!        integer(kind = kint), intent(in) :: n_point
!!        integer(kind = kint), intent(in) :: jmax
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!!        real(kind = kreal), intent(in) :: d_rj_fld(n_point)
!!        real(kind = kreal), intent(inout) :: d_rj_grad(n_point,3)
!!      subroutine dsdr_sph_lm0_fixed_ctr_2                             &
!!     &         (inod_rj_center, idx_rj_degree_zero, jmax, r_CTR1,     &
!!     &          g_sph_rj, d_center, fdm2_fix_fld_ctr1,                &
!!     &          fdm2_fixed_center, n_point, d_rj_fld, d_rj_grad)
!!        integer(kind = kint), intent(in) :: inod_rj_center
!!        integer(kind = kint), intent(in) :: idx_rj_degree_zero
!!        integer(kind = kint), intent(in) :: jmax
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: d_center
!!        real(kind = kreal), intent(in) :: r_CTR1(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!!        real(kind = kreal), intent(in) :: fdm2_fixed_center( 0:2,3)
!!        real(kind = kreal), intent(in) :: d_rj_fld(n_point)
!!        real(kind = kreal), intent(inout) :: d_rj_grad(n_point,3)
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
      subroutine cal_sph_fixed_center(inod_rj_center, CTR_fld,          &
     &                                n_point, d_rj_fld)
!
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: CTR_fld
!
      real(kind = kreal), intent(inout) :: d_rj_fld(n_point)
!
!
      if(inod_rj_center .eq. 0) return
      d_rj_fld(inod_rj_center) = CTR_fld
!
      end subroutine cal_sph_fixed_center
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_center1_grad22                                 &
     &         (jmax, r_CTR1, g_sph_rj, fdm2_fix_fld_ctr1,              &
     &          n_point, d_rj_fld, d_rj_grad)
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!
      real(kind = kreal), intent(in) :: d_rj_fld(n_point)
      real(kind = kreal), intent(inout) :: d_rj_grad(n_point,3)
!
      integer(kind = kint) :: inod, i_p1
      real(kind = kreal) :: d1sdr
!
!
!$omp parallel do private(inod,i_p1,d1sdr)
      do inod = 1, jmax
        i_p1 = inod + jmax
!
        d1sdr =  fdm2_fix_fld_ctr1( 0,2) * d_rj_fld(inod)               &
     &         + fdm2_fix_fld_ctr1( 1,2) * d_rj_fld(i_p1)
!
        d_rj_grad(inod,1) = d1sdr * g_sph_rj(inod,13) * r_CTR1(0)**2
        d_rj_grad(inod,2) = d_rj_fld(inod)
        d_rj_grad(inod,3) = zero
      end do
!$omp end parallel do
!
      end subroutine cal_sph_center1_grad22
!
! -----------------------------------------------------------------------
!
      subroutine sph0_scalar_fill_ctr_grad2                             &
     &         (inod_rj_center, idx_rj_degree_zero, jmax,               &
     &          fdm2_fix_fld_ctr1, n_point, d_rj_fld, d_rj_grad)
!
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: n_point
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
!
      real(kind = kreal), intent(in) :: d_rj_fld(n_point)
      real(kind = kreal), intent(inout) :: d_rj_grad(n_point,3)
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
      d1s_dr1 =  fdm2_fix_fld_ctr1(-1,2) * d_rj_fld(i_n1)               &
     &         + fdm2_fix_fld_ctr1( 0,2) * d_rj_fld(inod)               &
     &         + fdm2_fix_fld_ctr1( 1,2) * d_rj_fld(i_p1)
!
      d_rj_grad(inod,1) = d1s_dr1
      d_rj_grad(inod,2) = zero
!
      d_rj_grad(i_n1,1) = zero
      d_rj_grad(i_n1,2) = zero
      d_rj_grad(i_n1,3) = zero
!
      end subroutine sph0_scalar_fill_ctr_grad2
!
! -----------------------------------------------------------------------
!
      subroutine dsdr_sph_lm0_fixed_ctr_2                               &
     &         (inod_rj_center, idx_rj_degree_zero, jmax, r_CTR1,       &
     &          g_sph_rj, d_center, fdm2_fix_fld_ctr1,                  &
     &          fdm2_fixed_center, n_point, d_rj_fld, d_rj_grad)
!
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: d_center
      real(kind = kreal), intent(in) :: r_CTR1(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ctr1(-1:1,3)
      real(kind = kreal), intent(in) :: fdm2_fixed_center( 0:2,3)
!
      real(kind = kreal), intent(in) :: d_rj_fld(n_point)
      real(kind = kreal), intent(inout) :: d_rj_grad(n_point,3)
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
     &       + fdm2_fix_fld_ctr1( 0,2) * d_rj_fld(inod)                 &
     &       + fdm2_fix_fld_ctr1( 1,2) * d_rj_fld(i_p1)
!
      d_rj_grad(inod,1) = d1sdr * g_sph_rj(inod,13) * r_CTR1(0)**2
      d_rj_grad(inod,2) = zero
!
      d1sdr =  fdm2_fixed_center( 0,2) * d_rj_fld(i_n1)                 &
     &       + fdm2_fixed_center( 1,2) * d_rj_fld(inod)                 &
     &       + fdm2_fixed_center( 2,2) * d_rj_fld(i_p1)
!
      d_rj_grad(i_n1,1) = d1sdr
      d_rj_grad(i_n1,2) = zero
      d_rj_grad(i_n1,3) = zero
!
      end subroutine dsdr_sph_lm0_fixed_ctr_2
!
! -----------------------------------------------------------------------
!
      end module cal_sph_exp_center
