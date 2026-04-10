!>@file   sph_exp_fix_scalar_CMB.f90
!!@brief  module sph_exp_fix_scalar_CMB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Set fixed scalar boundarry for explicit method
!!
!!@verbatim
!!      subroutine dsdr_sph_fix_scalar_out_2(jmax, g_sph_rj,            &
!!     &          kr_out, r_CMB, fdm2_fix_fld_CMB, fix_CMB,             &
!!     &          n_point, d_rj_scl, d_rj_grad)
!!        integer(kind = kint), intent(in) :: jmax, kr_out
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: r_CMB(0:2)
!!        real(kind = kreal), intent(in) :: fix_CMB(jmax)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
!!        real(kind = kreal), intent(inout) :: d_rj_scl(n_point)
!!        real(kind = kreal), intent(inout) :: d_rj_grad(n_point,3)
!!      subroutine dsdr_sph_lm0_fix_scalar_out_2(idx_rj_degree_zero,    &
!!     &          jmax, kr_out, r_CMB, fdm2_fix_fld_CMB, fix_CMB,       &
!!     &          n_point, d_rj_scl, d_rj_grad)
!!        integer(kind = kint), intent(in) :: idx_rj_degree_zero
!!        integer(kind = kint), intent(in) :: jmax, kr_out
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: r_CMB(0:2)
!!        real(kind = kreal), intent(in) :: fix_CMB(jmax)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
!!        real(kind = kreal), intent(in) :: d_rj_scl(n_point)
!!        real(kind = kreal), intent(inout) :: d_rj_grad(n_point,3)
!!      subroutine cal_dsdr_sph_no_bc_out_2(jmax, kr_out,               &
!!     &          fdm2_fix_fld_CMB, n_point, d_rj_pol, d_rj_dr)
!!        integer(kind = kint), intent(in) :: jmax, kr_out
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
!!        real(kind = kreal), intent(in) :: d_rj_pol(n_point)
!!        real(kind = kreal), intent(inout) :: d_rj_dr(n_point)
!!
!!      subroutine cal_sph_div_flux_4_fix_out(jmax, g_sph_rj,           &
!!     &          kr_out, r_CMB, fdm2_fix_fld_CMB, fix_CMB,             &
!!     &          n_point, d_rj_fld, d_rj_div)
!!        integer(kind = kint), intent(in) :: jmax, kr_out
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: fix_CMB(jmax)
!!        real(kind = kreal), intent(in) :: r_CMB(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
!!        real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_div(n_point)
!!@endverbatim
!!
!!@n @param idx_rj_degree_zero    Local address for degree 0
!!@n @param n_point  Number of points for spectrum data
!!@n @param jmax         Number of local spherical harmonics mode
!!@n @param kr_out        Radial ID for outer boundary
!!@n @param r_CMB(0:2)    Radius at CMB
!!@n @param fdm2_fix_fld_CMB(-2:0,3)
!!         Matrix to evaluate radial derivative at CMB with fixed field
!!
!!@n @param fix_CMB(jmax) Spectr data for fixed scalar at CMB
!!@n @param fix_CTR(jmax) Spectr data for fixed scalar at center
!!
!!@n @param is_fld       Field address of input field
!!@n @param is_grd       Field address of radial gradient of field
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module sph_exp_fix_scalar_CMB
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
      subroutine dsdr_sph_fix_scalar_out_2(jmax, g_sph_rj,              &
     &          kr_out, r_CMB, fdm2_fix_fld_CMB, fix_CMB,               &
     &          n_point, d_rj_scl, d_rj_grad)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fix_CMB(jmax)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
!
      real(kind = kreal), intent(inout) :: d_rj_scl(n_point)
      real(kind = kreal), intent(inout) :: d_rj_grad(n_point,3)
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
        d1t_dr1 =  fdm2_fix_fld_CMB(-2,2) * d_rj_scl(i_n2)              &
     &           + fdm2_fix_fld_CMB(-1,2) * d_rj_scl(i_n1)              &
     &           + fdm2_fix_fld_CMB( 0,2) * fix_CMB(j)
!
        d_rj_scl(inod   ) = fix_CMB(j)
        d_rj_grad(inod,1) = d1t_dr1 * g_sph_rj(j,13) * r_CMB(0)**2
        d_rj_grad(inod,2) = fix_CMB(j)
        d_rj_grad(inod,3) = zero
      end do
!$omp end parallel do
!
      end subroutine dsdr_sph_fix_scalar_out_2
!
! -----------------------------------------------------------------------
!
      subroutine dsdr_sph_lm0_fix_scalar_out_2(idx_rj_degree_zero,      &
     &          jmax, kr_out, r_CMB, fdm2_fix_fld_CMB, fix_CMB,         &
     &          n_point, d_rj_scl, d_rj_grad)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fix_CMB(jmax)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
!
      real(kind = kreal), intent(in) :: d_rj_scl(n_point)
      real(kind = kreal), intent(inout) :: d_rj_grad(n_point,3)
!
      real(kind = kreal) :: d1t_dr1
      integer(kind = kint) :: inod, i_n1, i_n2
!
!
      if(idx_rj_degree_zero .le. 0) return
!
      inod = idx_rj_degree_zero + (kr_out-1) * jmax
      i_n1 = inod - jmax
      i_n2 = i_n1 - jmax
!
      d1t_dr1 =   fdm2_fix_fld_CMB(-2,2) * d_rj_scl(i_n2)               &
     &          + fdm2_fix_fld_CMB(-1,2) * d_rj_scl(i_n1)               &
     &          + fdm2_fix_fld_CMB( 0,2) * fix_CMB(idx_rj_degree_zero)
!
      d_rj_grad(inod,1) = d1t_dr1 * r_CMB(0)**2
      d_rj_grad(inod,2) = zero
      d_rj_grad(inod,3) = zero
!
      end subroutine dsdr_sph_lm0_fix_scalar_out_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_dsdr_sph_no_bc_out_2(jmax, kr_out,                 &
     &          fdm2_fix_fld_CMB, n_point, d_rj_pol, d_rj_dr)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
      real(kind = kreal), intent(in) :: d_rj_pol(n_point)
!
      real(kind = kreal), intent(inout) :: d_rj_dr(n_point)
!
      integer(kind = kint) :: inod, i_n1, i_n2, j
!
!
!$omp parallel do private(inod,i_n1,i_n2)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d_rj_dr(inod) =  fdm2_fix_fld_CMB(-2,2) * d_rj_pol(i_n2)        &
     &                 + fdm2_fix_fld_CMB(-1,2) * d_rj_pol(i_n1)        &
     &                 + fdm2_fix_fld_CMB( 0,2) * d_rj_pol(inod)
      end do
!$omp end parallel do
!
      end subroutine cal_dsdr_sph_no_bc_out_2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_div_flux_4_fix_out(jmax, g_sph_rj,             &
     &          kr_out, r_CMB, fdm2_fix_fld_CMB, fix_CMB,               &
     &          n_point, d_rj_fld, d_rj_div)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: fix_CMB(jmax)
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
      real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!
      real(kind = kreal), intent(inout) :: d_rj_div(n_point)
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
        d1s_dr1 =  fdm2_fix_fld_CMB(-2,2) * d_rj_fld(i_n2,1)            &
     &           + fdm2_fix_fld_CMB(-1,2) * d_rj_fld(i_n1,1)            &
     &           + fdm2_fix_fld_CMB( 0,2) * fix_CMB(j)
!
        d_rj_div(inod) =  (d1s_dr1 - d_rj_fld(inod,2) )                 &
     &                   * max(g_sph_rj(j,3),half) * r_CMB(2)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_div_flux_4_fix_out
!
! -----------------------------------------------------------------------
!
      end module sph_exp_fix_scalar_CMB
