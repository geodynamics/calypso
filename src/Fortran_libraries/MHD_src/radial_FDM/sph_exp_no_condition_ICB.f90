!>@file   sph_exp_no_condition_ICB.f90
!!@brief  module sph_exp_no_condition_ICB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate derivatives with no boundary conditions
!!
!!@verbatim
!!      subroutine cal_sph_nod_nobc_in_grad2(jmax, g_sph_rj,            &
!!     &          kr_in, r_ICB, fdm2_fix_fld_ICB,                       &
!!     &          n_point, d_rj_fld, d_rj_grad)
!!        integer(kind = kint), intent(in) :: jmax, kr_in
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: r_ICB(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: d_rj_fld(n_point)
!!        real(kind = kreal), intent(inout) :: d_rj_grad(n_point,3)
!!      subroutine cal_sph_nod_nobc_in_rot2(jmax, g_sph_rj,             &
!!     &          kr_in, r_ICB, fdm2_fix_fld_ICB,                       &
!!     &          n_point, d_rj_fld, d_rj_rot)
!!        integer(kind = kint), intent(in) :: jmax, kr_in
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: r_ICB(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_rot(n_point,3)
!!      subroutine cal_sph_nod_nobc_in_div2(jmax, g_sph_rj, kr_in,      &
!!     &          r_ICB, fdm2_fix_fld_ICB, n_point, d_rj_fld, d_rj_div)
!!        integer(kind = kint), intent(in) :: jmax, kr_in
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: r_ICB(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_div(n_point)
!!      subroutine cal_sph_nod_nobc_in_diffuse2(jmax, g_sph_rj,         &
!!     &          kr_in, r_ICB, fdm2_fix_fld_ICB,                       &
!!     &          n_point, d_rj_fld, d_rj_diffuse)
!!        integer(kind = kint), intent(in) :: jmax, kr_in
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: r_ICB(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_diffuse(n_point,3)
!!@endverbatim
!!
!!@n @param fdm2_fix_fld_ICB(0:2,3)
!!            Finite difference matrix for ICB with no boundary condition
!!
!!@n @param n_point  Number of points for spectrum data
!!@n @param jmax         Number of local spherical harmonics mode
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param r_ICB(0:2)   Radius at ICB
!
      module sph_exp_no_condition_ICB
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
      subroutine cal_sph_nod_nobc_in_grad2(jmax, g_sph_rj,              &
     &          kr_in, r_ICB, fdm2_fix_fld_ICB,                         &
     &          n_point, d_rj_fld, d_rj_grad)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: d_rj_fld(n_point)
      real(kind = kreal), intent(inout) :: d_rj_grad(n_point,3)
!
      integer(kind = kint) :: inod, j, i_p1, i_p2
      real(kind = kreal) :: d1s_dr1
!
!
!$omp parallel
!$omp do private(inod,j)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        d_rj_grad(inod,2) = d_rj_fld(inod)
        d_rj_grad(inod,3) = zero
      end do
!$omp end do
!
!$omp do private(inod,i_p1,i_p2,j,d1s_dr1)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d1s_dr1 =  fdm2_fix_fld_ICB( 0,2) * d_rj_grad(inod,2)        &
     &           + fdm2_fix_fld_ICB( 1,2) * d_rj_grad(i_p1,2)        &
     &           + fdm2_fix_fld_ICB( 2,2) * d_rj_grad(i_p2,2)
!
        d_rj_grad(inod,1) = d1s_dr1 * g_sph_rj(j,13) * r_ICB(0)**2
      end do
!$omp end do
!$omp end parallel
!
      end subroutine cal_sph_nod_nobc_in_grad2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_nobc_in_rot2(jmax, g_sph_rj,               &
     &          kr_in, r_ICB, fdm2_fix_fld_ICB,                         &
     &          n_point, d_rj_fld, d_rj_rot)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_rot(n_point,3)
!
      integer(kind = kint) :: inod, j, i_p1, i_p2
      real(kind = kreal) :: d2s_dr2, d1t_dr1
!
!
!$omp parallel do private(inod,i_p1,i_p2,j,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d2s_dr2 =  fdm2_fix_fld_ICB( 0,3) * d_rj_fld(inod,1)            &
     &           + fdm2_fix_fld_ICB( 1,3) * d_rj_fld(i_p1,1)            &
     &           + fdm2_fix_fld_ICB( 2,3) * d_rj_fld(i_p2,1)
        d1t_dr1 =  fdm2_fix_fld_ICB( 0,2) * d_rj_fld(inod,3)            &
     &           + fdm2_fix_fld_ICB( 1,2) * d_rj_fld(i_p1,3)            &
     &           + fdm2_fix_fld_ICB( 2,2) * d_rj_fld(i_p2,3)
!
        d_rj_rot(inod,1) = d_rj_fld(inod,3)
        d_rj_rot(inod,2) = d1t_dr1
        d_rj_rot(inod,3) = - (d2s_dr2                                   &
     &                  - g_sph_rj(j,3) * r_ICB(2) * d_rj_fld(inod,1))
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_nobc_in_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_nobc_in_div2(jmax, g_sph_rj, kr_in,        &
     &          r_ICB, fdm2_fix_fld_ICB, n_point, d_rj_fld, d_rj_div)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_div(n_point)
!
      integer(kind = kint) :: inod, j, i_p1, i_p2
      real(kind = kreal) :: d1s_dr1
!
!
!$omp parallel do private(inod,i_p1,i_p2,j,d1s_dr1)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d1s_dr1 =  fdm2_fix_fld_ICB( 0,2) * d_rj_fld(inod,1)            &
     &           + fdm2_fix_fld_ICB( 1,2) * d_rj_fld(i_p1,1)            &
     &           + fdm2_fix_fld_ICB( 2,2) * d_rj_fld(i_p2,1)
!
        d_rj_div(inod) =  (d1s_dr1 - d_rj_fld(inod,2))                  &
     &                   * max(g_sph_rj(j,3),half)*r_ICB(2)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_nobc_in_div2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_nobc_in_diffuse2(jmax, g_sph_rj,           &
     &          kr_in, r_ICB, fdm2_fix_fld_ICB,                         &
     &          n_point, d_rj_fld, d_rj_diffuse)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!
      real(kind = kreal), intent(inout) :: d_rj_diffuse(n_point,3)
!
      integer(kind = kint) :: inod, j, i_p1, i_p2
      real(kind = kreal) :: d2s_dr2,d2t_dr2
!
!
!$omp parallel do private(inod,i_p1,i_p2,j,d2s_dr2,d2t_dr2)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d2s_dr2 =  fdm2_fix_fld_ICB( 0,3) * d_rj_fld(inod,1)            &
     &           + fdm2_fix_fld_ICB( 1,3) * d_rj_fld(i_p1,1)            &
     &           + fdm2_fix_fld_ICB( 2,3) * d_rj_fld(i_p2,1)
        d2t_dr2 =  fdm2_fix_fld_ICB( 0,3) * d_rj_fld(inod,3)            &
     &           + fdm2_fix_fld_ICB( 1,3) * d_rj_fld(i_p1,3)            &
     &           + fdm2_fix_fld_ICB( 2,3) * d_rj_fld(i_p2,3)
!
        d_rj_diffuse(inod,1) =   d2s_dr2                                &
     &               - g_sph_rj(j,3) * r_ICB(2) * d_rj_fld(inod,1)
        d_rj_diffuse(inod,3) =   d2t_dr2                                &
     &               - g_sph_rj(j,3) * r_ICB(2) * d_rj_fld(inod,3)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_nobc_in_diffuse2
!
! -----------------------------------------------------------------------
!
      end module sph_exp_no_condition_ICB
