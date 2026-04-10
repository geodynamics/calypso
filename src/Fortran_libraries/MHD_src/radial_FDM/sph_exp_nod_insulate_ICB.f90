!>@file   sph_exp_nod_insulate_ICB.f90
!!@brief  module sph_exp_nod_insulate_ICB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!!@date Modified in   Nov., 2013
!
!>@brief  Set insulated magnetic boundary condition for ICB
!!
!!@verbatim
!!      subroutine cal_sph_nod_icb_ins_b_and_j(jmax, g_sph_rj,          &
!!     &           kr_in, r_ICB, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB,     &
!!     &           n_point, d_rj_fld, d_rj_rot)
!!        integer(kind = kint), intent(in) :: jmax, kr_in
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: r_ICB(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!!        real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: d_rj_fld(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_rot(n_point,3)
!!      subroutine cal_sph_nod_icb_ins_mag2(jmax, g_sph_rj, kr_in,      &
!!     &           r_ICB, n_point, d_rj_fld)
!!        integer(kind = kint), intent(in) :: jmax, kr_in
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: r_ICB(0:2)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: d_rj_fld(n_point,3)
!!
!!      subroutine cal_sph_nod_icb_ins_vp_rot2(jmax, g_sph_rj, kr_in,   &
!!     &          r_ICB, n_point, d_rj_fld, d_rj_rot)
!!        integer(kind = kint), intent(in) :: jmax, kr_in
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: r_ICB(0:2)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_rot(n_point,3)
!!      subroutine cal_sph_nod_icb_ins_rot2(jmax, g_sph_rj,             &
!!     &          kr_in, r_ICB, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB,      &
!!     &          n_point, d_rj_fld, d_rj_rot)
!!        integer(kind = kint), intent(in) :: jmax, kr_in
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: r_ICB(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!!        real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_rot(n_point,3)
!!      subroutine cal_sph_nod_icb_ins_diffuse2(jmax, g_sph_rj,         &
!!     &          kr_in, r_ICB, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB,      &
!!     &          coef_d, n_point, d_rj_fld, d_rj_diffuse)
!!        integer(kind = kint), intent(in) :: jmax, kr_in
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in) :: r_ICB(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!!        real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_diffuse(n_point,3)
!!@endverbatim
!!
!!@n @param n_point  Number of points for spectrum data
!!@n @param jmax  Number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param r_ICB(0:2)   Radius at ICB
!!@n @param fdm2_fix_fld_ICB(0:2,3)
!!!        Matrix to evaluate radial derivative at ICB with fixed field
!!@n @param fdm2_fix_dr_ICB(-1:1,3)
!!         Matrix to evaluate field at ICB with fixed radial derivative
!!@n @param coef_d  Coefficient for diffusion term
!
      module sph_exp_nod_insulate_ICB
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
      subroutine cal_sph_nod_icb_ins_b_and_j(jmax, g_sph_rj,            &
     &           kr_in, r_ICB, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB,       &
     &           n_point, d_rj_fld, d_rj_rot)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: d_rj_fld(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_rot(n_point,3)
!
      real(kind = kreal) :: d1s_dr1, d2s_dr2, d1t_dr1
      integer(kind = kint) :: j, inod, i_p1, i_p2
!
!
!$omp parallel do private(inod,i_p1,i_p2,d1s_dr1,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d1s_dr1 =   (g_sph_rj(j,1)+one) * r_ICB(1) * d_rj_fld(inod,1)
        d2s_dr2 =  ((g_sph_rj(j,1)+one) * r_ICB(1)                      &
     &          *  fdm2_fix_dr_ICB(-1,3)                                &
     &           + fdm2_fix_dr_ICB( 0,3)) * d_rj_fld(inod,1)            &
     &           + fdm2_fix_dr_ICB( 1,3)  * d_rj_fld(i_p1,1)
        d1t_dr1 =  fdm2_fix_fld_ICB( 1,2) * d_rj_fld(i_p1,3)            &
     &           + fdm2_fix_fld_ICB( 2,2) * d_rj_fld(i_p2,3)
!
        d_rj_fld(inod,2) = d1s_dr1
        d_rj_fld(inod,3) = zero
        d_rj_rot(inod,1) = zero
        d_rj_rot(inod,2) = d1t_dr1
        d_rj_rot(inod,3) = - (d2s_dr2 - g_sph_rj(j,3)                   &
     &                                 * r_ICB(2) * d_rj_fld(inod,1))
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_icb_ins_b_and_j
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_icb_ins_mag2(jmax, g_sph_rj, kr_in,        &
     &           r_ICB, n_point, d_rj_fld)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: d_rj_fld(n_point,3)
!
      real(kind = kreal) :: d1s_dr1
      integer(kind = kint) :: j, inod
!
!
!$omp parallel do private(inod,d1s_dr1)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        d1s_dr1 =  (g_sph_rj(j,1)+one) * r_ICB(1) * d_rj_fld(inod,1)
!
        d_rj_fld(inod,2) = d1s_dr1
        d_rj_fld(inod,3) = zero
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_icb_ins_mag2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_icb_ins_vp_rot2(jmax, g_sph_rj, kr_in,     &
     &          r_ICB, n_point, d_rj_fld, d_rj_rot)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_rot(n_point,3)
!
      real(kind = kreal) :: d1t_dr1
      integer(kind = kint) :: j, inod
!
!
!$omp parallel do private(inod,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        d1t_dr1 =  (g_sph_rj(j,1)+one) * r_ICB(1) * d_rj_fld(inod,3)
!
        d_rj_rot(inod,1) = d_rj_fld(inod,3)
        d_rj_rot(inod,2) = d1t_dr1
        d_rj_rot(inod,3) = zero
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_icb_ins_vp_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_icb_ins_rot2(jmax, g_sph_rj,               &
     &          kr_in, r_ICB, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB,        &
     &          n_point, d_rj_fld, d_rj_rot)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_rot(n_point,3)
!
      real(kind = kreal) :: d2s_dr2, d1t_dr1
      integer(kind = kint) :: j, inod, i_p1, i_p2
!
!
!$omp parallel do private(inod,i_p1,i_p2,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d2s_dr2 =  ( (g_sph_rj(j,1)+one) * r_ICB(1)                     &
     &          *  fdm2_fix_dr_ICB(-1,3)                                &
     &           + fdm2_fix_dr_ICB( 0,3)) * d_rj_fld(inod,1)            &
     &           + fdm2_fix_dr_ICB( 1,3)  * d_rj_fld(i_p1,1)
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
      end subroutine cal_sph_nod_icb_ins_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_icb_ins_diffuse2(jmax, g_sph_rj,           &
     &          kr_in, r_ICB, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB,        &
     &          coef_d, n_point, d_rj_fld, d_rj_diffuse)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_diffuse(n_point,3)
!
      real(kind = kreal) :: d2s_dr2,d2t_dr2
      integer(kind = kint) :: j, inod,i_p1,i_p2
!
!$omp parallel do private(inod,i_p1,i_p2,d2s_dr2,d2t_dr2)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d2s_dr2 =  ( (g_sph_rj(j,1)+one) * r_ICB(1)                     &
     &           * fdm2_fix_dr_ICB(-1,3)                                &
     &           + fdm2_fix_dr_ICB( 0,3) ) * d_rj_fld(inod,1)           &
     &           + fdm2_fix_dr_ICB( 1,3) * d_rj_fld(i_p1,1)
        d2t_dr2 =  fdm2_fix_fld_ICB( 0,3) * d_rj_fld(inod,3)            &
     &           + fdm2_fix_fld_ICB( 1,3) * d_rj_fld(i_p1,3)            &
     &           + fdm2_fix_fld_ICB( 2,3) * d_rj_fld(i_p2,3)
!
        d_rj_diffuse(inod,1) = coef_d * (d2s_dr2                        &
     &           - g_sph_rj(j,3)*r_ICB(2) * d_rj_fld(inod,1))
        d_rj_diffuse(inod,3) = coef_d * (d2t_dr2                        &
     &           - g_sph_rj(j,3)*r_ICB(2) * d_rj_fld(inod,3))
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_icb_ins_diffuse2
!
! -----------------------------------------------------------------------
!
      end module sph_exp_nod_insulate_ICB
