!> @file  add_sph_ICB_w_diffuse.f90
!!      module add_sph_ICB_w_diffuse
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2026
!
!> @brief Set boundary condition routines for velocity
!!
!!@verbatim
!!      subroutine add_sph_ICB_w_diffuse_by_vtor                        &
!!     &         (nri, jmax, ar_1d_rj, g_sph_rj, kr_in, Vt_ICB,         &
!!     &          d2nod_mat_fdm_2, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB,   &
!!     &          coef_d, n_point, d_rj_wp_diffuse)
!!      subroutine add_sph_ICB_w_diffuse_by_vpol                        &
!!     &         (nri, jmax, ar_1d_rj, g_sph_rj, kr_in, Vp_ICB,         &
!!     &          d2nod_mat_fdm_2, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB,   &
!!     &          coef_d, n_point, d_rj_wt_diffuse)
!!      subroutine add_sph_ICB_w_diffuse_by_dpdr                        &
!!     &         (nri, jmax, ar_1d_rj, g_sph_rj, kr_in, Vd_ICB,         &
!!     &          d2nod_mat_fdm_2, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB,   &
!!     &          coef_d, n_point, d_rj_wt_diffuse)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: ar_1d_rj(nri,3)
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: coef_d
!!        integer(kind = kint), intent(in) :: kr_in
!!        real(kind = kreal), intent(in) :: Vt_ICB(jmax)
!!        real(kind = kreal), intent(in) :: Vp_ICB(jmax)
!!        real(kind = kreal), intent(in) :: Vd_ICB(jmax)
!!        real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
!!        real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: d_rj_wp_diffuse(n_point)
!!        real(kind = kreal), intent(inout) :: d_rj_wt_diffuse(n_point)
!!@endverbatim
!!
      module add_sph_ICB_w_diffuse
!
      use m_precision
      use m_constants
!
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_sph_ICB_w_diffuse_by_vtor                          &
     &         (nri, jmax, ar_1d_rj, g_sph_rj, kr_in, Vt_ICB,           &
     &          d2nod_mat_fdm_2, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB,     &
     &          coef_d, n_point, d_rj_wp_diffuse)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: ar_1d_rj(nri,3)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: coef_d
!
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: Vt_ICB(jmax)
!
      real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: d_rj_wp_diffuse(n_point)
!
      integer(kind = kint) :: inod, j, k2, i_p1
      real(kind = kreal) :: d1t_dr1_w1, d2s_dr2_d1, d2s_dr2_d2
!
!
      k2 = kr_in + 1
!$omp parallel do                                                       &
!$omp& private(j,inod,i_p1,d1t_dr1_w1,d2s_dr2_d1,d2s_dr2_d2)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
!
        d1t_dr1_w1 =  fdm2_fix_fld_ICB( 0,2) * Vt_ICB(j)
        d2s_dr2_d1 =  fdm2_fix_dr_ICB(-1,3) * d1t_dr1_w1                &
     &              + fdm2_fix_dr_ICB( 0,3) * Vt_ICB(j)                 &
     &             - g_sph_rj(j,3)*ar_1d_rj(kr_in,2) * Vt_ICB(j)
        d2s_dr2_d2 = d2nod_mat_fdm_2(k2,-1) * Vt_ICB(j)
!
        d_rj_wp_diffuse(inod) = d_rj_wp_diffuse(inod)                   &
     &                         + coef_d * d2s_dr2_d1
        d_rj_wp_diffuse(i_p1) = d_rj_wp_diffuse(i_p1)                   &
     &                         + coef_d * d2s_dr2_d2
      end do
!$omp end parallel do
!
      end subroutine add_sph_ICB_w_diffuse_by_vtor
!
! -----------------------------------------------------------------------
!
      subroutine add_sph_ICB_w_diffuse_by_vpol                          &
     &         (nri, jmax, ar_1d_rj, g_sph_rj, kr_in, Vp_ICB,           &
     &          d2nod_mat_fdm_2, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB,     &
     &          coef_d, n_point, d_rj_wt_diffuse)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: ar_1d_rj(nri,3)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: coef_d
!
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: Vp_ICB(jmax)
!
      real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: d_rj_wt_diffuse(n_point)
!
      integer(kind = kint) :: inod, j, k2, k3, i_p1, i_p2
      real(kind = kreal) :: d2s_dr2_w1, d2s_dr2_w2
      real(kind = kreal) :: d2t_dr2_d1, d2t_dr2_d2, d2t_dr2_d3
!
!
      k2 = kr_in + 1
      k3 = kr_in + 2
!$omp parallel do private(j,inod,i_p1,i_p2,d2s_dr2_w1,d2s_dr2_w2,       &
!$omp&                    d2t_dr2_d1,d2t_dr2_d2,d2t_dr2_d3)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d2s_dr2_w1 =  fdm2_fix_dr_ICB( 0,3) * Vp_ICB(j)                 &
     &            - g_sph_rj(j,3)*ar_1d_rj(kr_in,2)*Vp_ICB(j)
        d2s_dr2_w2 =  d2nod_mat_fdm_2(k2,-1) * Vp_ICB(j)
!
        d2t_dr2_d1 =  fdm2_fix_fld_ICB( 0,3) * (-d2s_dr2_w1)            &
     &              + fdm2_fix_fld_ICB( 1,3) * (-d2s_dr2_w2)            &
     &            - g_sph_rj(j,3)*ar_1d_rj(kr_in,2) * (-d2s_dr2_w1)
        d2t_dr2_d2 =  d2nod_mat_fdm_2(k2,-1) * (-d2s_dr2_w1)            &
     &              + d2nod_mat_fdm_2(k2, 0) * (-d2s_dr2_w2)            &
     &          - g_sph_rj(j,3)*ar_1d_rj(k2,2)*(-d2s_dr2_w2)
        d2t_dr2_d3 =  d2nod_mat_fdm_2(k3,-1) * (-d2s_dr2_w2)
!
        d_rj_wt_diffuse(inod) = d_rj_wt_diffuse(inod)                   &
     &                         + coef_d * d2t_dr2_d1
        d_rj_wt_diffuse(i_p1) = d_rj_wt_diffuse(i_p1)                   &
     &                         + coef_d * d2t_dr2_d2
        d_rj_wt_diffuse(i_p2) = d_rj_wt_diffuse(i_p2)                   &
     &                         + coef_d * d2t_dr2_d3
      end do
!$omp end parallel do
!
      end subroutine add_sph_ICB_w_diffuse_by_vpol
!
! -----------------------------------------------------------------------
!
      subroutine add_sph_ICB_w_diffuse_by_dpdr                          &
     &         (nri, jmax, ar_1d_rj, g_sph_rj, kr_in, Vd_ICB,           &
     &          d2nod_mat_fdm_2, fdm2_fix_fld_ICB, fdm2_fix_dr_ICB,     &
     &          coef_d, n_point, d_rj_wt_diffuse)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: ar_1d_rj(nri,3)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: coef_d
!
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: Vd_ICB(jmax)
!
      real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_ICB(0:2,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: d_rj_wt_diffuse(n_point)
!
      integer(kind = kint) :: inod, j, k2, i_p1, i_p2
      real(kind = kreal) :: d2s_dr2_w1, d2t_dr2_d2, d2t_dr2_d1
!
!
      k2 = kr_in + 1
!$omp parallel do                                                       &
!$omp& private(j,inod,i_p1,i_p2,d2s_dr2_w1,d2t_dr2_d1,d2t_dr2_d2)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d2s_dr2_w1 =  fdm2_fix_dr_ICB(-1,3) * Vd_ICB(j)
        d2t_dr2_d1 =  fdm2_fix_fld_ICB( 0,3) * (-d2s_dr2_w1)            &
     &            - g_sph_rj(j,3)*ar_1d_rj(kr_in,2) * (-d2s_dr2_w1)
        d2t_dr2_d2 =  d2nod_mat_fdm_2(k2,-1) * (-d2s_dr2_w1)
!
        d_rj_wt_diffuse(inod) = d_rj_wt_diffuse(inod)                   &
     &                         + coef_d * d2t_dr2_d1
        d_rj_wt_diffuse(i_p1) = d_rj_wt_diffuse(i_p1)                   &
     &                         + coef_d * d2t_dr2_d2
      end do
!$omp end parallel do
!
      end subroutine add_sph_ICB_w_diffuse_by_dpdr
!
! -----------------------------------------------------------------------
!
      end module add_sph_ICB_w_diffuse
