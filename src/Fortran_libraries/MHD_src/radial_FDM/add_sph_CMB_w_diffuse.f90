!> @file  add_sph_CMB_w_diffuse.f90
!!      module add_sph_CMB_w_diffuse
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2026
!
!> @brief Set boundary condition routines for velocity
!!
!!@verbatim
!!      subroutine add_sph_CMB_w_diffuse_by_vtor                        &
!!     &         (nri, jmax, ar_1d_rj, g_sph_rj, kr_out, Vt_CMB,        &
!!     &          d2nod_mat_fdm_2, fdm2_fix_fld_CMB, fdm2_fix_dr_CMB,   &
!!     &          coef_d, n_point, d_rj_wp_diffuse)
!!      subroutine add_sph_CMB_w_diffuse_by_vpol                        &
!!     &         (nri, jmax, ar_1d_rj, g_sph_rj, kr_out, Vp_CMB,        &
!!     &          d2nod_mat_fdm_2, fdm2_fix_fld_CMB, fdm2_fix_dr_CMB,   &
!!     &          coef_d, n_point, d_rj_wt_diffuse)
!!      subroutine add_sph_CMB_w_diffuse_by_dpdr                        &
!!     &         (nri, jmax, ar_1d_rj, g_sph_rj, kr_out, Vd_CMB,        &
!!     &          d2nod_mat_fdm_2, fdm2_fix_fld_CMB, fdm2_fix_dr_CMB,   &
!!     &          coef_d, n_point, d_rj_wt_diffuse)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: ar_1d_rj(nri,3)
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: coef_d
!!        integer(kind = kint), intent(in) :: kr_out
!!        real(kind = kreal), intent(in) :: Vt_CMB(jmax)
!!        real(kind = kreal), intent(in) :: Vp_CMB(jmax)
!!        real(kind = kreal), intent(in) :: Vd_CMB(jmax)
!!        real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
!!        real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: d_rj_wp_diffuse(n_point)
!!        real(kind = kreal), intent(inout) :: d_rj_wt_diffuse(n_point)
!!@endverbatim
!!
      module add_sph_CMB_w_diffuse
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
      subroutine add_sph_CMB_w_diffuse_by_vtor                          &
     &         (nri, jmax, ar_1d_rj, g_sph_rj, kr_out, Vt_CMB,          &
     &          d2nod_mat_fdm_2, fdm2_fix_fld_CMB, fdm2_fix_dr_CMB,     &
     &          coef_d, n_point, d_rj_wp_diffuse)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: ar_1d_rj(nri,3)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: coef_d
!
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: Vt_CMB(jmax)
!
      real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: d_rj_wp_diffuse(n_point)
!
      integer(kind = kint) :: inod, j, k2, i_n1
      real(kind = kreal) :: d1t_dr1_w1, d2s_dr2_d2, d2s_dr2_d1
!
!
      k2 = kr_out - 1
!$omp parallel do                                                       &
!$omp& private(j,inod,i_n1,d1t_dr1_w1,d2s_dr2_d2,d2s_dr2_d1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
!
        d1t_dr1_w1 =  fdm2_fix_fld_CMB( 0,2) * Vt_CMB(j)
!
        d2s_dr2_d2 =  d2nod_mat_fdm_2(k2, 1) * Vt_CMB(j)
        d2s_dr2_d1 =  fdm2_fix_dr_CMB( 0,3) *  Vt_CMB(j)                &
     &              + fdm2_fix_dr_CMB( 1,3) *  d1t_dr1_w1               &
     &            - g_sph_rj(j,3)*ar_1d_rj(kr_out,2) * Vt_CMB(j)
!
        d_rj_wp_diffuse(i_n1)                                           &
     &             = d_rj_wp_diffuse(i_n1) + coef_d * d2s_dr2_d2
        d_rj_wp_diffuse(inod)                                           &
     &             = d_rj_wp_diffuse(inod) + coef_d * d2s_dr2_d1
      end do
!$omp end parallel do
!
      end subroutine add_sph_CMB_w_diffuse_by_vtor
!
! -----------------------------------------------------------------------
!
      subroutine add_sph_CMB_w_diffuse_by_vpol                          &
     &         (nri, jmax, ar_1d_rj, g_sph_rj, kr_out, Vp_CMB,          &
     &          d2nod_mat_fdm_2, fdm2_fix_fld_CMB, fdm2_fix_dr_CMB,     &
     &          coef_d, n_point, d_rj_wt_diffuse)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: ar_1d_rj(nri,3)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: coef_d
!
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: Vp_CMB(jmax)
!
      real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: d_rj_wt_diffuse(n_point)
!
      integer(kind = kint) :: inod, j, k2, k3, i_n1, i_n2
      real(kind = kreal) :: d2s_dr2_w2, d2s_dr2_w1
      real(kind = kreal) :: d2t_dr2_d3, d2t_dr2_d2, d2t_dr2_d1
!
!
      k2 = kr_out - 1
      k3 = kr_out - 2
!$omp parallel do private(j,inod,i_n1,i_n2,d2s_dr2_w2,d2s_dr2_w1,       &
!$omp&                    d2t_dr2_d3,d2t_dr2_d2,d2t_dr2_d1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d2s_dr2_w2 =  d2nod_mat_fdm_2(k2, 1) * Vp_CMB(j)
        d2s_dr2_w1 =  fdm2_fix_dr_CMB( 0,3) *  Vp_CMB(j)                &
     &            - g_sph_rj(j,3) * ar_1d_rj(kr_out,2) * Vp_CMB(j)
!
        d2t_dr2_d3 =  d2nod_mat_fdm_2(k3, 1) * (-d2s_dr2_w2)
!
        d2t_dr2_d2 =  d2nod_mat_fdm_2(k2, 0) * (-d2s_dr2_w2)            &
     &              + d2nod_mat_fdm_2(k2, 1) * (-d2s_dr2_w1)            &
     &            - g_sph_rj(j,3) * ar_1d_rj(k2,2) * (-d2s_dr2_w2)
!
        d2t_dr2_d1 =  fdm2_fix_fld_CMB(-1,3) * (-d2s_dr2_w2)            &
     &              + fdm2_fix_fld_CMB( 0,3) * (-d2s_dr2_w1)            &
     &            - g_sph_rj(j,3) * ar_1d_rj(kr_out,2) * (-d2s_dr2_w1)
!
        d_rj_wt_diffuse(i_n2) = d_rj_wt_diffuse(i_n2)                   &
     &                         + coef_d * d2t_dr2_d3
        d_rj_wt_diffuse(i_n1) = d_rj_wt_diffuse(i_n1)                   &
     &                         + coef_d * d2t_dr2_d2
        d_rj_wt_diffuse(inod) = d_rj_wt_diffuse(inod)                   &
     &                         + coef_d * d2t_dr2_d1
      end do
!$omp end parallel do
!
      end subroutine add_sph_CMB_w_diffuse_by_vpol
!
! -----------------------------------------------------------------------
!
      subroutine add_sph_CMB_w_diffuse_by_dpdr                          &
     &         (nri, jmax, ar_1d_rj, g_sph_rj, kr_out, Vd_CMB,          &
     &          d2nod_mat_fdm_2, fdm2_fix_fld_CMB, fdm2_fix_dr_CMB,     &
     &          coef_d, n_point, d_rj_wt_diffuse)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: ar_1d_rj(nri,3)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: coef_d
!
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: Vd_CMB(jmax)
!
      real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: d_rj_wt_diffuse(n_point)
!
      integer(kind = kint) :: inod, j, k2, i_n1, i_n2
      real(kind = kreal) :: d2s_dr2_w1, d2t_dr2_d2, d2t_dr2_d1
!
!
      k2 = kr_out - 1
!$omp parallel do                                                       &
!$omp& private(j,inod,i_n1,i_n2,d2s_dr2_w1,d2t_dr2_d2,d2t_dr2_d1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d2s_dr2_w1 =  fdm2_fix_dr_CMB( 1,3) *  Vd_CMB(j)
!
        d2t_dr2_d2 =  d2nod_mat_fdm_2(k2, 1) * (-d2s_dr2_w1)
        d2t_dr2_d1 =  fdm2_fix_fld_CMB( 0,3) * (-d2s_dr2_w1)            &
     &              - g_sph_rj(j,3)*ar_1d_rj(kr_out,2) * (-d2s_dr2_w1)
!
        d_rj_wt_diffuse(i_n1) = d_rj_wt_diffuse(i_n1)                   &
     &                         + coef_d * d2t_dr2_d2
        d_rj_wt_diffuse(inod) = d_rj_wt_diffuse(inod)                   &
     &                         + coef_d * d2t_dr2_d1
      end do
!$omp end parallel do


      end subroutine add_sph_CMB_w_diffuse_by_dpdr
!
! -----------------------------------------------------------------------
!
      end module add_sph_CMB_w_diffuse
