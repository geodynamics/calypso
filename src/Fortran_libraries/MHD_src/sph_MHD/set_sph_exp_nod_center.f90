!>@file   set_sph_exp_nod_center.f90
!!@brief  module set_sph_exp_nod_center
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate field approaching to center
!!
!!@verbatim
!!      subroutine sph_center_fld_and_curl(nri, jmax, a2r_k1,           &
!!     &          g_sph_rj, d1nod_mat_fdm_2, d2nod_mat_fdm_2,           &
!!     &          n_point, d_rj_fld, d_rj_rot)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: a2r_k1
!!        real(kind = kreal), intent(in) :: d1nod_mat_fdm_2(nri,-1:1)
!!        real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: d_rj_fld(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_rot(n_point,3)
!!      subroutine cal_dsdr_sph_center_2(nri, jmax, d1nod_mat_fdm_2,    &
!!     &                                 n_point, d_rj_pol, d_rj_dr)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: d1nod_mat_fdm_2(nri,-1:1)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: d_rj_pol(n_point)
!!        real(kind = kreal), intent(inout) :: d_rj_dr(n_point)
!!      subroutine cal_sph_nod_center_rot2(nri, jmax, a2r_k1,           &
!!     &          g_sph_rj, d1nod_mat_fdm_2, d2nod_mat_fdm_2,           &
!!     &          n_point, d_rj_fld, d_rj_rot)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: a2r_k1
!!        real(kind = kreal), intent(in) :: d1nod_mat_fdm_2(nri,-1:1)
!!        real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_rot(n_point,3)
!!      subroutine cal_sph_nod_center_diffuse2                          &
!!     &         (nri, jmax, a2r_k1, g_sph_rj, d2nod_mat_fdm_2, coef_d, &
!!     &          n_point, d_rj_fld, d_rj_diffuse)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: a2r_k1
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_diffuse(n_point,3)
!!@endverbatim
!
      module set_sph_exp_nod_center
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
      subroutine sph_center_fld_and_curl(nri, jmax, a2r_k1,             &
     &          g_sph_rj, d1nod_mat_fdm_2, d2nod_mat_fdm_2,             &
     &          n_point, d_rj_fld, d_rj_rot)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: a2r_k1
      real(kind = kreal), intent(in) :: d1nod_mat_fdm_2(nri,-1:1)
      real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: d_rj_fld(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_rot(n_point,3)
!
      real(kind = kreal) :: d1s_dr1, d2s_dr2, d1t_dr1
      integer(kind = kint) :: j, inod, i_p1
!
!
!$omp parallel do private(inod,i_p1,d1s_dr1,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j
        i_p1 = inod + jmax
!
        d1s_dr1 =  d1nod_mat_fdm_2(ione,0) * d_rj_fld(inod,1)           &
     &           + d1nod_mat_fdm_2(ione,1) * d_rj_fld(i_p1,1)
        d2s_dr2 =  d2nod_mat_fdm_2(ione,0) * d_rj_fld(inod,1)           &
     &           + d2nod_mat_fdm_2(ione,1) * d_rj_fld(i_p1,1)
        d1t_dr1 =  d1nod_mat_fdm_2(ione,0) * d_rj_fld(inod,3)           &
     &           + d1nod_mat_fdm_2(ione,1) * d_rj_fld(i_p1,3)
!
        d_rj_fld(inod,2) = d1s_dr1
        d_rj_rot(inod,1) = d_rj_fld(inod,3)
        d_rj_rot(inod,2) = d1t_dr1
        d_rj_rot(inod,3) = - (d2s_dr2 - g_sph_rj(j,3)                   &
     &                       * a2r_k1 * d_rj_fld(inod,1))
      end do
!$omp end parallel do
!
      end subroutine sph_center_fld_and_curl
!
! -----------------------------------------------------------------------
!
      subroutine cal_dsdr_sph_center_2(nri, jmax, d1nod_mat_fdm_2,      &
     &                                 n_point, d_rj_pol, d_rj_dr)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: d1nod_mat_fdm_2(nri,-1:1)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: d_rj_pol(n_point)
      real(kind = kreal), intent(inout) :: d_rj_dr(n_point)
!
      integer(kind = kint) :: inod, i_p1
!
!
!$omp parallel do private(inod,i_p1)
      do inod = 1, jmax
        i_p1 = inod + jmax
        d_rj_dr(inod) =  d1nod_mat_fdm_2(ione,0) * d_rj_pol(inod)       &
     &                 + d1nod_mat_fdm_2(ione,1) * d_rj_pol(i_p1)
      end do
!$omp end parallel do
!
      end subroutine cal_dsdr_sph_center_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_center_rot2(nri, jmax, a2r_k1,             &
     &          g_sph_rj, d1nod_mat_fdm_2, d2nod_mat_fdm_2,             &
     &          n_point, d_rj_fld, d_rj_rot)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: a2r_k1
      real(kind = kreal), intent(in) :: d1nod_mat_fdm_2(nri,-1:1)
      real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_rot(n_point,3)
!
      real(kind = kreal) :: d2s_dr2, d1t_dr1
      integer(kind = kint) :: j, inod, i_p1
!
!
!$omp parallel do private(inod,i_p1,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j
        i_p1 = inod + jmax
!
        d2s_dr2 =  d2nod_mat_fdm_2(ione, 0) * d_rj_fld(inod,1)          &
     &           + d2nod_mat_fdm_2(ione, 1) * d_rj_fld(i_p1,1)
        d1t_dr1 =  d1nod_mat_fdm_2(ione, 0) * d_rj_fld(inod,3)          &
     &           + d1nod_mat_fdm_2(ione, 1) * d_rj_fld(i_p1,3)
!
        d_rj_rot(inod,1) = d_rj_fld(inod,3)
        d_rj_rot(inod,2) = d1t_dr1
        d_rj_rot(inod,3) = - (d2s_dr2 - g_sph_rj(j,3)                   &
     &                           * a2r_k1 *d_rj_fld(inod,1))
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_center_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_center_diffuse2                            &
     &         (nri, jmax, a2r_k1, g_sph_rj, d2nod_mat_fdm_2, coef_d,   &
     &          n_point, d_rj_fld, d_rj_diffuse)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: a2r_k1
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: d2nod_mat_fdm_2(nri,-1:1)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_diffuse(n_point,3)
!
      real(kind = kreal) :: d2s_dr2, d2t_dr2
      integer(kind = kint) :: j, inod, i_p1
!
!
!$omp parallel do private(inod,i_p1,d2s_dr2,d2t_dr2)
      do j = 1, jmax
        inod = j
        i_p1 = inod + jmax
!
        d2s_dr2 =  d2nod_mat_fdm_2(ione, 0) * d_rj_fld(inod,1)          &
     &           + d2nod_mat_fdm_2(ione, 1) * d_rj_fld(i_p1,1)
        d2t_dr2 =  d2nod_mat_fdm_2(ione, 0) * d_rj_fld(inod,3)          &
     &           + d2nod_mat_fdm_2(ione, 1) * d_rj_fld(i_p1,3)
!
        d_rj_diffuse(inod,1) = coef_d * (d2s_dr2                        &
     &         - g_sph_rj(j,3) * a2r_k1 * d_rj_fld(inod,1))
        d_rj_diffuse(inod,3) = coef_d * (d2t_dr2                        &
     &         - g_sph_rj(j,3) * a2r_k1 * d_rj_fld(inod,3))
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_center_diffuse2
!
! -----------------------------------------------------------------------
!
      end module set_sph_exp_nod_center
