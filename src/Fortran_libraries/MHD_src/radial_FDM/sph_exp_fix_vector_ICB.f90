!>@file   sph_exp_fix_vector_ICB.f90
!!@brief  module sph_exp_fix_vector_ICB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate velocity with non-slip boundary at ICB
!!
!!@verbatim
!!      subroutine cal_sph_nod_icb_rigid_vect(nidx_rj, idx_rj,          &
!!     &          radius_rj, kr_in, r_ICB, Vp_ICB, Vd_ICB, Vt_ICB,      &
!!     &          n_point, d_rj_fld)
!!        integer(kind = kint), intent(in) :: nidx_rj(2)
!!        integer(kind = kint), intent(in) :: idx_rj(nidx_rj(2),3)
!!        integer(kind = kint), intent(in) :: kr_in
!!        real(kind = kreal), intent(in) :: r_ICB(0:2)
!!        real(kind = kreal), intent(in) :: radius_rj(nidx_rj(1))
!!        real(kind = kreal), intent(in) :: Vp_ICB(nidx_rj(2))
!!        real(kind = kreal), intent(in) :: Vd_ICB(nidx_rj(2))
!!        real(kind = kreal), intent(in) :: Vt_ICB(nidx_rj(2))
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: d_rj_fld(n_point,3)
!!      subroutine set_sph_filter_vect_to_center(nidx_rj, Vp_ICB,       &
!!     &                                         n_point, d_rj_fld)
!!        integer(kind = kint), intent(in) :: nidx_rj(2)
!!        real(kind = kreal), intent(in) :: Vp_ICB(nidx_rj(2))
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: d_rj_fld(n_point,3)
!!
!!      subroutine cal_sph_nod_icb_fixed_rot2(jmax, g_sph_rj,           &
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
!!      subroutine cal_sph_nod_icb_fixed_diffuse2(jmax, g_sph_rj,       &
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
!!@n @param idx_rj_degree_zero    Local address for degree 0
!!@n @param idx_rj_degree_one(-1:1)    Local address for degree 1
!!@n @param jmax  Number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param r_ICB(0:2)   Radius at ICB
!!@n @param Vt_ICB(jmax) Spectr data for toroidal velocity ICB
!!
!!@n @param fdm2_fix_fld_ICB(0:2,3)
!!         Matrix to evaluate radial derivative at ICB with fixed field
!!@n @param fdm2_fix_dr_ICB(-1:1,3)
!!         Matrix to evaluate field at ICB with fixed radial derivative
!!
!!@n @param coef_d     Coefficient for diffusion term
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module sph_exp_fix_vector_ICB
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
      subroutine cal_sph_nod_icb_rigid_vect(nidx_rj, idx_rj,            &
     &          radius_rj, kr_in, r_ICB, Vp_ICB, Vd_ICB, Vt_ICB,        &
     &          n_point, d_rj_fld)
!
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: idx_rj(nidx_rj(2),3)
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: radius_rj(nidx_rj(1))
      real(kind = kreal), intent(in) :: Vp_ICB(nidx_rj(2))
      real(kind = kreal), intent(in) :: Vd_ICB(nidx_rj(2))
      real(kind = kreal), intent(in) :: Vt_ICB(nidx_rj(2))
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: d_rj_fld(n_point,3)
!
      integer(kind = kint) :: inod, j, k, l
!
!
!$omp parallel do private(k,j,l,inod)
      do j = 1, nidx_rj(2)
        l = idx_rj(j,2)
        do k = 1, kr_in
          inod = j + (k-1) * nidx_rj(2)
!
          d_rj_fld(inod,1) = Vp_ICB(j) * (radius_rj(k)*r_ICB(1))**(l+1)
          d_rj_fld(inod,2) = Vd_ICB(j) * dble(l+1)                      &
    &                                   * (radius_rj(k)*r_ICB(1))**(l)
          d_rj_fld(inod,3) = Vt_ICB(j) * (radius_rj(k)*r_ICB(1))**(l+1)
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_icb_rigid_vect
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_filter_vect_to_center(nidx_rj, Vp_ICB,         &
     &                                         n_point, d_rj_fld)
!
      integer(kind = kint), intent(in) :: nidx_rj(2)
      real(kind = kreal), intent(in) :: Vp_ICB(nidx_rj(2))
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: d_rj_fld(n_point,3)
!
      integer(kind = kint) :: inod, j, k
!
!
!$omp parallel do private(k,j,inod)
      do k = 1, int(Vp_ICB(nidx_rj(2)))
        do j = 1, nidx_rj(2)
          if(k .gt. int(Vp_ICB(j))) cycle
!
          inod = j + (k-1) * nidx_rj(2)
          d_rj_fld(inod,1) = zero
          d_rj_fld(inod,3) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine set_sph_filter_vect_to_center
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_icb_fixed_rot2(jmax, g_sph_rj,             &
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
        d2s_dr2 =  fdm2_fix_dr_ICB(-1,3) * d_rj_fld(inod,2)             &
     &           + fdm2_fix_dr_ICB( 0,3) * d_rj_fld(inod,1)             &
     &           + fdm2_fix_dr_ICB( 1,3) * d_rj_fld(i_p1,1)
        d1t_dr1 =  fdm2_fix_fld_ICB( 0,2) * d_rj_fld(inod,3)            &
     &           + fdm2_fix_fld_ICB( 1,2) * d_rj_fld(i_p1,3)            &
     &           + fdm2_fix_fld_ICB( 2,2) * d_rj_fld(i_p2,3)
!
        d_rj_rot(inod,1) = d_rj_fld(inod,3)
        d_rj_rot(inod,2) = d1t_dr1
        d_rj_rot(inod,3) = - (d2s_dr2                                   &
     &                      - g_sph_rj(j,3)*r_ICB(2)*d_rj_fld(inod,1))
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_icb_fixed_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_icb_fixed_diffuse2(jmax, g_sph_rj,         &
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
      integer(kind = kint) :: inod, j, i_p1, i_p2
      real(kind = kreal) :: d2s_dr2, d2t_dr2
!
!
!$omp parallel do private(inod,i_p1,i_p2,j,d2s_dr2,d2t_dr2)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d2s_dr2 =  fdm2_fix_dr_ICB(-1,3) * d_rj_fld(inod,2)             &
     &           + fdm2_fix_dr_ICB( 0,3) * d_rj_fld(inod,1)             &
     &           + fdm2_fix_dr_ICB( 1,3) * d_rj_fld(i_p1,1)
        d2t_dr2 =  fdm2_fix_fld_ICB( 0,3) * d_rj_fld(inod,3)            &
     &           + fdm2_fix_fld_ICB( 1,3) * d_rj_fld(i_p1,3)            &
     &           + fdm2_fix_fld_ICB( 2,3) * d_rj_fld(i_p2,3)
!
        d_rj_diffuse(inod,1) = coef_d * (d2s_dr2                        &
     &              - g_sph_rj(j,3)*r_ICB(2) * d_rj_fld(inod,1))
        d_rj_diffuse(inod,3) = coef_d * (d2t_dr2                        &
     &              - g_sph_rj(j,3)*r_ICB(2) * d_rj_fld(inod,3))
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_icb_fixed_diffuse2
!
! -----------------------------------------------------------------------
!
      end module sph_exp_fix_vector_ICB
