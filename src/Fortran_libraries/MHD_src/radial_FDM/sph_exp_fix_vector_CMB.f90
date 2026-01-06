!>@file   sph_exp_fix_vector_CMB.f90
!!@brief  module sph_exp_fix_vector_CMB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate velocity with non-slip boundary at CMB
!!
!!@verbatim
!!      subroutine cal_sph_nod_cmb_rigid_vect(jmax, kr_out,             &
!!     &          Vp_CMB, dV_CMB, Vt_CMB, n_point, d_rj_fld)
!!        integer(kind = kint), intent(in) :: jmax, kr_out
!!        real(kind = kreal), intent(in) :: Vp_CMB(jmax)
!!        real(kind = kreal), intent(in) :: dV_CMB(jmax)
!!        real(kind = kreal), intent(in) :: Vt_CMB(jmax)
!!        integer(kind = kint), intent(in) :: n_point
!!        real (kind=kreal), intent(inout) :: d_rj_fld(n_point,3)
!!
!!      subroutine cal_sph_nod_cmb_fixed_rot2(jmax, g_sph_rj,           &
!!     &          kr_out, r_CMB, fdm2_fix_fld_CMB, fdm2_fix_dr_CMB,     &
!!     &          n_point, d_rj_fld, d_rj_rot)
!!        integer(kind = kint), intent(in) :: jmax, kr_out
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: r_CMB(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
!!        real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_rot(n_point,3)
!!
!!      subroutine cal_sph_nod_cmb_fixed_diffuse2(jmax, g_sph_rj,       &
!!     &          kr_out, r_CMB, fdm2_fix_fld_CMB, fdm2_fix_dr_CMB,     &
!!     &          coef_d, n_point, d_rj_vect, d_rj_diffuse)
!!        integer(kind = kint), intent(in) :: jmax, kr_out
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in) :: r_CMB(0:2)
!!        real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
!!        real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: d_rj_vect(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_diffuse(n_point,3)
!!@endverbatim
!!
!!@n @param n_point  Number of points for spectrum data
!!@n @param jmax  Number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param kr_out       Radial ID for outer boundary
!!@n @param r_CMB(0:2)   Radius at CMB
!!
!!@n @param fdm2_fix_fld_CMB(-2:0,3)
!!         Matrix to evaluate radial derivative at CMB with fixed field
!!@n @param fdm2_fix_dr_CMB(-1:1,3)
!!         Matrix to evaluate field at CMB with fixed radial derivative
!!
!!@n @param coef_d     Coefficient for diffusion term
!!@n @param is_fld     Address of poloidal velocity in d_rj
!!@n @param is_rot     Address of poloidal vorticity in d_rj
!!@n @param is_diffuse Address of poloidal viscousity in d_rj
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module sph_exp_fix_vector_CMB
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
      subroutine cal_sph_nod_cmb_rigid_vect(jmax, kr_out,               &
     &          Vp_CMB, dV_CMB, Vt_CMB, n_point, d_rj_fld)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      real(kind = kreal), intent(in) :: Vp_CMB(jmax)
      real(kind = kreal), intent(in) :: dV_CMB(jmax)
      real(kind = kreal), intent(in) :: Vt_CMB(jmax)
!
      integer(kind = kint), intent(in) :: n_point
      real (kind=kreal), intent(inout) :: d_rj_fld(n_point,3)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
!
        d_rj_fld(inod,1) = Vp_CMB(j)
        d_rj_fld(inod,2) = dV_CMB(j)
        d_rj_fld(inod,3) = Vt_CMB(j)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_rigid_vect
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_fixed_rot2(jmax, g_sph_rj,             &
     &          kr_out, r_CMB, fdm2_fix_fld_CMB, fdm2_fix_dr_CMB,       &
     &          n_point, d_rj_fld, d_rj_rot)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: d_rj_fld(n_point,3)
!
      real(kind = kreal), intent(inout) :: d_rj_rot(n_point,3)
!
      integer(kind = kint) :: inod, j, i_n1, i_n2
      real(kind = kreal) :: d2s_dr2, d1t_dr1
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d2s_dr2 =  fdm2_fix_dr_CMB(-1,3) *  d_rj_fld(i_n1,1)            &
     &           + fdm2_fix_dr_CMB( 0,3) *  d_rj_fld(inod,1)            &
     &           + fdm2_fix_dr_CMB( 1,3) *  d_rj_fld(inod,2)
        d1t_dr1 =  fdm2_fix_fld_CMB(-2,2) * d_rj_fld(i_n2,3)            &
     &           + fdm2_fix_fld_CMB(-1,2) * d_rj_fld(i_n1,3)            &
     &           + fdm2_fix_fld_CMB( 0,2) * d_rj_fld(inod,3)
!
        d_rj_rot(inod,1) = d_rj_fld(inod,3)
        d_rj_rot(inod,2) = d1t_dr1
        d_rj_rot(inod,3) = - (d2s_dr2 - g_sph_rj(j,3)                   &
     &                      * r_CMB(2)*d_rj_fld(inod,1))
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_fixed_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_fixed_diffuse2(jmax, g_sph_rj,         &
     &          kr_out, r_CMB, fdm2_fix_fld_CMB, fdm2_fix_dr_CMB,       &
     &          coef_d, n_point, d_rj_vect, d_rj_diffuse)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(-2:0,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: d_rj_vect(n_point,3)
!
      real(kind = kreal), intent(inout) :: d_rj_diffuse(n_point,3)
!
      integer(kind = kint) :: inod, j, i_n1, i_n2
      real(kind = kreal) :: d2s_dr2, d2t_dr2
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d2s_dr2,d2t_dr2)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d2s_dr2 =  fdm2_fix_dr_CMB(-1,3) *  d_rj_vect(i_n1,1)           &
     &           + fdm2_fix_dr_CMB( 0,3) *  d_rj_vect(inod,1)           &
     &           + fdm2_fix_dr_CMB( 1,3) *  d_rj_vect(inod,2)
        d2t_dr2 =  fdm2_fix_fld_CMB(-2,3) * d_rj_vect(i_n2,3)           &
     &           + fdm2_fix_fld_CMB(-1,3) * d_rj_vect(i_n1,3)           &
     &           + fdm2_fix_fld_CMB( 0,3) * d_rj_vect(inod,3)
!
        d_rj_diffuse(inod,1) =  coef_d * (d2s_dr2                       &
     &               - g_sph_rj(j,3)*r_CMB(2) * d_rj_vect(inod,1))
        d_rj_diffuse(inod,3) =  coef_d * (d2t_dr2                       &
     &               - g_sph_rj(j,3)*r_CMB(2) * d_rj_vect(inod,3))
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_fixed_diffuse2
!
! -----------------------------------------------------------------------
!
      end module sph_exp_fix_vector_CMB
