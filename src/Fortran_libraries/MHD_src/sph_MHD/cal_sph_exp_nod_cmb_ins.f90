!>@file   cal_sph_exp_nod_cmb_ins.f90
!!@brief  module cal_sph_exp_nod_cmb_ins
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!!@date Modified in   Nov., 2013
!
!>@brief  Set insulated magnetic boundary condition for CMB
!!
!!@verbatim
!!      subroutine cal_sph_nod_cmb_ins_b_and_j(jmax, kr_out, r_CMB,     &
!!     &          fdm2_fix_fld_CMB, fdm2_fix_dr_CMB, is_fld, is_rot)
!!      subroutine cal_sph_nod_cmb_ins_mag2(jmax, kr_out, r_CMB, is_fld)
!!
!!      subroutine cal_sph_nod_cmb_ins_vp_rot2(jmax, kr_out, r_CMB,     &
!!     &          is_fld, is_rot)
!!      subroutine cal_sph_nod_cmb_ins_rot2(jmax, kr_out, r_CMB,        &
!!     &          fdm2_fix_fld_CMB, fdm2_fix_dr_CMB, is_fld, is_rot)
!!      subroutine cal_sph_nod_cmb_ins_diffuse2(jmax, kr_out, r_CMB,    &
!!     &          fdm2_fix_fld_CMB, fdm2_fix_dr_CMB, coef_d,            &
!!     &          is_fld, is_diffuse)
!!@endverbatim
!!
!!@n @param jmax  Number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param kr_out       Radial ID for outer boundary
!!@n @param r_CMB(0:2)   Radius at CMB
!!@n @param fdm2_fix_fld_CMB(0:2,3)
!!         Matrix to evaluate radial derivative at CMB with fixed field
!!@n @param fdm2_fix_dr_CMB(-1:1,3)
!!         Matrix to evaluate field at CMB with fixed radial derivative
!!
!!@n @param coef_d       Coefficient for diffusion term
!!@n @param is_fld       Field address of input field
!!@n @param is_rot       Field address for curl of field
!!@n @param is_diffuse   Field address for diffusion of field
!
      module cal_sph_exp_nod_cmb_ins
!
      use m_precision
!
      use m_constants
      use m_schmidt_poly_on_rtm
      use m_sph_spectr_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_ins_b_and_j(jmax, kr_out, r_CMB,       &
     &          fdm2_fix_fld_CMB, fdm2_fix_dr_CMB, is_fld, is_rot)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(0:2,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      real(kind = kreal) :: d1s_dr1,d2s_dr2, d1t_dr1
      integer(kind = kint) :: j, inod, i_n1, i_n2
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d1s_dr1,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d1s_dr1 = - g_sph_rj(j,1) * r_CMB(1) * d_rj(inod,is_fld)
        d2s_dr2 =  fdm2_fix_dr_CMB(-1,3) * d_rj(i_n1,is_fld  )          &
     &          + (fdm2_fix_dr_CMB( 0,3)                                &
     &           -  g_sph_rj(j,1)*r_CMB(1)*fdm2_fix_dr_CMB( 1,3))       &
     &                                   * d_rj(inod,is_fld  )
        d1t_dr1 =  fdm2_fix_fld_CMB(2,2) * d_rj(i_n2,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(1,2) * d_rj(i_n1,is_fld+2)
!
        d_rj(inod,is_fld+1) = d1s_dr1
        d_rj(inod,is_fld+2) = zero
        d_rj(inod,is_rot  ) = zero
        d_rj(inod,is_rot+1) = d1t_dr1
        d_rj(inod,is_rot+2) = - ( d2s_dr2 - g_sph_rj(j,3)               &
     &                   * r_CMB(2)*d_rj(inod,is_fld  ) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_ins_b_and_j
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_ins_mag2(jmax, kr_out, r_CMB, is_fld)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld
      real(kind = kreal), intent(in) :: r_CMB(0:2)
!
      real(kind = kreal) :: d1s_dr1
      integer(kind = kint) :: j, inod
!
!
!$omp parallel do private(inod,d1s_dr1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        d1s_dr1 = - g_sph_rj(j,1) * r_CMB(1)  * d_rj(inod,is_fld)
!
        d_rj(inod,is_fld+1) = d1s_dr1
        d_rj(inod,is_fld+2) = zero
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_ins_mag2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_ins_vp_rot2(jmax, kr_out, r_CMB,       &
     &          is_fld, is_rot)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: r_CMB(0:2)
!
      real(kind = kreal) :: d1t_dr1
      integer(kind = kint) :: j, inod
!
!
!$omp parallel do private(inod,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        d1t_dr1 = - g_sph_rj(j,1) * r_CMB(1) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_rot  ) = d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) = d1t_dr1
        d_rj(inod,is_rot+2) = zero
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_ins_vp_rot2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_ins_rot2(jmax, kr_out, r_CMB,          &
     &          fdm2_fix_fld_CMB, fdm2_fix_dr_CMB, is_fld, is_rot)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_rot
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(0:2,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      real(kind = kreal) :: d2s_dr2, d1t_dr1
      integer(kind = kint) :: j, inod, i_n1, i_n2
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d2s_dr2,d1t_dr1)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d2s_dr2 =  fdm2_fix_dr_CMB(-1,3) * d_rj(i_n1,is_fld  )          &
     &          + (fdm2_fix_dr_CMB( 0,3)                                &
     &           -  g_sph_rj(j,1)*r_CMB(1)*fdm2_fix_dr_CMB( 1,3))       &
     &                                   * d_rj(inod,is_fld  )
        d1t_dr1 =  fdm2_fix_fld_CMB(2,2) * d_rj(i_n2,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(1,2) * d_rj(i_n1,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(0,2) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_rot) = d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) = d1t_dr1
        d_rj(inod,is_rot+2) = - ( d2s_dr2 - g_sph_rj(j,3)               &
     &                   * r_CMB(2)*d_rj(inod,is_fld  ) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_ins_rot2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_ins_diffuse2(jmax, kr_out, r_CMB,      &
     &          fdm2_fix_fld_CMB, fdm2_fix_dr_CMB, coef_d,              &
     &          is_fld, is_diffuse)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_fld_CMB(0:2,3)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      real(kind = kreal) :: d2s_dr2, d2t_dr2
      integer(kind = kint) :: j, inod, i_n1, i_n2
!
!
!$omp parallel do private(inod,i_n1,i_n2,d2s_dr2,d2t_dr2)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d2s_dr2 =  fdm2_fix_dr_CMB(-1,3) * d_rj(i_n1,is_fld  )          &
     &          + (fdm2_fix_dr_CMB( 0,3)                                &
     &           -  g_sph_rj(j,1)*r_CMB(1)*fdm2_fix_dr_CMB( 1,3))       &
     &                                   * d_rj(inod,is_fld  )
        d2t_dr2 =  fdm2_fix_fld_CMB(2,3) * d_rj(i_n2,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(1,3) * d_rj(i_n1,is_fld+2)          &
     &           + fdm2_fix_fld_CMB(0,3) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_diffuse  ) = coef_d * (d2s_dr2                     &
     &    - g_sph_rj(j,3)*r_CMB(2)*d_rj(inod,is_fld  ) )
        d_rj(inod,is_diffuse+2) = coef_d * (d2t_dr2                     &
     &    - g_sph_rj(j,3)*r_CMB(2)*d_rj(inod,is_fld+2) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_ins_diffuse2
!
! -----------------------------------------------------------------------
!
      end module cal_sph_exp_nod_cmb_ins
