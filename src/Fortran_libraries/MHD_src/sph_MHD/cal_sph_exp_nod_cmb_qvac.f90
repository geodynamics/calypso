!>@file   cal_sph_exp_nod_cmb_qvac.f90
!!@brief  module cal_sph_exp_nod_cmb_qvac
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Set pseudo vacuum magnetic boundary condition for CMB
!!
!!@verbatim
!!      subroutine cal_sph_nod_cmb_qvc_b_and_j(is_fld, is_rot)
!!      subroutine cal_sph_nod_cmb_qvc_mag2(is_fld)
!!
!!      subroutine cal_sph_nod_cmb_qvc_vp_rot2(is_fld, is_rot)
!!      subroutine cal_sph_nod_cmb_qvc_rot2(is_fld, is_rot)
!!      subroutine cal_sph_nod_cmb_qvc_diffuse2(coef_d,                 &
!!     &          is_fld, is_diffuse)
!!@endverbatim
!!
!!@n @param coef_d        Coefficient for diffusion term
!!@n @param is_fld       Field address of input field
!!@n @param is_rot       Field address for curl of field
!!@n @param is_diffuse   Field address for diffusion of field
!
      module cal_sph_exp_nod_cmb_qvac
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_sph_spectr_data
      use m_fdm_coefs
!
      implicit none
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_qvc_b_and_j(is_fld, is_rot)
!
      use m_coef_fdm_fixed_CMB
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_rot
!
      real(kind = kreal) :: d2s_dr2, d1t_dr1
      integer(kind = kint) :: j, inod, i_n1, i_n2
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d2s_dr2,d1t_dr1)
      do j = 1, nidx_rj(2)
        inod = j + (nlayer_CMB-1) * nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        i_n2 = i_n1 - nidx_rj(2)
!
        d2s_dr2 =  coef_fdm_fix_dr_CMB_2(-1,3) * d_rj(i_n1,is_fld  )    &
     &          +  coef_fdm_fix_dr_CMB_2( 0,3) * d_rj(inod,is_fld  )
        d1t_dr1 =  coef_fdm_fix_CMB_2(2,2) * d_rj(i_n2,is_fld+2)        &
     &           + coef_fdm_fix_CMB_2(1,2) * d_rj(i_n1,is_fld+2)
!
        d_rj(inod,is_fld+1) = zero
        d_rj(inod,is_fld+2) = zero
        d_rj(inod,is_rot  ) = zero
        d_rj(inod,is_rot+1) = d1t_dr1
        d_rj(inod,is_rot+2) = - ( d2s_dr2 - g_sph_rj(j,3)               &
     &                   * ar_1d_rj(nlayer_CMB,2)*d_rj(inod,is_fld  ) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_qvc_b_and_j
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_qvc_mag2(is_fld)
!
      integer(kind = kint), intent(in) :: is_fld
!
      integer(kind = kint) :: j, inod
!
!
!$omp parallel do private(inod)
      do j = 1, nidx_rj(2)
        inod = j + (nlayer_CMB-1) * nidx_rj(2)
!
        d_rj(inod,is_fld+1) = zero
        d_rj(inod,is_fld+2) = zero
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_qvc_mag2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_qvc_vp_rot2(is_fld, is_rot)
!
      integer(kind = kint), intent(in) :: is_fld, is_rot
!
      integer(kind = kint) :: j, inod
!
!
!$omp parallel do private(inod)
      do j = 1, nidx_rj(2)
        inod = j + (nlayer_CMB-1) * nidx_rj(2)
!
        d_rj(inod,is_rot  ) = d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) = zero
        d_rj(inod,is_rot+2) = zero
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_qvc_vp_rot2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_qvc_rot2(is_fld, is_rot)
!
      use m_coef_fdm_fixed_CMB
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_rot
!
      real(kind = kreal) :: d2s_dr2, d1t_dr1
      integer(kind = kint) :: j, inod, i_n1, i_n2
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d2s_dr2,d1t_dr1)
      do j = 1, nidx_rj(2)
        inod = j + (nlayer_CMB-1) * nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        i_n2 = i_n1 - nidx_rj(2)
!
        d2s_dr2 =  coef_fdm_fix_dr_CMB_2(-1,3) * d_rj(i_n1,is_fld  )    &
     &          +  coef_fdm_fix_dr_CMB_2( 0,3) * d_rj(inod,is_fld  )
        d1t_dr1 =  coef_fdm_fix_CMB_2(2,2) * d_rj(i_n2,is_fld+2)        &
     &           + coef_fdm_fix_CMB_2(1,2) * d_rj(i_n1,is_fld+2)        &
     &           + coef_fdm_fix_CMB_2(0,2) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_rot) = d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) = d1t_dr1
        d_rj(inod,is_rot+2) = - ( d2s_dr2 - g_sph_rj(j,3)               &
     &                   * ar_1d_rj(nlayer_CMB,2)*d_rj(inod,is_fld  ) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_qvc_rot2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_qvc_diffuse2(coef_d,                   &
     &          is_fld, is_diffuse)
!
      use m_coef_fdm_fixed_CMB
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_diffuse
      real(kind = kreal), intent(in) :: coef_d
!
      real(kind = kreal) :: d2s_dr2, d2t_dr2
      integer(kind = kint) :: j, inod, i_n1, i_n2
!
!
!$omp parallel do private(inod,i_n1,i_n2,d2s_dr2,d2t_dr2)
      do j = 1, nidx_rj(2)
        inod = j + (nlayer_CMB-1) * nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        i_n2 = i_n1 - nidx_rj(2)
!
        d2s_dr2 =  coef_fdm_fix_dr_CMB_2(-1,3) * d_rj(i_n1,is_fld  )    &
     &          +  coef_fdm_fix_dr_CMB_2( 0,3) * d_rj(inod,is_fld  )
        d2t_dr2 =  coef_fdm_fix_CMB_2(2,3) * d_rj(i_n2,is_fld+2)        &
     &           + coef_fdm_fix_CMB_2(1,3) * d_rj(i_n1,is_fld+2)        &
     &           + coef_fdm_fix_CMB_2(0,3) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_diffuse  ) = coef_d * (d2s_dr2                     &
     &    - g_sph_rj(j,3)*ar_1d_rj(nlayer_CMB,2)*d_rj(inod,is_fld  ) )
        d_rj(inod,is_diffuse+2) = coef_d * (d2t_dr2                     &
     &    - g_sph_rj(j,3)*ar_1d_rj(nlayer_CMB,2)*d_rj(inod,is_fld+2) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_qvc_diffuse2
!
! -----------------------------------------------------------------------
!
      end module cal_sph_exp_nod_cmb_qvac
