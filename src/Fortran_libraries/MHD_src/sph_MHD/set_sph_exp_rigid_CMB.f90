!>@file   set_sph_exp_rigid_CMB.f90
!!@brief  module set_sph_exp_rigid_CMB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate velocity with non-slip boundary at CMB
!!
!!@verbatim
!!      subroutine cal_sph_nod_cmb_rigid_v_and_w(is_fld, is_rot)
!!      subroutine cal_sph_nod_cmb_rigid_velo2(is_fld)
!!      subroutine cal_sph_nod_cmb_rigid_rot2(is_fld, is_rot)
!!      subroutine cal_sph_nod_cmb_rigid_diffuse2(coef_d,               &
!!     &          is_fld, is_diffuse)
!!      subroutine cal_sph_nod_cmb_rgd_w_diffuse2(coef_d,               &
!!     &          is_fld, is_diffuse)
!!@endverbatim
!!
!!@n @param coef_d     Coefficient for diffusion term
!!@n @param is_fld     Address of poloidal velocity in d_rj
!!@n @param is_rot     Address of poloidal vorticity in d_rj
!!@n @param is_diffuse Address of poloidal viscousity in d_rj
!
      module set_sph_exp_rigid_CMB
!
      use m_precision
!
      use m_constants
      use m_control_params_sph_MHD
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_sph_spectr_data
      use m_coef_fdm_fixed_CMB
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_rigid_v_and_w(is_fld, is_rot)
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_rot
!
      integer(kind = kint) :: inod, j, i_n1, i_n2
      real(kind = kreal) :: d2s_dr2, d1t_dr1
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d2s_dr2,d1t_dr1)
      do j = 1, nidx_rj(2)
        inod = j + (nlayer_CMB-1) * nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        i_n2 = i_n1 - nidx_rj(2)
!
        d_rj(inod,is_fld  ) = zero
        d_rj(inod,is_fld+1) = zero
        d_rj(inod,is_fld+2) = vt_CMB_bc(j)*ar_1d_rj(nlayer_CMB,1)
!
        d2s_dr2 =  coef_fdm_fix_dr_CMB_2(-1,3) * d_rj(i_n1,is_fld  )
        d1t_dr1 =  coef_fdm_fix_CMB_2(2,2) * d_rj(i_n2,is_fld+2)        &
     &           + coef_fdm_fix_CMB_2(1,2) * d_rj(i_n1,is_fld+2)        &
     &           + coef_fdm_fix_CMB_2(0,2) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_rot  ) =  d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) =  d1t_dr1
        d_rj(inod,is_rot+2) = -d2s_dr2
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_rigid_v_and_w
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_rigid_velo2(is_fld)
!
      integer(kind = kint), intent(in) :: is_fld
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, nidx_rj(2)
        inod = j + (nlayer_CMB-1) * nidx_rj(2)
!
        d_rj(inod,is_fld  ) = zero
        d_rj(inod,is_fld+1) = zero
        d_rj(inod,is_fld+2) = vt_CMB_bc(j)*ar_1d_rj(nlayer_CMB,1)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_rigid_velo2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_rigid_rot2(is_fld, is_rot)
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_rot
!
      integer(kind = kint) :: inod, j, i_n1, i_n2
      real(kind = kreal) :: d2s_dr2, d1t_dr1
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d2s_dr2,d1t_dr1)
      do j = 1, nidx_rj(2)
        inod = j + (nlayer_CMB-1) * nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        i_n2 = i_n1 - nidx_rj(2)
!
        d2s_dr2 =  coef_fdm_fix_dr_CMB_2(-1,3) * d_rj(i_n1,is_fld  )    &
     &           + coef_fdm_fix_dr_CMB_2( 0,3) * d_rj(inod,is_fld  )
        d1t_dr1 =  coef_fdm_fix_CMB_2(2,2) * d_rj(i_n2,is_fld+2)        &
     &           + coef_fdm_fix_CMB_2(1,2) * d_rj(i_n1,is_fld+2)        &
     &           + coef_fdm_fix_CMB_2(0,2) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_rot  ) = d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) = d1t_dr1
        d_rj(inod,is_rot+2) = - ( d2s_dr2 - g_sph_rj(j,3)               &
     &                  * ar_1d_rj(nlayer_CMB,2)*d_rj(inod,is_fld  ) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_rigid_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_rigid_diffuse2(coef_d,                 &
     &          is_fld, is_diffuse)
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_diffuse
      real(kind = kreal), intent(in) :: coef_d
!
      integer(kind = kint) :: inod, j, i_n1, i_n2
      real(kind = kreal) :: d2s_dr2, d2t_dr2
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d2s_dr2,d2t_dr2)
      do j = 1, nidx_rj(2)
        inod = j + (nlayer_CMB-1) * nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        i_n2 = i_n1 - nidx_rj(2)
!
        d2s_dr2 =  coef_fdm_fix_dr_CMB_2(-1,3) * d_rj(i_n1,is_fld  )    &
     &           + coef_fdm_fix_dr_CMB_2( 0,3) * d_rj(inod,is_fld  )
        d2t_dr2 =  coef_fdm_fix_CMB_2(2,3) * d_rj(i_n2,is_fld+2)        &
     &           + coef_fdm_fix_CMB_2(1,3) * d_rj(i_n1,is_fld+2)        &
     &           + coef_fdm_fix_CMB_2(0,3) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_diffuse  ) =  coef_d * (d2s_dr2                    &
     &    - g_sph_rj(j,3)*ar_1d_rj(nlayer_CMB,2)*d_rj(inod,is_fld  ) )
        d_rj(inod,is_diffuse+2) =  coef_d * (d2t_dr2                    &
     &    - g_sph_rj(j,3)*ar_1d_rj(nlayer_CMB,2)*d_rj(inod,is_fld+2) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_rigid_diffuse2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_cmb_rgd_w_diffuse2(coef_d,                 &
     &          is_fld, is_diffuse)
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_diffuse
      real(kind = kreal), intent(in) :: coef_d
!
      integer(kind = kint) :: inod, j, i_n1, i_n2
      real(kind = kreal) :: d2s_dr2, d2t_dr2
!
!
!$omp parallel do private(inod,i_n1,i_n2,j,d2s_dr2,d2t_dr2)
      do j = 1, nidx_rj(2)
        inod = j + (nlayer_CMB-1) * nidx_rj(2)
        i_n1 = inod - nidx_rj(2)
        i_n2 = i_n1 - nidx_rj(2)
!
        d2s_dr2 =  coef_fdm_fix_CMB_2(2,3) * d_rj(i_n2,is_fld  )        &
     &           + coef_fdm_fix_CMB_2(1,3) * d_rj(i_n1,is_fld  )        &
     &           + coef_fdm_fix_CMB_2(0,3) * d_rj(inod,is_fld  )
        d2t_dr2 =  coef_fdm_fix_CMB_2(2,3) * d_rj(i_n2,is_fld+2)        &
     &           + coef_fdm_fix_CMB_2(1,3) * d_rj(i_n1,is_fld+2)        &
     &           + coef_fdm_fix_CMB_2(0,3) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_diffuse  ) =  coef_d * (d2s_dr2                    &
     &                           - g_sph_rj(j,3)*ar_1d_rj(nlayer_CMB,2) &
     &                            * d_rj(inod,is_fld  ) )
        d_rj(inod,is_diffuse+2) =  coef_d * (d2t_dr2                    &
     &                           - g_sph_rj(j,3)*ar_1d_rj(nlayer_CMB,2) &
     &                            * d_rj(inod,is_fld+2) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_cmb_rgd_w_diffuse2
!
! -----------------------------------------------------------------------
!
      end module set_sph_exp_rigid_CMB
