!> @file cal_sph_bc_fdm_matrix.f90
!!      module cal_sph_bc_fdm_matrix
!!
!! @author H. Matsui
!! @date Written on May, 2003
!
!!> @brief calculate FDM matrices for boundaries
!!
!!@verbatim
!!      subroutine s_cal_sph_bc_fdm_matrices
!!@endverbatim
!
      module cal_sph_bc_fdm_matrix
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_sph_bc_fdm_matrices
!
      use m_spheric_parameter
      use m_coef_fdm_fixed_ICB
      use m_coef_fdm_fixed_CMB
      use m_coef_fdm_free_ICB
      use m_coef_fdm_free_CMB
      use m_coef_fdm_to_center
      use cal_fdm_coefs_4_boundaries
!
!
      call cal_fdm2_coef_fix_fld_ICB(radius_1d_rj_r(nlayer_ICB),        &
     &    coef_fdm_fix_ICB_2)
      call cal_fdm2_coef_fix_df_ICB(radius_1d_rj_r(nlayer_ICB),         &
     &    coef_fdm_fix_dr_ICB_2)
!
      call cal_fdm2_coef_fix_fld_CMB(radius_1d_rj_r(nlayer_CMB-2),      &
     &    coef_fdm_fix_CMB_2)
      call cal_fdm2_coef_fix_df_CMB(radius_1d_rj_r(nlayer_CMB-1),       &
     &    coef_fdm_fix_dr_CMB_2)
!
!
      call cal_2nd_ICB_free_vp_bc_fdm(radius_1d_rj_r(nlayer_ICB))
      call cal_2nd_ICB_free_vt_bc_fdm(radius_1d_rj_r(nlayer_ICB))
!
      call cal_2nd_CMB_free_vp_bc_fdm(radius_1d_rj_r(nlayer_CMB-1))
      call cal_2nd_CMB_free_vt_bc_fdm(radius_1d_rj_r(nlayer_CMB-1))
!
      call cal_2nd_to_center_fixed_fdm(radius_1d_rj_r(1))
      call cal_2nd_to_center_fix_df_fdm(radius_1d_rj_r(1))
!
      if (iflag_debug .eq. iflag_full_msg) then
        call check_coef_fdm_fix_dr_ICB
        call check_coef_fdm_fix_dr_CMB
        call check_coef_fdm_free_ICB
        call check_coef_fdm_free_CMB
        call check_coef_fdm_fix_dr_2ctr
      end if
!
!      call cal_sph_bc_2nd_ele_fdm_mat
!
      end subroutine s_cal_sph_bc_fdm_matrices
!
! -----------------------------------------------------------------------
!
      end module cal_sph_bc_fdm_matrix
