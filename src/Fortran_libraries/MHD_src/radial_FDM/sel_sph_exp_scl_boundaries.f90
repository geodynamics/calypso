!> @file  sel_sph_exp_scl_boundaries.f90
!!      module sel_sph_exp_scl_boundaries
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate radial delivatives
!!
!!@verbatim
!!      subroutine sel_ICB_sph_scalar_diffusion                         &
!!     &         (sph_rj, sph_bc, ICB_Sspec, fdm2_center,               &
!!     &          g_sph_rj, coef_diffuse, scl_rj, dfs_rj)
!!      subroutine sel_ICB_sph_scalar_val_diffuse                       &
!!     &         (sph_rj, sph_bc, ICB_Sspec, fdm2_center, g_sph_rj,     &
!!     &          coef_diffuse, k_ratio, dk_dr, scl_rj, dfs_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_scalar_BC_coef), intent(in) :: ICB_Sspec
!!        real(kind = kreal), intent(in) :: coef_diffuse
!!        real(kind = kreal), intent(in) :: k_ratio(0:sph_rj%nidx_rj(1))
!!        real(kind = kreal), intent(in) :: dk_dr(0:sph_rj%nidx_rj(1))
!!        real(kind=kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!!        real(kind = kreal), intent(inout) :: scl_rj(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout) :: dfs_rj(sph_rj%nnod_rj)
!!
!!      subroutine sel_CMB_sph_scalar_diffusion                         &
!!     &         (sph_rj, sph_bc, CMB_Sspec, g_sph_rj, coef_diffuse,    &
!!     &          is_fld, is_diffuse, n_point, ntot_phys_rj, d_rj)
!!      subroutine sel_CMB_sph_scalar_val_diffuse                       &
!!     &         (sph_rj, sph_bc, CMB_Sspec, g_sph_rj,                  &
!!     &          coef_diffuse, k_ratio, dk_dr, scl_rj, dfs_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_scalar_BC_coef), intent(in) :: CMB_Sspec
!!        real(kind = kreal), intent(in) :: coef_diffuse
!!        real(kind = kreal), intent(in) :: k_ratio, dk_dr
!!        real(kind=kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!!        real(kind = kreal), intent(inout) :: scl_rj(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout) :: dfs_rj(sph_rj%nnod_rj)
!!@endverbatim
!!
!!@param sph_bc  Structure for basic boundary condition parameters
!!@param coef_diffuse   Diffusion coefficient
!!
!!@param is_fld       Spherical hermonics data address for input vector
!!@param is_diffuse   Input spectr diffusiton term address
!
      module sel_sph_exp_scl_boundaries
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
      use t_coef_fdm2_centre
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sel_ICB_sph_scalar_diffusion                           &
     &         (sph_rj, sph_bc, ICB_Sspec, fdm2_center,                 &
     &          g_sph_rj, coef_diffuse, scl_rj, dfs_rj)
!
      use sph_exp_fix_scl_diffuse_ICB
      use sph_exp_fix_flx_diffuse_ICB
      use sph_filled_center_diffuse2
      use sph_fixed_center_diffuse2
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_BC_coef), intent(in) :: ICB_Sspec
!
      real(kind = kreal), intent(in) :: coef_diffuse
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout) :: scl_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout) :: dfs_rj(sph_rj%nnod_rj)
!
!
      if     (sph_bc%iflag_icb .eq. iflag_sph_fill_center) then
        call sph_filled_ctr_diffuse_ctr2                                &
     &     (sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      sph_rj%nnod_rj, sph_rj%nidx_rj(2), g_sph_rj, sph_bc%r_ICB,  &
     &      fdm2_center%dmat_fix_fld, fdm2_center%dmat_fix_dr,          &
     &      coef_diffuse, scl_rj, dfs_rj)
      else if(sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        call sph_fixed_ctr_diffuse_ctr1                                 &
     &     (sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      sph_rj%nnod_rj, sph_rj%nidx_rj(2), g_sph_rj,                &
     &      sph_bc%r_ICB, fdm2_center%dmat_fix_fld, ICB_Sspec%S_BC,     &
     &      coef_diffuse, scl_rj, dfs_rj)
      else if(sph_bc%iflag_icb .eq. iflag_fixed_flux                    &
     &    .or. sph_bc%iflag_icb .eq. iflag_evolve_flux) then
        call sph_in_fix_flux_scl_diffuse2                               &
     &     (sph_rj%nnod_rj, sph_rj%nidx_rj(2), g_sph_rj,                &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_dr_ICB,         &
     &      ICB_Sspec%S_BC, coef_diffuse, scl_rj, dfs_rj)
!      else if(sph_bc%iflag_icb .eq. iflag_fixed_field                  &
!     &   .or. sph_bc%iflag_icb .eq. iflag_evolve_field) then
      else
        call sph_in_fix_scalar_diffuse2                                 &
     &     (sph_rj%nnod_rj, sph_rj%nidx_rj(2), g_sph_rj,                &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,        &
     &      ICB_Sspec%S_BC, coef_diffuse, scl_rj, dfs_rj)
      end if
!
      end subroutine sel_ICB_sph_scalar_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine sel_ICB_sph_scalar_val_diffuse                         &
     &         (sph_rj, sph_bc, ICB_Sspec, fdm2_center, g_sph_rj,       &
     &          coef_diffuse, k_ratio, dk_dr, scl_rj, dfs_rj)
!
      use sph_exp_fix_scl_diffuse_ICB
      use sph_exp_fix_flx_diffuse_ICB
      use sph_filled_center_diffuse2
      use sph_fixed_center_diffuse2
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_BC_coef), intent(in) :: ICB_Sspec
!
      real(kind = kreal), intent(in) :: coef_diffuse
      real(kind = kreal), intent(in) :: k_ratio(0:sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: dk_dr(0:sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout) :: scl_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout) :: dfs_rj(sph_rj%nnod_rj)
!
!
      if     (sph_bc%iflag_icb .eq. iflag_sph_fill_center) then
        call sph_filled_ctr_val_diffuse_ctr2                            &
     &     (sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      sph_rj%nnod_rj, sph_rj%nidx_rj(2), g_sph_rj, sph_bc%r_ICB,  &
     &      fdm2_center%dmat_fix_fld, fdm2_center%dmat_fix_dr,          &
     &      coef_diffuse, k_ratio(0), dk_dr(0), scl_rj, dfs_rj)
      else if(sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        call sph_fixed_ctr_val_diffuse_ctr1                             &
     &     (sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      sph_rj%nnod_rj, sph_rj%nidx_rj(2), g_sph_rj,                &
     &      sph_bc%r_ICB, fdm2_center%dmat_fix_fld, ICB_Sspec%S_BC,     &
     &      coef_diffuse, k_ratio(1), dk_dr(1), scl_rj, dfs_rj)
      else if(sph_bc%iflag_icb .eq. iflag_fixed_flux                    &
     &    .or. sph_bc%iflag_icb .eq. iflag_evolve_flux) then
        call sph_in_fix_flux_val_diffuse2                               &
     &     (sph_rj%nnod_rj, sph_rj%nidx_rj(2), g_sph_rj, sph_bc%kr_in,  &
     &      sph_bc%r_ICB, sph_bc%fdm2_fix_dr_ICB, ICB_Sspec%S_BC,       &
     &      coef_diffuse, k_ratio(sph_bc%kr_in), dk_dr(sph_bc%kr_in),   &
     &      scl_rj, dfs_rj)
!      else if(sph_bc%iflag_icb .eq. iflag_fixed_field                  &
!     &   .or. sph_bc%iflag_icb .eq. iflag_evolve_field) then
      else
        call sph_in_fix_scl_val_diffuse2                                &
     &     (sph_rj%nnod_rj, sph_rj%nidx_rj(2), g_sph_rj, sph_bc%kr_in,  &
     &      sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB, ICB_Sspec%S_BC,      &
     &      coef_diffuse, k_ratio(sph_bc%kr_in), dk_dr(sph_bc%kr_in),   &
     &      scl_rj, dfs_rj)
      end if
!
      end subroutine sel_ICB_sph_scalar_val_diffuse
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_sph_scalar_diffusion                           &
     &         (sph_rj, sph_bc, CMB_Sspec, g_sph_rj, coef_diffuse,      &
     &          scl_rj, dfs_rj)
!
      use sph_exp_fix_scl_diffuse_CMB
      use sph_exp_fix_flx_diffuse_CMB
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_BC_coef), intent(in) :: CMB_Sspec
!
      real(kind = kreal), intent(in) :: coef_diffuse
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout) :: scl_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout) :: dfs_rj(sph_rj%nnod_rj)
!
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux                        &
     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
        call sph_out_fix_flux_scl_diffuse2                              &
     &     (sph_rj%nnod_rj, sph_rj%nidx_rj(2), g_sph_rj,                &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_dr_CMB,        &
     &      CMB_Sspec%S_BC, coef_diffuse, scl_rj, dfs_rj)
!      else if(sph_bc%iflag_cmb .eq. iflag_fixed_field                  &
!     &   .or. sph_bc%iflag_cmb .eq. iflag_evolve_field) then
      else
        call sph_out_fix_scalar_diffuse2                                &
     &     (sph_rj%nnod_rj, sph_rj%nidx_rj(2), g_sph_rj,                &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      CMB_Sspec%S_BC, coef_diffuse, scl_rj, dfs_rj)
      end if
!
      end subroutine sel_CMB_sph_scalar_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_sph_scalar_val_diffuse                         &
     &         (sph_rj, sph_bc, CMB_Sspec, g_sph_rj,                    &
     &          coef_diffuse, k_ratio, dk_dr, scl_rj, dfs_rj)
!
      use sph_exp_fix_scl_diffuse_CMB
      use sph_exp_fix_flx_diffuse_CMB
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_BC_coef), intent(in) :: CMB_Sspec
!
      real(kind = kreal), intent(in) :: coef_diffuse
      real(kind = kreal), intent(in) :: k_ratio, dk_dr
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout) :: scl_rj(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout) :: dfs_rj(sph_rj%nnod_rj)
!
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux                        &
     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
        call sph_out_fix_flux_val_diffuse2                              &
     &     (sph_rj%nnod_rj, sph_rj%nidx_rj(2), g_sph_rj,                &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_dr_CMB,        &
     &      CMB_Sspec%S_BC, coef_diffuse, k_ratio, dk_dr,               &
     &      scl_rj, dfs_rj)
!      else if(sph_bc%iflag_cmb .eq. iflag_fixed_field                  &
!     &   .or. sph_bc%iflag_cmb .eq. iflag_evolve_field) then
      else
        call sph_out_fix_scl_val_diffuse2                               &
     &     (sph_rj%nnod_rj, sph_rj%nidx_rj(2), g_sph_rj,                &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      CMB_Sspec%S_BC, coef_diffuse, k_ratio, dk_dr,               &
     &      scl_rj, dfs_rj)
      end if
!
      end subroutine sel_CMB_sph_scalar_val_diffuse
!
! -----------------------------------------------------------------------
!
      end module sel_sph_exp_scl_boundaries
