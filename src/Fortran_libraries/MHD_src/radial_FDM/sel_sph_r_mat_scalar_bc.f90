!>@file   sel_sph_r_mat_scalar_bc.f90
!!@brief  module sel_sph_r_mat_scalar_bc
!!
!!@date  Programmed by H.Matsui on Apr., 2009
!
!>@brief Construct matrix for time evolution of scalar fields
!!
!!@verbatim
!!      subroutine sel_sph_radial_mat_press_bc(sph_rj, sph_bc_U,        &
!!     &          fdm2_center, g_sph_rj, r_coef, band_p_poisson)
!!      subroutine sel_sph_radial_mat_scalar_ICB                        &
!!     &         (flag_val_diffuse, sph_rj, sph_bc, bcs_S, fdm2_center, &
!!     &          g_sph_rj, coef, k_ratio, dk_dr, band_s_evo)
!!      subroutine sel_sph_radial_mat_scalar_CMB                        &
!!     &         (flag_val_diffuse, sph_rj, sph_bc, bcs_S, fdm2_center, &
!!     &          g_sph_rj, coef, k_ratio, band_s_evo)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_scalar_boundary_data) :: bcs_S
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        real(kind = kreal), intent(in):: g_sph_rj(sph_rj%nidx_rj(2),13)
!!        real(kind = kreal), intent(in) :: r_coef
!!        real(kind = kreal), intent(in) :: k_ratio
!!        real(kind = kreal), intent(in) :: dk_dr
!!        type(band_matrices_type), intent(inout) :: band_p_poisson
!!        type(band_matrices_type), intent(inout) :: band_s_evo
!!@endverbatim
!
      module sel_sph_r_mat_scalar_bc
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_sph_matrices
      use t_sph_matrix
      use t_fdm_coefs
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
      subroutine sel_sph_radial_mat_press_bc(sph_rj, sph_bc_U,          &
     &          fdm2_center, g_sph_rj, r_coef, band_p_poisson)
!
      use cal_inner_core_rotation
      use center_sph_matrices
      use set_sph_scalar_matrix_ICB
      use set_sph_scalar_matrix_CMB
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: r_coef
!
      type(band_matrices_type), intent(inout) :: band_p_poisson
!
!
!   Boundary condition for ICB
      if(     (sph_bc_U%iflag_icb .eq. iflag_sph_fill_center)           &
     &   .or. (sph_bc_U%iflag_icb .eq. iflag_sph_filter_center)) then
        call add_scalar_poisson_mat_ctr1                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_U%r_ICB, fdm2_center%dmat_fix_fld,                   &
     &      r_coef, band_p_poisson%mat)
!      else if(sph_bc_U%iflag_icb .eq. iflag_free_sph) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_evolve_field) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_fixed_field) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_non_slip) then
      else
        call add_icb_scalar_poisson_mat                                 &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, sph_bc_U%fdm2_fix_dr_ICB,   &
     &      r_coef, band_p_poisson%mat)
      end if
!
!   Boundary condition for CMB
      call add_cmb_scalar_poisson_mat                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,               &
     &    sph_bc_U%kr_out, sph_bc_U%r_CMB, sph_bc_U%fdm2_fix_dr_CMB,    &
     &    r_coef, band_p_poisson%mat)
!
      end subroutine sel_sph_radial_mat_press_bc
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_sph_radial_mat_scalar_ICB                          &
     &         (flag_val_diffuse, sph_rj, sph_bc, bcs_S, fdm2_center,   &
     &          g_sph_rj, coef, k_ratio, dk_dr, band_s_evo)
!
      use center_sph_matrices
      use set_sph_scalar_matrix_ICB
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_boundary_data) :: bcs_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: k_ratio
      real(kind = kreal), intent(in) :: dk_dr
      logical, intent(in) :: flag_val_diffuse
!
      type(band_matrices_type), intent(inout) :: band_s_evo
!
      real(kind = kreal) :: coef_p
!
!
      if(     (sph_bc%iflag_icb .eq. iflag_sph_fill_center)             &
     &   .or. (sph_bc%iflag_icb .eq. iflag_sph_fix_center)              &
     &   .or. (sph_bc%iflag_icb .eq. iflag_sph_filter_center)) then
        if(flag_val_diffuse) then
          call add_scl_val_diffuse_mat_ctr1                             &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc%r_ICB, fdm2_center%dmat_fix_fld,                     &
     &      coef, k_ratio, dk_dr, band_s_evo%mat)
        else
          call add_scalar_poisson_mat_ctr1                              &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc%r_ICB, fdm2_center%dmat_fix_fld, coef,               &
     &      band_s_evo%mat)
        end if
!
        if(sph_bc%iflag_icb .eq. iflag_sph_filter_center) then
          call set_unit_mat3_filter_to_center                           &
     &       (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                     &
     &        bcs_S%ICB_Sspec%S_BC, band_s_evo%mat)
        end if
      else if((sph_bc%iflag_icb .eq. iflag_fixed_flux)                  &
     &   .or. (sph_bc%iflag_icb .eq. iflag_evolve_flux)) then
        coef_p = coef
        if(flag_val_diffuse) coef_p = coef_p * k_ratio
        call add_fix_flux_icb_poisson_mat                               &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_dr_ICB,         &
     &      coef_p, band_s_evo%mat)
!      else if ((sph_bc%iflag_icb .eq. iflag_fixed_field)               &
!     &    .or. (sph_bc%iflag_icb .eq. iflag_evolve_field)) then
      else
        call set_fix_fld_icb_poisson_mat                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc%kr_in, band_s_evo%mat)
      end if
!
      end subroutine sel_sph_radial_mat_scalar_ICB
!
! -----------------------------------------------------------------------
!
      subroutine sel_sph_radial_mat_scalar_CMB                          &
     &         (flag_val_diffuse, sph_rj, sph_bc, bcs_S, fdm2_center,   &
     &          g_sph_rj, coef, k_ratio, band_s_evo)
!
      use set_sph_scalar_matrix_CMB
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_boundary_data) :: bcs_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: k_ratio
      logical, intent(in) :: flag_val_diffuse
!
      type(band_matrices_type), intent(inout) :: band_s_evo
!
      real(kind = kreal) :: coef_p
!
!
      if(      (sph_bc%iflag_cmb .eq. iflag_fixed_flux)                 &
     &    .or. (sph_bc%iflag_cmb .eq. iflag_evolve_flux)) then
        coef_p = coef
        if(flag_val_diffuse) coef_p = coef_p * k_ratio
        call add_fix_flux_cmb_poisson_mat                               &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_dr_CMB,        &
     &      coef_p, band_s_evo%mat)
!      else if((sph_bc%iflag_cmb .eq. iflag_fixed_field)                &
!     &   .or. (sph_bc%iflag_cmb .eq. iflag_evolve_field)) then
      else
        call set_fix_fld_cmb_poisson_mat                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc%kr_out, band_s_evo%mat)
      end if
!
      end subroutine sel_sph_radial_mat_scalar_CMB
!
! -----------------------------------------------------------------------
!
      end module sel_sph_r_mat_scalar_bc
