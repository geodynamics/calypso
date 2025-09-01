!>@file   select_sph_r_mat_magne_BC.f90
!!@brief  module select_sph_r_mat_magne_BC
!!
!!@date  Programmed by H.Matsui on Apr., 2009
!
!>@brief Construct matrix for magnetic field at boundaries
!!
!!@verbatim
!!      subroutine sel_sph_r_mat_pol_magnetic_ICB                       &
!!     &         (sph_rj, sph_bc_B, bcs_B, fdm2_center,                 &
!!     &          g_sph_rj, coef_dbt, band_bp_evo)
!!      subroutine sel_sph_r_mat_tor_magnetic_ICB                       &
!!     &         (sph_rj, sph_bc_B, bcs_B, fdm2_center,                 &
!!     &          g_sph_rj, coef_dbt, band_bt_evo)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc_B
!!        type(sph_vector_boundary_data), intent(in) :: bcs_B
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        real(kind=kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!!        real(kind = kreal), intent(in) :: coef_dbt
!!        type(band_matrices_type), intent(inout) :: band_bt_evo
!!
!!      subroutine sel_sph_r_mat_pol_magnetic_CMB(sph_rj, sph_bc_B,     &
!!     &          g_sph_rj, coef_dbt, band_bp_evo)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc_B
!!        real(kind = kreal), intent(in) :: coef_dbt
!!        real(kind=kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!!        type(band_matrices_type), intent(inout) :: band_bp_evo
!!@endverbatim
!
      module select_sph_r_mat_magne_BC
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_ludcmp_3band
!
      use t_physical_property
      use t_spheric_rj_data
      use t_sph_matrices
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
      use t_coef_fdm2_centre
!
      use set_radial_mat_sph
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sel_sph_r_mat_pol_magnetic_ICB                         &
     &         (sph_rj, sph_bc_B, bcs_B, fdm2_center,                   &
     &          g_sph_rj, coef_dbt, band_bp_evo)
!
      use set_sph_scalar_matrix_ICB
      use center_sph_matrices
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(sph_vector_boundary_data), intent(in) :: bcs_B
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: coef_dbt
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(band_matrices_type), intent(inout) :: band_bp_evo
!
!
      if(     (sph_bc_B%iflag_icb .eq. iflag_sph_fill_center)           &
     &   .or. (sph_bc_B%iflag_icb .eq. iflag_sph_filter_center)) then
        call add_vector_poisson_mat_center                              &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_B%r_ICB, fdm2_center%dmat_fix_fld,                   &
     &      coef_dbt, band_bp_evo%mat)
        if(sph_bc_B%iflag_icb .eq. iflag_sph_filter_center) then
          call set_unit_mat3_filter_to_center                           &
     &       (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                     &
     &        bcs_B%ICB_Vspec%Vp_BC, band_bp_evo%mat)
        end if
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        call add_fix_flux_icb_poisson_mat                               &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_B%kr_in, sph_bc_B%r_ICB, sph_bc_B%fdm2_fix_dr_ICB,   &
     &      coef_dbt, band_bp_evo%mat)
      else if(sph_bc_B%iflag_icb .eq. iflag_evolve_field) then
        call set_fix_fld_icb_poisson_mat                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc_B%kr_in, band_bp_evo%mat)
!
!      else if(sph_bc_B%iflag_icb .eq. iflag_sph_insulator) then
      else
        call set_ins_magne_icb_rmat_sph                                 &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_B%kr_in, sph_bc_B%r_ICB, sph_bc_B%fdm2_fix_dr_ICB,   &
     &      coef_dbt, band_bp_evo%mat)
      end if
!
      end subroutine sel_sph_r_mat_pol_magnetic_ICB
!
! -----------------------------------------------------------------------
!
      subroutine sel_sph_r_mat_tor_magnetic_ICB                         &
     &         (sph_rj, sph_bc_B, bcs_B, fdm2_center,                   &
     &          g_sph_rj, coef_dbt, band_bt_evo)
!
      use set_sph_scalar_matrix_ICB
      use center_sph_matrices
!
      real(kind = kreal), intent(in) :: coef_dbt
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(sph_vector_boundary_data), intent(in) :: bcs_B
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(band_matrices_type), intent(inout) :: band_bt_evo
!
!
      if(     (sph_bc_B%iflag_icb .eq. iflag_sph_fill_center)           &
     &   .or. (sph_bc_B%iflag_icb .eq. iflag_sph_filter_center)) then
        call add_vector_poisson_mat_center                              &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_B%r_ICB, fdm2_center%dmat_fix_fld,                   &
     &      coef_dbt, band_bt_evo%mat)
        if(sph_bc_B%iflag_icb .eq. iflag_sph_filter_center) then
          call set_unit_mat3_filter_to_center                           &
     &       (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                     &
     &        bcs_B%ICB_Vspec%Vp_BC, band_bt_evo%mat)
        end if
!
!      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
!      else if(sph_bc_B%iflag_icb .eq. iflag_evolve_field) then
!      else if(sph_bc_B%iflag_icb .eq. iflag_sph_insulator) then
      else
        call set_fix_fld_icb_poisson_mat                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc_B%kr_in, band_bt_evo%mat)
      end if
!
      end subroutine sel_sph_r_mat_tor_magnetic_ICB
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_sph_r_mat_pol_magnetic_CMB(sph_rj, sph_bc_B,       &
     &          g_sph_rj, coef_dbt, band_bp_evo)
!
      use set_sph_scalar_matrix_CMB
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_B
      real(kind = kreal), intent(in) :: coef_dbt
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(band_matrices_type), intent(inout) :: band_bp_evo
!
!
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call add_fix_flux_cmb_poisson_mat                               &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_B%kr_out, sph_bc_B%r_CMB, sph_bc_B%fdm2_fix_dr_CMB,  &
     &      coef_dbt, band_bp_evo%mat)
      else if(sph_bc_B%iflag_cmb .eq. iflag_evolve_field) then
        call set_fix_fld_cmb_poisson_mat                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc_B%kr_out, band_bp_evo%mat)
!      else if(sph_bc_B%iflag_cmb .eq. iflag_sph_insulator) then
      else
        call set_ins_magne_cmb_rmat_sph                                 &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_B%kr_out, sph_bc_B%r_CMB, sph_bc_B%fdm2_fix_dr_CMB,  &
     &      coef_dbt, band_bp_evo%mat)
      end if
!
      end subroutine sel_sph_r_mat_pol_magnetic_CMB
!
! -----------------------------------------------------------------------
!
      end module select_sph_r_mat_magne_BC
