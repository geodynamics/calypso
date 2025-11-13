!>@file   select_sph_r_mat_vort_BC.f90
!!@brief  module select_sph_r_mat_vort_BC
!!
!!@date  Programmed by H.Matsui on Apr., 2009
!
!>@brief Construct matrix for time evolution of vector fields
!!
!!@verbatim
!!      subroutine sel_sph_r_mat_vort_2step_ICB                         &
!!     &         (sph_rj, sph_bc_U, bcs_U, bc_fdms_U, fdm2_center,      &
!!     &          g_sph_rj, coef_dvt, band_vs_poisson, band_wt_evo)
!!      subroutine sel_sph_r_mat_tor_flow_ICB                           &
!!     &         (sph_rj, sph_bc_U, bcs_U, bc_fdms_U, fdm2_center,      &
!!     &          g_sph_rj, coef_dvt, band_vt_evo)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_vector_boundary_data), intent(in) :: bcs_U
!!        type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        real(kind=kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!!        real(kind = kreal), intent(in) :: coef_dvt
!!        type(band_matrices_type), intent(inout) :: band_wt_evo
!!        type(band_matrices_type), intent(inout) :: band_vs_poisson
!!        type(band_matrices_type), intent(inout) :: band_vt_evo
!!
!!      subroutine sel_sph_r_mat_vort_2step_CMB                         &
!!     &         (sph_rj, sph_bc_U, bc_fdms_U, g_sph_rj, coef_dvt,      &
!!     &          band_vs_poisson, band_wt_evo)
!!      subroutine sel_sph_r_mat_tor_flow_CMB(sph_rj, sph_bc_U,         &
!!     &          bc_fdms_U, g_sph_rj, coef_dvt, band_vt_evo)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
!!        real(kind=kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!!        real(kind = kreal), intent(in) :: coef_dvt
!!        type(band_matrices_type), intent(inout) :: band_vs_poisson
!!        type(band_matrices_type), intent(inout) :: band_wt_evo
!!        type(band_matrices_type), intent(inout) :: band_vt_evo
!!@endverbatim
!
      module select_sph_r_mat_vort_BC
!
      use m_precision
      use calypso_mpi
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
      use t_coef_sph_velocity_BCs
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
      subroutine sel_sph_r_mat_vort_2step_ICB                           &
     &         (sph_rj, sph_bc_U, bcs_U, bc_fdms_U, fdm2_center,        &
     &          g_sph_rj, coef_dvt, band_vs_poisson, band_wt_evo)
!
      use set_sph_scalar_matrix_ICB
      use center_sph_matrices
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_vector_boundary_data), intent(in) :: bcs_U
      type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_dvt
!
      type(band_matrices_type), intent(inout) :: band_wt_evo
      type(band_matrices_type), intent(inout) :: band_vs_poisson
!
!
      if(     (sph_bc_U%iflag_icb .eq. iflag_sph_fill_center)           &
     &   .or. (sph_bc_U%iflag_icb .eq. iflag_sph_filter_center)) then
        call add_vector_poisson_mat_center                              &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_U%r_ICB, fdm2_center%dmat_fix_fld,                   &
     &      coef_dvt, band_wt_evo%mat)
        call add_vector_poisson_mat_center                              &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_U%r_ICB, fdm2_center%dmat_fix_fld,                   &
     &      one, band_vs_poisson%mat)
!
        if(sph_bc_U%iflag_icb .eq. iflag_sph_filter_center) then
!          call set_unit_mat3_filter_to_center                          &
!     &       (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                    &
!     &        bcs_U%ICB_Vspec%Vp_BC, band_wt_evo%mat)
          call set_unit_mat3_filter_to_center                           &
     &       (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                     &
     &        bcs_U%ICB_Vspec%Vp_BC, band_vs_poisson%mat)
        end if
      else
        call add_fix_flux_icb_poisson_mat                               &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, sph_bc_U%fdm2_fix_dr_ICB,   &
     &      coef_dvt, band_wt_evo%mat)
!
        if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
          call add_fix_flux_icb_poisson_mat                             &
     &       (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,           &
     &        sph_bc_U%kr_in, sph_bc_U%r_ICB,                           &
     &        bc_fdms_U%fdm2_free_ICB%dmat_vp,                          &
     &        one, band_vs_poisson%mat)
!      else if(sph_bc_U%iflag_icb .eq. iflag_evolve_field) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_fixed_field) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_fixed_velo) then
        else
          call add_fix_flux_icb_poisson_mat                             &
     &       (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,           &
     &        sph_bc_U%kr_in, sph_bc_U%r_ICB, sph_bc_U%fdm2_fix_dr_ICB, &
     &        one, band_vs_poisson%mat)
        end if
      end if
!
      end subroutine sel_sph_r_mat_vort_2step_ICB
!
! -----------------------------------------------------------------------
!
      subroutine sel_sph_r_mat_tor_flow_ICB                             &
     &         (sph_rj,sph_bc_U,  bcs_U, bc_fdms_U, fdm2_center,        &
     &          g_sph_rj, coef_dvt, band_vt_evo)
!
      use set_sph_scalar_matrix_ICB
      use center_sph_matrices
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_vector_boundary_data), intent(in) :: bcs_U
      type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_dvt
!
      type(band_matrices_type), intent(inout) :: band_vt_evo
!
!
!
      if(     (sph_bc_U%iflag_icb .eq. iflag_sph_fill_center)           &
     &   .or. (sph_bc_U%iflag_icb .eq. iflag_sph_filter_center)) then
        call add_vector_poisson_mat_center                              &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_U%r_ICB, fdm2_center%dmat_fix_fld,                   &
     &      coef_dvt, band_vt_evo%mat)
!
        if(sph_bc_U%iflag_icb .eq. iflag_sph_filter_center) then
          call set_unit_mat3_filter_to_center                           &
     &       (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                     &
     &        bcs_U%ICB_Vspec%Vp_BC, band_vt_evo%mat)
        end if
      else
        if(sph_bc_U%iflag_icb .eq. iflag_free_slip) then
          call add_fix_flux_icb_poisson_mat                             &
     &       (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,           &
     &        sph_bc_U%kr_in, sph_bc_U%r_ICB,                           &
     &        bc_fdms_U%fdm2_free_ICB%dmat_vt,                          &
     &        coef_dvt, band_vt_evo%mat)
!      else if(sph_bc_U%iflag_icb .eq. iflag_evolve_field) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_fixed_field) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_fixed_velo) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        else
          call set_fix_fld_icb_poisson_mat                              &
     &       (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                     &
     &        sph_bc_U%kr_in, band_vt_evo%mat)
        end if
      end if
!
      end subroutine sel_sph_r_mat_tor_flow_ICB
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_sph_r_mat_vort_2step_CMB                           &
     &         (sph_rj, sph_bc_U, bc_fdms_U, g_sph_rj, coef_dvt,        &
     &          band_vs_poisson, band_wt_evo)
!
      use set_sph_scalar_matrix_CMB
      use center_sph_matrices
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_dvt
!
      type(band_matrices_type), intent(inout) :: band_vs_poisson
      type(band_matrices_type), intent(inout) :: band_wt_evo
!
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call add_fix_flux_cmb_poisson_mat                               &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB,                            &
     &      bc_fdms_U%fdm2_free_CMB%dmat_vp,                            &
     &      one, band_vs_poisson%mat)
!      else if(sph_bc_U%iflag_cmb .eq. iflag_evolve_field) then
!      else if(sph_bc_U%iflag_cmb .eq. iflag_fixed_field) then
!      else if(sph_bc_U%iflag_cmb .eq. iflag_fixed_velo) then
      else
        call add_fix_flux_cmb_poisson_mat                               &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB, sph_bc_U%fdm2_fix_dr_CMB,  &
     &      one, band_vs_poisson%mat)
      end if
!
      call add_fix_flux_cmb_poisson_mat                                 &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,               &
     &    sph_bc_U%kr_out, sph_bc_U%r_CMB, sph_bc_U%fdm2_fix_dr_CMB,    &
     &    coef_dvt, band_wt_evo%mat)
!
      end subroutine sel_sph_r_mat_vort_2step_CMB
!
! -----------------------------------------------------------------------
!
      subroutine sel_sph_r_mat_tor_flow_CMB(sph_rj, sph_bc_U,           &
     &          bc_fdms_U, g_sph_rj, coef_dvt, band_vt_evo)
!
      use set_sph_scalar_matrix_CMB
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_dvt
!
      type(band_matrices_type), intent(inout) :: band_vt_evo
!
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call add_fix_flux_cmb_poisson_mat                               &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_U%kr_out, sph_bc_U%r_CMB,                            &
     &      bc_fdms_U%fdm2_free_CMB%dmat_vt,                            &
     &      coef_dvt, band_vt_evo%mat)
!      else if(sph_bc_U%iflag_cmb .eq. iflag_evolve_field) then
!      else if(sph_bc_U%iflag_cmb .eq. iflag_fixed_field) then
!      else if(sph_bc_U%iflag_cmb .eq. iflag_fixed_velo) then
      else
        call set_fix_fld_cmb_poisson_mat                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc_U%kr_out, band_vt_evo%mat)
      end if
!
      end subroutine sel_sph_r_mat_tor_flow_CMB
!
! -----------------------------------------------------------------------
!
      end module select_sph_r_mat_vort_BC
